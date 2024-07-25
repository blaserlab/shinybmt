#' @import ggplot2
#' @import tidycmprsk
#' @import tidyr
#' @import survminer
#' @import survival
#' @import gtsummary
#' @import gt
#' @import ggsurvfit
#' @import ggsci
#' @import readxl
#' @import shinyWidgets
#' @import shiny
#' @import kableExtra
#' @export


shinyBMT <- function(data_dir, shiny_host = NULL, shiny_port = NULL) {
  options(shiny.host = shiny_host)
  options(shiny.port = shiny_port)
  options(shiny.launch.browser = FALSE)
  
  # get the data dictionary
  dict = get_dict(dir = data_dir)
  
  
  # manually select variables from dict to be incorporated in baseline selection
  # inputID1 - those are set
  # inputID2 - those are dynamic and can be searched and added
  inputId1 = c(
    'dx',
    'tp_prep_class',
    'tp_donor1_type'
  )
  
  inputId2 = c(
    'tp_gvhd_prophy_csa_yn',
    'tp_gvhd_prophy_fk506_yn',
    'tp_gvhd_prophy_mmf_yn',
    'tp_gvhd_prophy_mtx_yn',
    'tp_gvhd_prophy_csa_yn'
  )

  input_choices1 = select_input_choices(inputId1, dict)
  input_choices2 = select_input_choices(inputId2, dict)
  
  # manually select variables to be included in the baseline table
  tbl_variable_name = c('Age', 'Sex', 'Race', 'Diagnosis', 'Prep type', 'Donor type')
  # generate format list(new_Age ~ 'Age',...) for gtsummary
  tbl_variable_list = setNames(tbl_variable_name, map_variable_name(tbl_variable_name))
  
  # A selection of K-M functions
  surv_selection = c('Please select...' = '', 'OS', 'RFS', 'GRFS', 'NRM', 'NRM_100')
  cum_selection = c('Please select...' = '', 'ANC engraftment', 'Plt engraftment', 
                    'G2-4 aGvHD', 'G3-4 aGvHD')
  # A selection of Cox-regression outcomes
  cox_selection = c('Please select...' = '', 'OS', 'RFS', 'GRFS','G2-4 aGvHD', 'G3-4 aGvHD', 'NRM', 'NRM_100')
  # A selection of Cox-regression covariates
  cox_covariates_names = c('Group', 'Age (>=65)', 'Sex', 'Race', 'Diagnosis', 
                           'Prep type', 'Donor type', 'Disease status', 'HCT-CI (>=3)')
  
  
  ui <- fluidPage(
    tabsetPanel(
      tabPanel("Baseline Selection",
               fluidRow(
                 column(3,
                        tags$h3("Group 1"),
                        textInput("g1_name", "Name this group:", value='Group 1'),
                        sliderInput("age_range1", "Select Age Range:", 
                                    min = 0, max = 120, value = c(18, 120)),
                        lapply(1:nrow(input_choices1), function(i) {
                          selectInput(paste0(input_choices1$inputId[i], "1"),
                                      # naming is dx1, dx2, tp1, tp2, etc, for group1 and 2
                                      sprintf("%s:", input_choices1$variable_display[i]),
                                      # choices from original dataset + All + select...
                                      choices = c('Select...'='', 'All'='All',
                                                  input_choices1$choices[[i]]))
                        }),
                        uiOutput("dynamic_inputs1"),  # placeholder for dynamically adding rows
                        actionButton("add_row1", "Add New Row")
                 ),
                 column(3,
                        tags$h3("Group 2"),
                        textInput("g2_name", "Name this group:", value='Group 2'),
                        sliderInput("age_range2", "Select Age Range:", 
                                    min = 0, max = 120, value = c(18, 120)),
                        lapply(1:nrow(input_choices1), function(i) {
                          selectInput(paste0(input_choices1$inputId[i], "2"),
                                      sprintf("%s:", input_choices1$variable_display[i]),
                                      choices = c('Select...'='', 'All'='All',
                                                  input_choices1$choices[[i]]))
                        }),
                        uiOutput("dynamic_inputs2"),
                        actionButton("add_row2", "Add New Row")
                 ),
                 div(style = "text-align: center; margin-top: 20px;",
                     actionButton("gen_tbl", "Generate Baseline Table", class='btn-primary')),
                 column(6, gt_output("baseline_chrcs"))
               )),
      tabPanel("Time-to-Event Analysis",
               sidebarPanel(
                 selectInput("curve_type_surv", "Survival analysis:", 
                             choices = surv_selection,
                             selected = ''),
                 selectInput("curve_type_cum", "Cumulative incidence analysis:", 
                             choices = cum_selection,
                             selected = '')
               ),
               mainPanel(plotOutput("KM_curves"))
      ),
      tabPanel("Cox Regression",
               sidebarPanel(
                 selectInput("cox_outcome_selection", "Time-to-Event Outcome:", 
                             choices = cox_selection,
                             selected = ''),
                 pickerInput("cox_covariates", "Select Covariates:", 
                             choices = cox_covariates_names,
                             options = list(
                               'actions-box' = TRUE, 
                               size = 10,
                               'selected-text-format' = "count > 3"
                             ), 
                             multiple = TRUE),
                 radioButtons("regression_type", "Regression Type:",
                              choices = c("Univariate" = "univariate",
                                          "Multivariable" = "multivariable"),
                              selected = "multivariable"),
                 actionButton("run_cox", "Run Cox Regression", class='btn-primary')
               ),
               column(6, gt_output("cox_reg_results"))
      )
      
    )
  )
  
  server <- function(input, output, session) {
    
    ########## for baseline-analysis tab ##########
    # synchronize selections between two groups
    # age slider synchronization
    observeEvent(input$age_range1, {
      updateSliderInput(session, "age_range2", 
                        value = input$age_range1)
    }, ignoreInit = TRUE)
    
    # selection input synchronization for the set inputs
    for (i in 1:nrow(input_choices1)) {
      local({
        idx = i
        inputId_g1 = paste0(input_choices1$inputId[idx], "1")
        inputId_g2 = paste0(input_choices1$inputId[idx], "2")
        observeEvent(input[[inputId_g1]], {
          updateSelectInput(session, inputId_g2, selected = input[[inputId_g1]])
        }, ignoreInit = TRUE)
      })
    }
    
    # Reactive values to store the number of rows for each group
    row_count1 = reactiveVal(1)
    row_count2 = reactiveVal(1)
    
    # Reactive lists to store input values for each group
    g1_inputs = reactiveValues()
    g2_inputs = reactiveValues()
    
    # Create the 1st row for Group 1 and 2
    output$dynamic_inputs1 = renderUI({
      create_row(1, "g1", input_choices2)
    })
    output$dynamic_inputs2 = renderUI({
      create_row(1, "g2", input_choices2)
    })
    
    # "Add row" button logistics for Group 1
    observeEvent(input$add_row1, {
      save_inputs("row_count1", row_count1, row_count2, g1_inputs, g2_inputs, input) 
      # save contents of previous rows
      new_row_count = row_count1() + 1
      row_count1(new_row_count)
      row_count2(max(row_count2(), new_row_count))  # Ensure Group 2 has at least as many rows
      output$dynamic_inputs1 = renderUI({
        lapply(1:row_count1(), function(i) {
          create_row(i, "g1", input_choices2)
        }) # recreate k+1 empty rows for group 1
      })
      output$dynamic_inputs2 = renderUI({
        lapply(1:row_count2(), function(i) {
          create_row(i, "g2", input_choices2)
        }) # recreate k+1/row_count2 empty rows for group 2
      })
      restore_inputs("row_count1", input_choices2, row_count1, row_count2, g1_inputs, g2_inputs, session)
    })
    
    # Add row button for Group 2
    observeEvent(input$add_row2, {
      save_inputs("row_count2", row_count1, row_count2, g1_inputs, g2_inputs, input)
      new_row_count <- row_count2() + 1
      row_count2(new_row_count)
      row_count1(max(row_count1(), new_row_count))
      output$dynamic_inputs2 <- renderUI({
        lapply(1:row_count2(), function(i) {
          create_row(i, "g2", input_choices2)
        })
      })
      output$dynamic_inputs1 <- renderUI({
        lapply(1:row_count1(), function(i) {
          create_row(i, "g1", input_choices2)
        })
      })
      restore_inputs("row_count2", input_choices2, row_count1, row_count2, g1_inputs, g2_inputs, session)
    })
    
    
    # while add row restores all previous selections,  still need to  
    # update value choices real-time during normal interaction without needing to click add row
    observe({
      lapply(1:max(row_count1(), row_count2()), function(i) {
        lapply(c("g1", "g2"), function(group) {
          search_id = paste0("search_", group, "_", i)
          value_id = paste0("value_", group, "_", i)
          
          observeEvent(input[[search_id]], {
            if (!is.null(input[[search_id]]) && input[[search_id]] != "") {
              selected_var = input[[search_id]]
              matching_index = which(input_choices2$variable_display == selected_var)
              
              if (length(matching_index) > 0) {
                choices = input_choices2$choices[[matching_index[1]]]
                updateSelectizeInput(session, value_id,
                                     choices = c("Select..." = "", choices),
                                     selected = isolate(input[[value_id]]))
                
                # Synchronize search selection to the other group
                other_group = if(group == "g1") "g2" else "g1"
                updateSelectizeInput(session, paste0("search_", other_group, "_", i), selected = selected_var)
              } else {
                # Handle the case where no matching variable display is found
                updateSelectizeInput(session, value_id,
                                     choices = c("Select..." = ""),
                                     selected = character(0))
              }
            }
          }, ignoreInit = TRUE)
        })
      })
    })
    
    # print
    print (g1_inputs())
    print (g2_inputs())
    
    # function button (gen_tbl) action
    reactive_filtered_data = reactiveVal()
    observeEvent(input$gen_tbl, {
      # Extract choices from both groups and
      # if 'Select...' (empty string) is chosen then replace with 'All'
      selections_group1 = lapply(inputId, function(id) {
        choice = input[[paste0(id, "1")]]
        if (choice == "") "All" else choice
      })
      selections_group2 = lapply(inputId, function(id) {
        choice = input[[paste0(id, "2")]]
        if (choice == "") "All" else choice
      })
      
      # get the data
      bmtdata <- get_bmtdata(dir = data_dir)
      
      # generate filtered dataset based on selections 
      filtered_data = filter_data(age_range_1 = input$age_range1,
                                  age_range_2 = input$age_range2,
                                  dob = 'pt_dob',
                                  doe = 'tp_hct_date',
                                  list_choice_1 = selections_group1, 
                                  list_choice_2 = selections_group2, 
                                  inputId, 
                                  group_name_1 = input$g1_name, 
                                  group_name_2 = input$g2_name, 
                                  data = bmtdata)
      if (nrow(filtered_data) == 0) {
        showNotification("No data matches the selected criteria. Please adjust your selections.", 
                         type = "error")
        reactive_filtered_data(NULL)
      } else {
        reactive_filtered_data(filtered_data)
      }
    })
    
    output$baseline_chrcs = render_gt({
      # Only render the table if the reactive_filtered_data is not NULL
      req(reactive_filtered_data())
      # create gt table
      reactive_filtered_data() %>% 
        select (map_variable_name(tbl_variable_name), 'group') %>%
        tbl_summary(
          by = 'group',
          missing_text = '(Missing)',
          label = tbl_variable_list,
        ) %>% add_p() %>% add_stat_label() %>%
        as_gt()
    })
    
    ########## for time-to-event analysis tab ##########
    # logic to make sure when one dropdown is selected, 
    # the other dropdown goes to "please select..."
    observeEvent(input$curve_type_surv, {
      if (input$curve_type_surv != "") {
        updateSelectInput(session, "curve_type_cum", selected = "")
      }
    })
    observeEvent(input$curve_type_cum, {
      if (input$curve_type_cum != "") {
        updateSelectInput(session, "curve_type_surv", selected = "")
      }
    })
    
    # output curve functions
    output$KM_curves = renderPlot({
      
      filtered_data = reactive_filtered_data()
      req(input$curve_type_surv != '' | input$curve_type_cum != '')
      surv_params = surv_param(input$curve_type_surv, filtered_data)
      cum_params = cum_param(input$curve_type_cum, filtered_data)
      
      if (input$curve_type_surv != '') {
        # If the survival analysis dropdown is selected, plot survival data.
        surv_from_hct(
          subset_data = surv_params$subset_data, 
          surv_status = surv_params$surv_status, 
          surv_time = surv_params$surv_time, 
          surv_type = surv_params$surv_type, 
          group_names = surv_params$group_names
        )
      } else if (input$curve_type_cum != '') {
        # If the cumulative incidence dropdown is selected, plot CI data.
        ci_from_hct(
          subset_data = cum_params$subset_data, 
          ci_status = cum_params$ci_status,
          ci_time = cum_params$ci_time,
          surv_status = cum_params$surv_status, 
          surv_time = cum_params$surv_time, 
          ci_type = cum_params$ci_type, 
          group_names = cum_params$group_names
        )
      }
    })
    
    
    ########## for cox-regression tab ##########
    # Define a reactive value to store the results
    cox_results = reactiveVal(NULL)
    
    # Observer for the "Run Cox Regression" button
    observeEvent(input$run_cox, {
      req(input$cox_outcome_selection, input$cox_covariates)
      
      # Apply age group and HCI group filters
      filtered_data = reactive_filtered_data()
      filtered_data = filter_age_group(65, dob='pt_dob', doe='tp_hct_date', data=filtered_data)
      filtered_data = filter_hci_group(3, filtered_data)
      
      # Determine survival parameters
      cox_params = if (input$cox_outcome_selection %in% c('OS', 'RFS', 'GRFS', 'NRM', 'NRM_100')) {
        surv_param(input$cox_outcome_selection, filtered_data)
      } else {
        cum_param(input$cox_outcome_selection, filtered_data)
      }
      
      # Perform Cox regression and store the results
      results = if (input$regression_type == "univariate") {
        uni_cox(
          surv_time = cox_params$surv_time,
          surv_status = cox_params$surv_status,
          surv_type = input$cox_outcome_selection,
          variables = map_variable_name(input$cox_covariates),
          data = cox_params$subset_data
        )
      } else {
        multi_cox(
          surv_time = cox_params$surv_time,
          surv_status = cox_params$surv_status,
          surv_type = input$cox_outcome_selection,
          variables = map_variable_name(input$cox_covariates),
          data = cox_params$subset_data
        )
      }
      
      # Update the reactive value with the new results
      cox_results(results)
    })
    
    # Render the Cox regression results
    output$cox_reg_results <- render_gt({
      req(cox_results())
      cox_results() %>%
        as_gt() 
    })
    

  }
  
  shinyApp(ui = ui, server = server)
  
}
