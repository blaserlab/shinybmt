#' @import ggplot2
#' @import tidycmprsk
#' @import dplyr
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
    'tx_type',
    'donor',
    'gvhdpr'
  )
  
  inputId2 = c(
    'RemSta',
    'doncmv',
    'aborh',
    "Crd", "CVD", "DM", "HVD", "Hpm", "Hps", "Ifc", "IBD", "Obs", "Pep", "Psy", "Plm", "Pls", "Ren", "Rhe", "SoT"
  )

  input_choices1 = select_input_choices(inputId1, dict)
  input_choices2 = select_input_choices(inputId2, dict)
  
  # manually select variables to be included in the baseline table
  tbl_variable_name = c('Age', 'Sex', 'Race', 'Diagnosis', 'Prep type', 'Donor type')
  # generate format list(new_Age ~ 'Age',...) for gtsummary
  tbl_variable_list = setNames(as.list(tbl_variable_name), map_variable_name(tbl_variable_name))
  
  # A selection of K-M functions
  surv_selection = c('Please select...' = '', 'OS', 'RFS', 'GRFS')
  cum_selection = c('Please select...' = '', 'NRM', 'ANC engraftment', 'Plt engraftment', 
                    'G2-4 aGvHD', 'G3-4 aGvHD')
  # A selection of Cox-regression outcomes
  cox_selection = c('Please select...' = '', 'OS', 'RFS', 'GRFS', 'NRM', 'G2-4 aGvHD', 'G3-4 aGvHD')
  # A selection of Cox-regression covariates
  cox_covariates_names = c('Group', 'Age (>=65)', 'Sex', 'Race', 'Diagnosis', 
                           'Prep type', 'Donor type', 'Disease status', 'HCT-CI (>=3)')
  
  
  ui <- fluidPage(
    tabsetPanel(
      tabPanel("Baseline Selection",
               tags$head(
                 tags$style(HTML("
      .panel-title { font-size: 24px; font-weight: bold; margin-bottom: 20px; }
      .additional-criteria { font-size: 18px; font-weight: bold; text-align: center; margin-top: 30px; }
      hr { border-top: 2px dashed #95a5a6; margin-top: 10px; margin-bottom: 30px; }
      .btn-primary { background-color: #3498db; border-color: #2980b9; }
      .btn-primary:hover { background-color: #2980b9; border-color: #2980b9; }
      .group-column { padding: 20px; border-radius: 8px; }
      .group-1 { background-color: #ecf0f1; }
      .group-2 { background-color: #e8f6f3; }
    "))
               ),
               fluidRow(
                 column(3,
                        div(class = "group-column group-1",
                            tags$h3("Group 1", class = "panel-title"),
                            textInput("g1_name", "Name this group:", value='Group 1'),
                            sliderInput("age_range1", "Select age range:", 
                                        min = 0, max = 120, value = c(18, 120)),
                            dateRangeInput("transplant_date1", "Transplant date:",
                                           start = as.Date(paste0(as.integer(format(Sys.Date(), "%Y")) - 10, "-01-01")),
                                           end = Sys.Date(),
                                           format = "MM/yyyy",
                                           startview = "year"),
                            lapply(1:nrow(input_choices1), function(i) {
                              selectizeInput(
                                inputId = paste0(input_choices1$inputId[i], "1"),
                                label = sprintf("%s:", input_choices1$variable_display[i]),
                                choices = c('Select...' = '', 'All', input_choices1$choices[[i]]),
                                multiple = TRUE,
                                options = list(
                                  placeholder = 'Select options',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )
                              )
                            }),
                            tags$div(class = "additional-criteria", "Additional criteria"),
                            tags$hr(), 
                            uiOutput("dynamic_inputs1"),
                            div(style = "display: flex; justify-content: flex-end; align-items: center; margin-top: 20px; padding-right: 0px;",
                                actionButton("add_row", "Add New Row", class = "btn-info")
                            )
                        )
                 ),
                 column(3,
                        div(class = "group-column group-2",
                            tags$h3("Group 2", class = "panel-title"),
                            textInput("g2_name", "Name this group:", value='Group 2'),
                            sliderInput("age_range2", "Select age range:", 
                                        min = 0, max = 120, value = c(18, 120)),
                            dateRangeInput("transplant_date2", "Transplant date:",
                                           start = as.Date(paste0(as.integer(format(Sys.Date(), "%Y")) - 10, "-01-01")),
                                           end = Sys.Date(),
                                           format = "MM/yyyy",
                                           startview = "year"),
                            lapply(1:nrow(input_choices1), function(i) {
                              selectizeInput(
                                inputId = paste0(input_choices1$inputId[i], "2"),
                                label = sprintf("%s:", input_choices1$variable_display[i]),
                                choices = c('Select...' = '', 'All', input_choices1$choices[[i]]),
                                multiple = TRUE,
                                options = list(
                                  placeholder = 'Select options',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )
                              )
                            }),
                            tags$div(class = "additional-criteria", "Additional criteria"),
                            tags$hr(), 
                            uiOutput("dynamic_inputs2")
                        )
                 ),
                 div(style = "text-align: center; margin-top: 30px;",
                     actionButton("gen_tbl", "Generate Study Cohorts", 
                                  class = 'btn-primary')),
                 column(6, 
                        div(class = "well", style = "margin-top: 20px;",
                            gt_output("baseline_chrcs")
                        )
                 )
               )
      ),
      tabPanel("Time-to-Event Analysis",
               sidebarPanel(
                 selectInput("curve_type_surv", "Survival analysis:", 
                             choices = surv_selection,
                             selected = ''),
                 selectInput("curve_type_cum", "Cumulative incidence analysis:", 
                             choices = cum_selection,
                             selected = '')
               ),
               mainPanel(
                 plotOutput("KM_curves"),
                 gt_output("summary_table"))
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
    
    # date selection synchronization
    # Error handling for transplant_date1
    valid_date = reactiveVal(TRUE)
    observeEvent(input$transplant_date1, {
      date_range = input$transplant_date1
      if (date_range[2] <= date_range[1]) {
        showNotification(
          "Error: End date must be later than start date for Group 1",
          type = "error",
        )
        valid_date(FALSE)
      } else {
        # If the date range is valid, update transplant_date2
        updateDateRangeInput(session, "transplant_date2",
                             start = date_range[1],
                             end = date_range[2])
        valid_date(TRUE)
      }
    }, ignoreInit = TRUE)
    
    # Error handling for transplant_date2
    observeEvent(input$transplant_date2, {
      date_range = input$transplant_date2
      if (date_range[2] <= date_range[1]) {
        showNotification(
          "Error: End date must be later than start date for Group 2",
          type = "error"
        )
        valid_date(FALSE)
      } else {
        valid_date(TRUE)
      }
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
    row_count = reactiveVal(1)
    
    # Reactive lists to store input values for each group
    g1_inputs = reactiveValues()
    g2_inputs = reactiveValues()
    
    # Initialize the 1st row for Group 1 and 2
    output$dynamic_inputs1 = renderUI({
      create_row(1, "g1", input_choices2)
    })
    output$dynamic_inputs2 = renderUI({
      create_row(1, "g2", input_choices2)
    })
    
    # "Add row" button logistics for Group 1
    observeEvent(input$add_row, {
      save_inputs(row_count(), g1_inputs, g2_inputs, input) 
      # save contents of previous rows for g1 and g2
      row_count(row_count() + 1)
      output$dynamic_inputs1 = renderUI({
        lapply(1:row_count(), function(i) {
          create_row(i, "g1", input_choices2)
        }) # recreate k+1 empty rows for group 1
      })
      output$dynamic_inputs2 = renderUI({
        lapply(1:row_count(), function(i) {
          create_row(i, "g2", input_choices2)
        }) # recreate k+1 empty rows for group 2
      })
      restore_inputs(input_choices2, row_count(), g1_inputs, g2_inputs, session)
    })
    
    observe({
      lapply(1:row_count(), function(i) {
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
            } else {
              # Handle the case where search_id input is NULL or empty
              updateSelectizeInput(session, value_id,
                                   choices = c("Select..." = ""),
                                   selected = "All")
            }
          }, ignoreInit = TRUE)
        })
      })
    })
    
    
    # function button (gen_tbl) action
    reactive_filtered_data = reactiveVal()
    no_duplicates = reactiveVal(TRUE)
    
    observeEvent(input$gen_tbl, {
      
      save_inputs(row_count(), g1_inputs, g2_inputs, input) 
      
      # Check for duplicates in search_g1_* values
      observe({
        search_values = reactiveValuesToList(g1_inputs)[grep("^search_g1_", names(g1_inputs))]
        search_values = search_values[search_values != ""]
        
        if (any(duplicated(search_values))) {
          showNotification("There are duplicate criteria in search fields", type = "error")
          no_duplicates(FALSE)
        } else {
          no_duplicates(TRUE)
        }
      })
      
      transformed_g1 = transform_group_inputs(g1_inputs, 1, input_choices2)
      transformed_g2 = transform_group_inputs(g2_inputs, 2, input_choices2)
      
      # Extract choices from set groups and
      # if 'Select...' (empty string) is chosen then replace with 'All'
      selections_group1 = lapply(inputId1, function(id) {
        choice = input[[paste0(id, "1")]]
        if (length(choice) == 0 || "All" %in% choice) "All" else choice
      })
      
      selections_group2 = lapply(inputId1, function(id) {
        choice = input[[paste0(id, "2")]]
        if (length(choice) == 0 || "All" %in% choice) "All" else choice
      })
      
      # add a name column for selections_group1/2
      names(selections_group1) = inputId1
      names(selections_group2) = inputId1
      

      ##### get the data #####
      bmtdata <- get_bmtdata(dir = data_dir)
      
      # filter data
      filtered_data = filter_data(age_range_1 = input$age_range1,
                                  age_range_2 = input$age_range2,
                                  dob = 'dob',
                                  doe = 'bmt_date',
                                  list_choice_1 = c(selections_group1, transformed_g1), 
                                  list_choice_2 = c(selections_group2, transformed_g2), 
                                  group_name_1 = input$g1_name, 
                                  group_name_2 = input$g2_name, 
                                  data = bmtdata)
      
      # further filter data based on defined transplant data range
      filtered_data = rbind(
        filter_date(input$transplant_date1[1], 
                    input$transplant_date1[2], 
                    'bmt_date', 
                    filtered_data %>% filter(group == input$g1_name)),
        filter_date(input$transplant_date2[1], 
                    input$transplant_date2[2], 
                    'bmt_date',  
                    filtered_data %>% filter(group == input$g2_name))
      )
      
      # error handling if 0 pt meets criteria
      if (nrow(filtered_data) == 0) {
        showNotification("No patients matches the selected criteria. Please adjust your selections.", 
                         type = "error")
        reactive_filtered_data(NULL)
      } else {
        reactive_filtered_data(filtered_data)
      }
    })
    
    ## TABLE OUTPUT ##
    output$baseline_chrcs = render_gt({
      # check error messages
      if (!no_duplicates() || !valid_date() || is.null(reactive_filtered_data())) {
        # Return an empty gt table if conditions are not met
        return(gt::gt(data.frame()))
      }
      
      # create gt table
      reactive_filtered_data() %>% 
        select (map_variable_name(tbl_variable_name), 'group') %>%
        tbl_summary(
          by = 'group',
          missing_text = '(Missing)',
          label = tbl_variable_list
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
    
    # summary table functions
    output$summary_table = render_gt({
      
      filtered_data = reactive_filtered_data()
      surv_params = surv_param(input$curve_type_surv, filtered_data)
      cum_params = cum_param(input$curve_type_cum, filtered_data)
      
      if (input$curve_type_surv != '') {
        # If the survival analysis dropdown is selected, plot survival data.
        surv_table = surv_table_from_hct(
          subset_data = surv_params$subset_data, 
          surv_status = surv_params$surv_status, 
          surv_time = surv_params$surv_time, 
          surv_type = surv_params$surv_type, 
          group_names = surv_params$group_names
        )
        surv_table %>% as_gt()
      } else if (input$curve_type_cum %in% c('NRM', 'G2-4 aGvHD', 'G3-4 aGvHD')) {
        cum_table = ci_table_from_hct(
          subset_data = cum_params$subset_data, 
          ci_status = cum_params$ci_status,
          ci_time = cum_params$ci_time,
          surv_status = cum_params$surv_status, 
          surv_time = cum_params$surv_time, 
          ci_type = cum_params$ci_type, 
          group_names = cum_params$group_names
        )
        cum_table %>% as_gt()
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
      filtered_data = filter_age_group(65, dob='dob', doe='bmt_date', data=filtered_data)
      filtered_data = filter_hci_group(3, filtered_data)
      
      # Determine survival parameters
      cox_params = if (input$cox_outcome_selection %in% c('OS', 'RFS', 'GRFS')) {
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
