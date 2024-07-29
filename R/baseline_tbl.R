
# extract label and choices from dict based on 'inputID'
#' @import dplyr
#' @import tidyr
#' @import gtsummary
#' 
#' 
# return a datafarme with variable, label, and options for given variables based on the data dict
select_input_choices = function(inputId, dict) {
  filtered_dict = dict %>% filter(variable %in% inputId)
  input_choices = data.frame(
    inputId = filtered_dict$variable,
    variable_display = filtered_dict$label,
    choices = I(filtered_dict$attributes_name),
    stringsAsFactors = FALSE
  )
  return(input_choices)
}

# UI to create a new row of inputs, with 2 columns (e.g. search_g1_1 and value_g1_1)
# search is single search/selection, value is single/multiple selections
# choices are input_choices2 (the name is specific)
create_row = function(id, group, input_choice) {
  fluidRow(
    column(6, 
           selectizeInput(
             inputId = paste0("search_", group, "_", id),
             label = NULL,
             choices = c("Select..." = "", input_choice$variable_display),
             options = list(
               placeholder = 'Type to search...',
               onInitialize = I('function() { this.setValue(""); }')
             )
           )
    ),
    column(6,
           selectizeInput(
             inputId = paste0("value_", group, "_", id),
             label = NULL,
             choices = c("Select..." = ""),
             multiple = TRUE,
             options = list(placeholder = 'Select value(s)...')
           )
    )
  )
}

# function to save inputs of all previous rows 
# output 2 lists with format (search_g1_1 = '', value_g1_1 = '',...)
save_inputs = function(row_count, g1_inputs, g2_inputs, input) {
  for (i in 1:row_count) {
    g1_inputs[[paste0("search_g1_", i)]] = input[[paste0("search_g1_", i)]]
    g1_inputs[[paste0("value_g1_", i)]] = input[[paste0("value_g1_", i)]]
    g2_inputs[[paste0("search_g2_", i)]] = input[[paste0("search_g2_", i)]]
    g2_inputs[[paste0("value_g2_", i)]] = input[[paste0("value_g2_", i)]]
  }
}

# Function to restore input values (g1_inputs and g2_inputs) from g1/g2_input_list
# Function to restore input values (g1_inputs and g2_inputs) from g1/g2_input_list
restore_inputs = function(input_choices, row_count, g1_inputs, g2_inputs, session) {
  for (i in 1:row_count) {
    
    if (!is.null(g1_inputs[[paste0("search_g1_", i)]])) {
      updateSelectizeInput(session, 
                           paste0("search_g1_", i), 
                           selected = g1_inputs[[paste0("search_g1_", i)]])
      
      updateSelectizeInput(session, 
                           paste0("search_g2_", i), 
                           selected = g2_inputs[[paste0("search_g2_", i)]])
      
      # restore value selections
      if (!is.null(g1_inputs[[paste0("value_g1_", i)]])) {
        updateSelectizeInput(session, 
                             paste0("value_g1_", i), 
                             choices = c("Select..." = "", 
                                         input_choices$choices[[which(input_choices$variable_display == g1_inputs[[paste0("search_g1_", i)]])]]), 
                             selected = g1_inputs[[paste0("value_g1_", i)]])
      }
      
      if (!is.null(g2_inputs[[paste0("value_g2_", i)]])) {
        updateSelectizeInput(session, 
                             paste0("value_g2_", i), 
                             choices = c("Select..." = "", 
                                         input_choices$choices[[which(input_choices$variable_display == g2_inputs[[paste0("search_g2_", i)]])]]), 
                             selected = g2_inputs[[paste0("value_g2_", i)]])
      }
    }
  }
}


# transform g1_inputs and g2_inputs into a list format
# from (search_g1_1='variable_display', value_g1_1='a', ) to list ('variable_display' = 'a', '' = '', ), 
# then to ('inputId' = 'a', '' = '', )
transform_group_inputs = function(group_inputs, group_number, input_choices) {
  result = list()
  
  # Create the search prefix based on group number
  search_prefix = paste0("search_g", group_number, "_")
  value_prefix = paste0("value_g", group_number, "_")
  
  # Get all the search keys
  search_keys = names(group_inputs)[grep(paste0("^", search_prefix), names(group_inputs))]
  
  for (key in search_keys) {
    # Extract the index
    index = sub(search_prefix, "", key)
    
    # Get the corresponding value key
    value_key = paste0(value_prefix, index)
    
    # Get the search criterion and its corresponding values
    criterion <- group_inputs[[key]]
    values <- group_inputs[[value_key]]
    
    # If value is NULL, change it to All
    if (is.null(values)) {
      values = "All"
    }
    
    result[[criterion]] = values
  }
  
  # mapping from list ('variable_display' = 'a', ) to ('inputId' = 'a', )
  mapping = setNames(input_choices$inputId, input_choices$variable_display)
  names(result) = mapping[names(result)]
  
  return(result)
}

# filter age based on a range, and doe (date of event), dob (date of birth)
# create a new variable named 'new_age'
#' @import dplyr
#' @import tidyr
#' @import gtsummary
filter_age = function (age_range, dob, doe, data) {
  # Safe conversion to Date format with handling NA or incorrect formats
  data$dob_date = tryCatch(as.Date(data[[dob]]), error = function(e) return('Not date format'))
  data$doe_date = tryCatch(as.Date(data[[doe]]), error = function(e) return('Not date format'))
  
  # Calculate age safely, considering potential NA values in date conversion
  data$new_age = ifelse(!is.na(data$dob_date) & !is.na(data$doe_date),
                        as.integer((data$doe_date - data$dob_date) / 365.25),
                        NA)
  # Filter data based on age range
  filtered_data = data[!is.na(data$new_age) & data$new_age >= age_range[1] & data$new_age <= age_range[2], ]
  return(filtered_data)
}


# for an age threshold k, create a new variable named "age_group_k", 
# where age>=k dummy labeled as '>=k', age<k labeled as '<k'
#' @import dplyr
#' @import tidyr
#' @import gtsummary
filter_age_group = function (k, dob, doe, data) {
  # Safe conversion to Date format with handling NA or incorrect formats
  data$dob_date = tryCatch(as.Date(data[[dob]]), error = function(e) return('Not date format'))
  data$doe_date = tryCatch(as.Date(data[[doe]]), error = function(e) return('Not date format'))
  
  # Calculate age safely, considering potential NA values in date conversion
  data$age = ifelse(!is.na(data$dob_date) & !is.na(data$doe_date),
                    as.integer((data$doe_date - data$dob_date) / 365.25),
                    NA)
  
  # Create new variable for age group based on the threshold k
  data[[paste("age_group", k, sep = "_")]] = ifelse(data$age >= k, paste('>=',k) , paste('<',k))
  return(data)
}

# for a HCT-CI threshold k, create a new variable "hct_ci_k", where
# HCT-CI>=k dummy labeled as '>=k', HCT-CI<k labeled as '<k'
#' @import dplyr
#' @import tidyr
#' @import gtsummary
filter_hci_group = function (k, data) {
  # Check if 'hct_ci' column exists in the data
  if (!"hct_ci" %in% colnames(data)) {
    stop("Error: 'hct_ci' column not found in the data.")
  }
  # Create new variable "hct_ci_k"
  data[[paste("hct_ci", k, sep = "_")]] = ifelse(is.na(data$hct_ci), NA,
                                                 ifelse(data$hct_ci >= k, paste('>=',k), paste('<',k)))
  return(data)
}


# filter based on date range
#' @import dplyr
filter_date = function(start_date, end_date, event_date, data) {
  # Convert input dates to Date objects
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  
  # Ensure the event_date column is in Date format
  data = data %>%
    mutate(!!event_date := as.Date(!!sym(event_date)))
  
  # Filter the data
  filtered_data = data %>%
    filter(!!sym(event_date) >= start_date & !!sym(event_date) <= end_date)
  
  return(filtered_data)
}


# based on input selections (list_choice) to filter data and create Group 1 and 2
# return filtered data, with a new variable "group" with names from "group_name"
#' @import dplyr
#' @import tidyr
#' @import gtsummary
filter_data = function(age_range_1, age_range_2, dob, doe,
                       list_choice_1, list_choice_2,
                       group_name_1, group_name_2, data) {

  if (length(list_choice_1) != length(list_choice_2)) {
    stop("Error: list_choice_1 and list_choice_2 have different lengths")
  }
  # Start filtering based on age ranges for both groups
  filtered_group1 = filter_age(age_range_1, dob, doe, data)
  filtered_group2 = filter_age(age_range_2, dob, doe, data)

  # Helper function to apply filters
  apply_filters <- function(filtered_data, choices) {
    for (column_name in names(choices)) {
      values <- choices[[column_name]]
      if (is.list(values)) {
        # Handle case where values is a list 
        values <- unlist(values)
      }

      # Ensure `values` is not NULL and has length > 0
      if (!is.null(values) && length(values) > 0) {
        # Only filter when "All" is not selected and values are not empty
        if (!identical(values, "All") && any(values != "")) {
          filtered_data <- filtered_data[filtered_data[[column_name]] %in% values, ]
        }
      }
    }
    return(filtered_data)
  }

  # Filter data for Group 1
  filtered_group1 = apply_filters(filtered_group1, list_choice_1)

  # Filter data for Group 2
  filtered_group2 = apply_filters(filtered_group2, list_choice_2)

  # Assign group labels by creating a new variable "group"
  filtered_group1$group = group_name_1
  filtered_group2$group = group_name_2

  filtered_data = rbind(filtered_group1, filtered_group2)

  return(filtered_data)
}