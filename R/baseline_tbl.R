
# extract label and choices from dict based on 'inputID'
#' @import dplyr
#' @import tidyr
#' @import gtsummary
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



# based on input selections (list_choice) to filter data and create Group 1 and 2
# return filtered data, with a new variable "group" with names from "group_name"
#' @import dplyr
#' @import tidyr
#' @import gtsummary
filter_data = function(age_range_1, age_range_2, dob, doe,
                       list_choice_1, list_choice_2, inputId, 
                       group_name_1, group_name_2, data) {
  # Start filtering based on age ranges for both groups
  filtered_group1 = filter_age(age_range_1, dob, doe, data)
  filtered_group2 = filter_age(age_range_2, dob, doe, data)
  
  # Filter data for Group 1
  for (i in seq_along(inputId)) {
    if (list_choice_1[i] != "All") {
      # Apply filter only if the choice is not "All"
      filtered_group1 = filtered_group1[filtered_group1[[inputId[i]]] == list_choice_1[i], ]
    }
  }
  
  # Filter data for Group 2
  for (i in seq_along(inputId)) {
    if (list_choice_2[i] != "All") {
      # Apply filter only if the choice is not "All"
      filtered_group2 = filtered_group2[filtered_group2[[inputId[i]]] == list_choice_2[i], ]
    }
  }
  
  # Assign group labels by creating a new variable "group"
  filtered_group1$group = group_name_1
  filtered_group2$group = group_name_2
  
  filtered_data = rbind(filtered_group1, filtered_group2)
  
  return(filtered_data)
}
