#' @import dplyr
#' @import tidyr
#' @import gtsummary
#' @import survival

# Univariate Cox regression
#' @import dplyr
#' @import tidyr
#' @import gtsummary
#' @import survival
uni_cox = function(surv_time, surv_status, surv_type, variables, data) {
  caption = sprintf('Univariate Cox Regression of %s', surv_type)
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop over each variable and perform Cox regression
  for (var in variables) {
    surv_obj = Surv(data[[surv_time]], data[[surv_status]])
    formula_str = paste("surv_obj ~", var)
    formula = as.formula(formula_str)
    cox_model = coxph(formula, data = data)
    
    # Store the result in the list
    results_list[[var]] = tbl_regression(cox_model, exponentiate = TRUE)
  }
  
  # Combine the results into a single table
  combined_result = tbl_stack(results_list) %>%
    bold_labels() %>%
    modify_caption(caption)
  
  # return a table
  return(combined_result)
}


# Multivariable cox regression
#' @import dplyr
#' @import tidyr
#' @import gtsummary
#' @import survival
multi_cox = function(surv_time, surv_status, surv_type, variables, data) {
  # variables are a list of variables
  surv_obj = Surv(data[[surv_time]], data[[surv_status]])
  formula_str = paste("surv_obj ~", paste(variables, collapse = " + "))
  formula = as.formula(formula_str)
  cox_model = coxph(formula, data = data)
  caption = sprintf('Multivariable Cox Regression of %s', surv_type)
  return(cox_model%>%
           tbl_regression(exponentiate = TRUE) %>%
           modify_caption(caption) %>%
           bold_labels())
}


# Univariate logistic regression
#' @import dplyr
#' @import tidyr
#' @import gtsummary
#' @import survival
uni_log = function (event, event_name, variables, data) {
  caption = sprintf('Univariate Logistic Regression of %s', event_name)
  results_list <- list()
  for (var in variables) {
    formula_str = paste(event, "~", var)
    formula = as.formula(formula_str)
    log_model = glm(formula, data = data, family = binomial)
    results_list[[var]] = tbl_regression(log_model, exponentiate = TRUE)
  }
  
  # Combine the results into a single table
  combined_result = tbl_stack(results_list) %>%
    bold_labels() %>%
    modify_caption(caption)
  
  return(combined_result)
}

# multivariable logistic regression
#' @import dplyr
#' @import tidyr
#' @import gtsummary
#' @import survival
multi_log = function (event, event_name, variables, data) {
  caption = sprintf('Multivariable Logistic Regression of %s', event_name)
  formula_str = paste(event, "~", paste(variables, collapse = " + "))
  formula = as.formula(formula_str)
  log_model = glm(formula, data = data, family = binomial)
  
  # Return the regression table 
  result = log_model %>%
    tbl_regression(exponentiate = TRUE) %>%
    bold_labels() %>%
    modify_caption(caption)
  
  return(result)
}
