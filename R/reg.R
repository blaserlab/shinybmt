#' @import dplyr
#' @import tidyr
#' @import gtsummary
#' @import survival
#' @import kableExtra

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
    add_inline_forest_plot(vline=1) %>%
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
           add_inline_forest_plot(vline=0) %>%
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


# Blow is modified from "ddsjoberg/bstfun/add_inline_forest_plot()" to adde forest plot
#' @import dplyr
#' @import tidyr
#' @import gtsummary
#' @import survival
#' @import kableExtra
add_inline_forest_plot <- function(x, header = "**Forest Plot**",
                                   spec_pointrange.args = NULL,
                                   vline) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "gtsummary")) rlang::abort("`x=` must be class 'gtsummary'")
  if (!all(c("estimate", "conf.low", "conf.high") %in% names(x$table_body)))
    rlang::abort("`x$table_body` must contain columns 'estimate', 'conf.low', 'conf.high'")
  #assert_package("kableExtra", "add_inline_forest_plot()")
  updated_call_list <- c(x$call_list, list(add_inline_forest_plot = match.call()))
  
  # if exponentiated, plot on the log scale ------------------------------------
  scale_fun <-
    switch(
      inherits(x, c("tbl_regression", "tbl_uvregression")) && x$inputs$exponentiate,
      log
    ) %||%
    identity
  
  # add column with forest plot ------------------------------------------------
  # prepping arguments for `kableExtra::spec_pointrange()`
  spec_pointrange.args <-
    list(vline = vline, width = 600, cex = .8, col = "black", pch = 16) %>%  # change here to modify size of forest plot
    purrr::list_modify(!!!spec_pointrange.args) %>%
    purrr::list_modify(
      x = ifelse(scale_fun(x$table_body$conf.low) > 10, 10, scale_fun(x$table_body$estimate)),
      xmin = ifelse(scale_fun(x$table_body$conf.low) > 10, 10, scale_fun(x$table_body$conf.low)),
      xmax = ifelse(scale_fun(x$table_body$conf.low) > 10, 10, pmin(scale_fun(x$table_body$conf.high), 10))
    ) # max out at 10
  
  x <-
    gtsummary::modify_table_body(
      x = x,
      ~.x %>%
        # construct the forest plots and add to `x$table_body`
        dplyr::bind_cols(
          tibble::tibble(
            forest_plot =
              rlang::inject(kableExtra::spec_pointrange(!!!spec_pointrange.args)) %>%
              purrr::map("svg_text") %>%
              purrr::map(~gt::html(as.character(.x)))
          )
        ) %>%
        # move forest plot to before the coef
        dplyr::relocate("forest_plot", .before = "estimate") %>%
        # remove empty forest plots
        dplyr::mutate(forest_plot = purrr::map2(.data$estimate, .data$forest_plot,
                                         ~switch(!is.na(.x), .y)))
    ) %>%
    gtsummary::modify_table_styling(
      columns = "forest_plot",
      hide = FALSE,
      label = header
    )
  
  # return updated gtsummary object --------------------------------------------
  x$call_list <- updated_call_list
  x
}
