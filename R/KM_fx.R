
# survival function
# surv_type = c('OS', 'PFS', 'GRFS', 'NRM', 'NRM_100')
#' @import ggplot2
#' @import tidycmprsk
#' @import dplyr
#' @import tidyr
#' @import survminer
#' @import survival
#' @import gtsummary
#' @import ggsurvfit
#' @import ggsci

surv_from_hct = function(subset_data, surv_status, surv_time, 
                         group_names, surv_type) {
  
  # surv_status, surv_time, group_names are symbols 
  formula_str = sprintf("Surv(%s, %s) ~ %s", surv_time, surv_status, group_names)
  surv_formula = as.formula(formula_str)
  max_time = max(subset_data[[surv_time]], na.rm = TRUE)
  
  # create K-M and cox models
  surv_model = surv_fit(surv_formula, subset_data)
  surv_cox = coxph(surv_formula, subset_data)
  surv_hr = exp(surv_cox$coef)
  surv_ci_lower = exp(confint(surv_cox))[, 1]
  surv_ci_upper = exp(confint(surv_cox))[, 2]
  
  # remove annoying 'group=' prefix in legends: pay attention, this is name specific
  names(surv_model$strata) = gsub("group=", "", names(surv_model$strata))
  
  # draw CI plot for NRM, and survival plot for the rest
  if (surv_type %in% c('OS', 'RFS', 'GRFS')) {
    surv_plot = ggsurvplot(surv_model, data=subset_data,
                           title = sprintf("%s", surv_type),
                           palette = "npg", censor = T,
                           conf.int = T, conf.int.alpha = 0.2,
                           ylim = c(0, 1), 
                           xlim = c(0,max_time + 50),
                           break.x.by = round(max_time / 100) * 10,
                           xlab = "Time since transplant (days)",
                           ylab = sprintf("%s probability", surv_type),
                           legend = 'bottom',
                           risk.table = T,
                           risk.table.col = "strata",
                           tables.theme = theme_void(),
                           pval = T)
    surv_plot$plot = surv_plot$plot + theme(legend.title=element_blank()) +
      ggplot2::annotate("text", hjust = 0, x = 0, y = 0.1,
                        label = sprintf("HR %.2f, 95%%CI %.2f to %.2f", 
                                        surv_hr, surv_ci_lower, surv_ci_upper),
                        size = 5)
    return(surv_plot)
  } else if (surv_type =='NRM') {
    surv_plot = ggsurvplot(surv_model, data=subset_data,
                           title = sprintf("%s", surv_type),
                           palette = "npg", censor = T,
                           fun = "cumhaz",
                           conf.int = T, conf.int.alpha = 0.2,
                           ylim = c(0, 1), 
                           xlim = c(0,max_time + 50),
                           break.x.by = round(max_time / 100) * 10,
                           xlab = "Time since transplant (days)",
                           ylab = sprintf("%s probability", surv_type),
                           legend = 'bottom',
                           risk.table = T,
                           risk.table.col = "strata",
                           tables.theme = theme_void(),
                           pval = T)
    surv_plot$plot = surv_plot$plot + theme(legend.title=element_blank()) +
      ggplot2::annotate("text", hjust = 1, x = max_time, y = 0.1,
                        label = sprintf("HR %.2f, 95%%CI %.2f to %.2f", 
                                        surv_hr, surv_ci_lower, surv_ci_upper),
                        size = 5)
    return(surv_plot)
  } else if (surv_type =='NRM_100') {
    surv_plot = ggsurvplot(surv_model, data=subset_data,
                           title = sprintf("%s", surv_type),
                           palette = "npg", censor = T,
                           fun = "cumhaz",
                           conf.int = T, conf.int.alpha = 0.2,
                           ylim = c(0, 1), 
                           xlim = c(0,105),
                           break.x.by = 20,
                           xlab = "Time since transplant (days)",
                           ylab = sprintf("%s probability", surv_type),
                           legend = 'bottom',
                           risk.table = T,
                           risk.table.col = "strata",
                           tables.theme = theme_void(),
                           pval = T)
    surv_plot$plot = surv_plot$plot + theme(legend.title=element_blank()) +
      ggplot2::annotate("text", hjust = 0, x = 60, y = 0.1,
                        label = sprintf("HR %.2f, 95%%CI %.2f to %.2f", 
                                        surv_hr, surv_ci_lower, surv_ci_upper),
                        size = 5)
    return(surv_plot)
  }
  
  
}



# cumulative incidence (ci) function
# Fine-Gray competing risk model, treating death as a competing risk
# ci_type = c('ANC engraftment', 'Plt engraftment', 'G2-4 aGvHD', 'G3-4 aGvHD')
#' @import ggplot2
#' @import tidycmprsk
#' @import dplyr
#' @import tidyr
#' @import survminer
#' @import survival
#' @import gtsummary
#' @import ggsurvfit
#' @import ggsci
ci_from_hct = function(subset_data, ci_status, ci_time, surv_status, surv_time,
                       group_names, ci_type) {

  # define time and status for competing risk analysis by
  # combining cumulative incidence and overall survival
  subset_data = subset_data %>%
    dplyr::filter(!( .data[[ci_status]] == 1 & is.na(.data[[ci_time]]))) %>%
    dplyr::filter(.data[[ci_status]] != 0 | is.na(.data[[ci_time]]))

  subset_data$cmprsk_time = ifelse(is.na(subset_data[[ci_time]]), 
                                   subset_data[[surv_time]], 
                                   subset_data[[ci_time]])

  # status: 0=censor, 1=event(e.g. engraftment), 2=death
  subset_data$cmprsk_status = ifelse(subset_data[[ci_status]] == 1, 
                                     1, 
                                     ifelse(subset_data[[surv_status]] == 1, 
                                            2, 0))
  subset_data$cmprsk_status = as.factor(subset_data$cmprsk_status)
  
  # Correctly constructing the model formula using the 'group_names' variable
  time_status_formula = reformulate(termlabels = group_names, 
                                    response = 'Surv(cmprsk_time, cmprsk_status)')
  
  # Creating models
  ci_model = cuminc(time_status_formula, data = subset_data)
  ci_cox = crr(time_status_formula, data = subset_data)
  tbl = gtsummary::tbl_regression(ci_cox, exponentiate = TRUE)

  # Extracting inline text for annotation
  # Level is set to the 2nd element (ie group2 vs group1) - if >2 groups, need update
  subset_data[[group_names]] = as.factor(subset_data[[group_names]])
  annotate_text = gtsummary::inline_text(tbl, variable = group_names, 
                                         level = levels(subset_data[[group_names]])[2])

  # draw plot
  ci_plot = ggcuminc(ci_model, linetype_aes = T, linewidth = 1) +
    theme_classic() +
    scale_color_npg() +
    scale_fill_npg() +
    labs(y = "Cumulative Incidence",
         title = sprintf("%s", ci_type),
         x = "Time since transplant (days)") +
    coord_cartesian(ylim = c(0, 1)) + 
    theme(legend.position = "bottom",
          text = element_text(size = 14)) +
    add_confidence_interval() +
    add_risktable(size = 4) +
    scale_ggsurvfit() +
    annotate ("text", hjust = 1, x = max(subset_data$cmprsk_time, na.rm=T),
              y = 0.1,
              label = paste('HR', annotate_text),
              size = 4.5)

  return(ci_plot)
}
