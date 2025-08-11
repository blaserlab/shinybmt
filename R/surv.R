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

  } 
  return(surv_plot)
  
}

# survival summary table function ('OS', 'RFS', 'GRFS')
#' @import survival
#' @import dplyr
#' @import gt
#' @import gtsummary
surv_table_from_hct = function(subset_data, surv_status, surv_time, 
                               group_names, surv_type) {
  
  # surv_status, surv_time, group_names are symbols 
  formula_str = sprintf("Surv(%s, %s) ~ %s", surv_time, surv_status, group_names)
  surv_formula = as.formula(formula_str)
  surv_model = surv_fit(surv_formula, data = subset_data)
  
  # draw CI plot for NRM, and survival plot for the rest
  if (surv_type %in% c('OS', 'RFS', 'GRFS')) {
    surv_table_1 = tbl_survfit(
      surv_model,
      times = c(365, 730, 1825),
      label_header = "**{time/365}-yr est, %(95%CI)**"
    )
    surv_table_2 = tbl_survfit(
      surv_model,
      probs = 0.5,
      label_header = paste0("**Median, days(95%CI)**")
    )
    surv_table = tbl_merge(
      tbls = list(surv_table_1, surv_table_2),
      tab_spanner = FALSE
    ) %>%
      modify_header(label ~ paste0("**", surv_type, "**"))
    
  } 
  return(surv_table)
}

# cumulative incidence (ci) function
# Fine-Gray competing risk model, treating death as a competing risk for engraftment and GVHD
# treating relapse as a competing risk for NRM
# ci_type = c('NRM', 'ANC engraftment', 'Plt engraftment', 'G2-4 aGvHD', 'G3-4 aGvHD')
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
  
  # remove inconsistencies in data
  subset_data = subset_data %>%
    filter(!( .data[[ci_status]] == 1 & is.na(.data[[ci_time]])))
  
  # define time and status for competing risk analysis by
  # combining cumulative incidence and overall survival
  subset_data$cmprsk_time = ifelse(is.na(subset_data[[ci_time]]), 
                                   subset_data[[surv_time]], 
                                   subset_data[[ci_time]])

  # status: 0=censor, 1=event(e.g. engraftment), 2=death (or relapse for NRM)
  if (ci_type == "NRM") {
    subset_data$cmprsk_status = case_when(
      subset_data[[ci_status]] == 1 & subset_data[[surv_status]] == 0 ~ 1,  # NRM event
      subset_data[[surv_status]] == 1 ~ 2,  # Competing event (relapse)
      TRUE ~ 0  # Censored
    )
  } else if (ci_type %in% c('ANC engraftment', 'Plt engraftment', 'G2-4 aGvHD', 'G3-4 aGvHD')) {
    subset_data$cmprsk_status = case_when(
      subset_data[[ci_status]] == 1 ~ 1,  # Engraftment/GVHD event
      subset_data[[surv_status]] == 1 ~ 2,  # Competing event (death)
      TRUE ~ 0  # Censored
    )
  }
    
  subset_data$cmprsk_status = as.factor(subset_data$cmprsk_status)
  
  subset_data = subset_data %>%
    dplyr::filter(!is.na(cmprsk_time), !is.na(cmprsk_status), !is.na(.data[[group_names]]))
  
  # Correctly constructing the model formula using the 'group_names' variable
  time_status_formula = reformulate(termlabels = group_names, 
                                    response = 'Surv(cmprsk_time, cmprsk_status)')
  
  # Creating models
  ci_model = cuminc(time_status_formula, data = subset_data)
  ci_cox = crr(time_status_formula, data = subset_data)
  tbl = gtsummary::tbl_regression(ci_cox, exponentiate = TRUE)
  max_time = max(subset_data[[ci_time]], na.rm = TRUE)

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
    coord_cartesian(ylim = c(0, 1),
                    xlim = c(0,max_time + 10),) + 
    theme(legend.position = "bottom",
          text = element_text(size = 14)) +
    add_confidence_interval() +
    add_risktable(size = 4) +
    scale_ggsurvfit() +
    annotate ("text", hjust = 0, x = 0,
              y = 0.6,
              label = paste('HR', annotate_text),
              size = 4.5)

  return(ci_plot)
}


# survival summary table function
#' @import survival
#' @import dplyr
#' @import gt
#' @import gtsummary
#' @import tidycmprsk
ci_table_from_hct = function(subset_data, ci_status, ci_time, surv_status, surv_time,
                             group_names, ci_type) {
  
  # remove inconsistencies in data
  subset_data = subset_data %>%
    filter(!( .data[[ci_status]] == 1 & is.na(.data[[ci_time]])))
  
  # define time and status for competing risk analysis by
  # combining cumulative incidence and overall survival
  subset_data$cmprsk_time = ifelse(is.na(subset_data[[ci_time]]), 
                                   subset_data[[surv_time]], 
                                   subset_data[[ci_time]])
  
  # status: 0=censor, 1=event(e.g. engraftment), 2=death (or relapse for NRM)
  if (ci_type == "NRM") {
    subset_data$cmprsk_status = case_when(
      subset_data[[ci_status]] == 1 & subset_data[[surv_status]] == 0 ~ 1,  # NRM event
      subset_data[[surv_status]] == 1 ~ 2,  # Competing event (relapse)
      TRUE ~ 0  # Censored
    )
  } else if (ci_type %in% c('ANC engraftment', 'Plt engraftment', 'G2-4 aGvHD', 'G3-4 aGvHD')) {
    subset_data$cmprsk_status = case_when(
      subset_data[[ci_status]] == 1 ~ 1,  # Engraftment/GVHD event
      subset_data[[surv_status]] == 1 ~ 2,  # Competing event (death)
      TRUE ~ 0  # Censored
    )
  }
  
  subset_data$cmprsk_status = as.factor(subset_data$cmprsk_status)
  
  # Correctly constructing the model formula using the 'group_names' variable
  time_status_formula = reformulate(termlabels = group_names, 
                                    response = 'Surv(cmprsk_time, cmprsk_status)')
  
  # Creating models
  ci_model = cuminc(time_status_formula, data = subset_data)
  
  if (ci_type %in% c('G2-4 aGvHD', 'G3-4 aGvHD')) {
    cum_table = tbl_cuminc( #tbl_cuminc function from tidycmprsk
      ci_model,
      times = c(60, 100, 180),
      label_header = "**{time}-day aGvHD, %(95%CI)**"
    ) %>%
      modify_header(label ~ "**Group**")
  } else if (ci_type == 'NRM') {
    cum_table = tbl_cuminc( #tbl_cuminc function from tidycmprsk
      ci_model,
      times = c(30, 100, 365),
      label_header = "**{time}-day NRM, %(95%CI)**"
    ) %>%
      modify_header(label ~ "**Group**")
  }
  
  return(cum_table)
}