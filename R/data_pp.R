library (dplyr)
library (tidyr)
library (readxl)

# ***The functions here are dataset-specific***
# ***When deploying a different dataset, variable names must be updated***

bmtdata = read_excel(here::here('data/bmtdata.xlsx'))

# surv_selection = c('Please select...' = '', 'OS', 'RFS', 'GRFS')
# In this dataset
# 20: pt_status: did the pt die, c(1,0)
# 19: last_fu: data of last follow up or death, YYYY-MM-DD
# 751: tp_hct_date: Date of HCT: YYYY-MM-DD

# 736: tx_relapse: did the pt relapse, c(1,0)
# 738: tx_relapse_date: date of relaspe after this line of therapy, YYYY-MM-DD

# 1388: ptp_agvhd_max_grade: max overall grade of acute GVHD
# 1389: ptp_agvhd_max_grade_date: Date of maximum overall grade of acute GVHD
# 1539: ptp_cgvhd_tx_syst_yn: Was systemic therapy given to treat chronic GVHD?
# 1541: ptp_cgvhd_tx_syst_date: Date Therapy was started for Chronic GVHD


# Define the function to calculate GRFS status and GRFS time
calculate_grfs <- function(data) {
  data$grfs_status <- with(data, {
    # Initialize GRFS status to 0 (false)
    grfs <- rep(0, nrow(data))
    
    # Set GRFS status to 1 if any of the following conditions occur
    grfs[pt_status == 1] <- 1  # Patient died
    grfs[!is.na(tx_relapse_date) & tx_relapse == 1] <- 1  # Patient relapsed
    grfs[ptp_agvhd_max_grade >= 3 & !is.na(ptp_agvhd_max_grade_date)] <- 1  # Severe acute GVHD
    grfs[ptp_cgvhd_tx_syst_yn == 1 & !is.na(ptp_cgvhd_tx_syst_date)] <- 1  # Systemic therapy for chronic GVHD
    
    # Return the GRFS status
    grfs
  })
  
  # Calculate GRFS time 
  data$grfs_time <- with(data, {
    # Determine the earliest date of GRFS-defining event
    event_date <- pmin(last_fu,
                       ifelse(!is.na(tx_relapse_date) & tx_relapse == 1, tx_relapse_date, last_fu),
                       ifelse(ptp_agvhd_max_grade >= 3, ptp_agvhd_max_grade_date, last_fu),
                       ifelse(ptp_cgvhd_tx_syst_yn == 1, ptp_cgvhd_tx_syst_date, last_fu),
                       na.rm = TRUE)
    as.integer(as.Date(event_date) - as.Date(tp_hct_date))
  })
  return(data)
}

# a function to return the list of variables needed for surv_from_hct input
surv_param = function(surv_type, filtered_data) {
  
  # define variables from source dataset
  filtered_data$os_time = as.integer(as.Date(filtered_data$last_fu) - 
                                       as.Date(filtered_data$tp_hct_date))
  filtered_data$rfs_status = ifelse(filtered_data$pt_status==1 | filtered_data$tx_relapse==1,
                                    1, 0)
  filtered_data$rfs_time = with(filtered_data, {
    days_to_last_fu = as.integer(as.Date(last_fu) - as.Date(tp_hct_date))
    days_to_relapse = as.integer(as.Date(tx_relapse_date) - as.Date(tp_hct_date))
    pmin(days_to_last_fu, days_to_relapse, na.rm = TRUE)
  })
  
  filtered_data = calculate_grfs(filtered_data)
  
  # create different lists for plot func input based on selection  
  if (surv_type == "OS") {
    list(
      subset_data = filtered_data, 
      surv_status = 'pt_status', 
      surv_time = 'os_time', 
      surv_type = surv_type, 
      group_names = 'group'
    )
  } else if (surv_type == 'RFS') {
    list(
      subset_data = filtered_data, 
      surv_status = 'rfs_status', 
      surv_time = 'rfs_time', 
      surv_type = surv_type, 
      group_names = 'group'
    )
  } else if (surv_type == 'GRFS') {
    list(
      subset_data = filtered_data, 
      surv_status = 'grfs_status', 
      surv_time = 'grfs_time', 
      surv_type = surv_type, 
      group_names = 'group'
    )
  }
}


# cum_selection = c('Please select...' = '', 'ANC engraftment', 'Plt engraftment', 
#                   'G2-4 aGvHD', 'G3-4 aGvHD')

# In this dataset
# 20: pt_status: did the pt die, c(1,0)
# 19: last_fu: data of last follow up or death, YYYY-MM-DD
# 751: tp_hct_date: Date of HCT: YYYY-MM-DD

# 1160: ptp_g_drop_recov_yn:Did the patient recover and maintain ANC ? 500/mm3
# 1161: ptp_g_drop_recov_date: Date of ANC recovery
# 1163: ptp_plt20_yn: Was an initial platelet count > 20 x 109/L achieved?
# 1165: ptp_plt20_date: Date platelets > 20/x109/L

# 1388: ptp_agvhd_max_grade: max overall grade of acute GVHD
# 1389: ptp_agvhd_max_grade_date: Date of maximum overall grade of acute GVHD

cum_param = function(ci_type, filtered_data) {
  
  # define variables from source dataset
  filtered_data$os_time = as.integer(as.Date(filtered_data$last_fu) - 
                                       as.Date(filtered_data$tp_hct_date))
  filtered_data$anc_engraftment_time = as.integer(as.Date(filtered_data$ptp_g_drop_recov_date) - 
                                                    as.Date(filtered_data$tp_hct_date))
  filtered_data$plt_engraftment_time = as.integer(as.Date(filtered_data$ptp_plt20_date) - 
                                                    as.Date(filtered_data$tp_hct_date))
  filtered_data$g2_4_gvhd_status = ifelse (filtered_data$ptp_agvhd_max_grade>1, 1, 0)
  filtered_data$g2_4_gvhd_time = ifelse (filtered_data$ptp_agvhd_max_grade>1, 
                                         as.integer(as.Date(filtered_data$ptp_agvhd_max_grade_date) - 
                                                      as.Date(filtered_data$tp_hct_date)), 
                                         NA)
  filtered_data$g3_4_gvhd_status = ifelse (filtered_data$ptp_agvhd_max_grade>2, 1, 0)
  filtered_data$g3_4_gvhd_time = ifelse (filtered_data$ptp_agvhd_max_grade>2, 
                                         as.integer(as.Date(filtered_data$ptp_agvhd_max_grade_date) - 
                                                      as.Date(filtered_data$tp_hct_date)), 
                                         NA)
  
  # create different lists for plot func input based on selection  
  if (ci_type == "ANC engraftment") {
    list(
      subset_data = filtered_data, 
      ci_status = 'ptp_g_drop_recov_yn',
      ci_time = 'anc_engraftment_time',
      surv_status = 'pt_status', 
      surv_time = 'os_time', 
      ci_type = ci_type, 
      group_names = 'group'
    )
  } else if (ci_type == 'Plt engraftment') {
    list(
      subset_data = filtered_data, 
      ci_status = 'ptp_plt20_yn',
      ci_time = 'plt_engraftment_time',
      surv_status = 'pt_status', 
      surv_time = 'os_time', 
      ci_type = ci_type, 
      group_names = 'group'
    )
  } else if (ci_type == 'G2-4 aGvHD') {
    list(
      subset_data = filtered_data, 
      ci_status = 'g2_4_gvhd_status',
      ci_time = 'g2_4_gvhd_time',
      surv_status = 'pt_status', 
      surv_time = 'os_time', 
      ci_type = ci_type, 
      group_names = 'group'
    )
  } else if (ci_type == 'G3-4 aGvHD') {
    list(
      subset_data = filtered_data, 
      ci_status = 'g3_4_gvhd_status',
      ci_time = 'g3_4_gvhd_time',
      surv_status = 'pt_status', 
      surv_time = 'os_time', 
      ci_type = ci_type, 
      group_names = 'group'
    )
  } 
}
