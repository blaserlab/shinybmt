
# ***The functions here are dataset-specific***
# ***When deploying a different dataset, variable names must be updated***

#' @import dplyr
#' @import tidyr
#' @import readxl
#' @import survival

get_bmtdata <- function(dir) {
  bmtdata = read_excel(fs::path(dir, 'final_merged_052025_upload.xlsx'))
  
}

# surv_selection = c('Please select...' = '', 'OS', 'RFS', 'GRFS')
# In this dataset
# 20: pt_status: did the pt die, c(1,0) -> alive
# 19: last_fu: data of last follow up or death, YYYY-MM-DD ->LastUpdate
# 751: tp_hct_date: Date of HCT: YYYY-MM-DD -> bmt_date

# 736: tx_relapse: did the pt relapse, c(1,0) -> rlap
# 738: tx_relapse_date: date of relaspe after this line of therapy, YYYY-MM-DD ->rl_date

# 1388: ptp_agvhd_max_grade: max overall grade of acute GVHD ->MaxAgvhd
# 1389: ptp_agvhd_max_grade_date: Date of maximum overall grade of acute GVHD ->DateMaxAGVHD
# 1539: ptp_cgvhd_tx_syst_yn: Was systemic therapy given to treat chronic GVHD? ->MaxExtentCGVHD
# 1541: ptp_cgvhd_tx_syst_date: Date Therapy was started for Chronic GVHD -> DateOnsetCGVHD


# Define the function to calculate GRFS status and GRFS time
#' @import dplyr
#' @import tidyr
#' @import readxl
calculate_grfs <- function(data) {
  data$grfs_status <- with(data, {
    # Initialize GRFS status to 0 (false)
    grfs <- rep(0, nrow(data))
    
    # Set GRFS status to 1 if any of the following conditions occur
    grfs[alive == 1] <- 1  # Patient died
    grfs[!is.na(rl_date) & rlap == 1] <- 1  # Patient relapsed
    grfs[MaxAgvhd >= 3 & !is.na(DateMaxAGVHD)] <- 1  # Severe acute GVHD
    grfs[MaxExtentCGVHD == 1 & !is.na(DateOnsetCGVHD)] <- 1  # Systemic therapy for chronic GVHD
    
    # Return the GRFS status
    grfs
  })
  
  # Calculate GRFS time 
  data$grfs_time <- with(data, {
    # Determine the earliest date of GRFS-defining event
    event_date <- pmin(LastUpdate,
                       ifelse(!is.na(rl_date) & rlap == 1, rl_date, LastUpdate),
                       ifelse(MaxAgvhd >= 3, DateMaxAGVHD, LastUpdate),
                       ifelse(MaxExtentCGVHD == 1, DateOnsetCGVHD, LastUpdate),
                       na.rm = TRUE)
    as.integer(as.Date(event_date) - as.Date(bmt_date))
  })
  return(data)
}

# a function to return the list of variables needed for surv_from_hct input
#' @import dplyr
#' @import tidyr
#' @import readxl
surv_param = function(surv_type, filtered_data) {
  
  # define and adds new variables from source dataset
  filtered_data$os_time = as.integer(as.Date(filtered_data$LastUpdate) - 
                                       as.Date(filtered_data$bmt_date))
  
  filtered_data$rfs_status = ifelse(filtered_data$alive==1 | filtered_data$rlap==1,
                                    1, 0)
  filtered_data$rfs_time = with(filtered_data, {
    days_to_LastUpdate = as.integer(as.Date(LastUpdate) - as.Date(bmt_date))
    days_to_relapse = as.integer(as.Date(rl_date) - as.Date(bmt_date))
    pmin(days_to_LastUpdate, days_to_relapse, na.rm = TRUE)
  })
  
  filtered_data = calculate_grfs(filtered_data)

  
  # create different lists for plot func input based on selection  
  if (surv_type == "OS") {
    list(
      subset_data = filtered_data, 
      surv_status = 'alive', 
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


# cum_selection = c('Please select...' = '', 'NRM, 'ANC engraftment', 'Plt engraftment', 
#                   'G2-4 aGvHD', 'G3-4 aGvHD')

# In this dataset
# 20: pt_status: did the pt die, c(1,0) ->alive
# 19: last_fu: data of last follow up or death, YYYY-MM-DD ->LastUpdate
# 751: tp_hct_date: Date of HCT: YYYY-MM-DD ->bmt_date

# 1160: ptp_g_drop_recov_yn:Did the patient recover and maintain ANC ? 500/mm3
# 1161: ptp_g_drop_recov_date: Date of ANC recovery
# 1163: ptp_plt20_yn: Was an initial platelet count > 20 x 109/L achieved?
# 1165: ptp_plt20_date: Date platelets > 20/x109/L

# 1388: ptp_agvhd_max_grade: max overall grade of acute GVHD
# 1389: ptp_agvhd_max_grade_date: Date of maximum overall grade of acute GVHD
#' @import dplyr
#' @import tidyr
#' @import readxl
cum_param = function(ci_type, filtered_data) {
  
  # define variables from source dataset
  filtered_data$os_time = as.integer(as.Date(filtered_data$LastUpdate) - 
                                       as.Date(filtered_data$bmt_date))
  filtered_data$anc_engraftment_time = as.integer(as.Date(filtered_data$ptp_g_drop_recov_date) - 
                                                    as.Date(filtered_data$bmt_date))
  filtered_data$plt_engraftment_time = as.integer(as.Date(filtered_data$ptp_plt20_date) - 
                                                    as.Date(filtered_data$bmt_date))
  filtered_data$g2_4_gvhd_status = ifelse (filtered_data$MaxAgvhd>1, 1, 0)
  filtered_data$g2_4_gvhd_time = ifelse (filtered_data$MaxAgvhd>1, 
                                         as.integer(as.Date(filtered_data$DateMaxAGVHD) - 
                                                      as.Date(filtered_data$bmt_date)), 
                                         NA)
  filtered_data$g3_4_gvhd_status = ifelse (filtered_data$MaxAgvhd>2, 1, 0)
  filtered_data$g3_4_gvhd_time = ifelse (filtered_data$MaxAgvhd>2, 
                                         as.integer(as.Date(filtered_data$DateMaxAGVHD) - 
                                                      as.Date(filtered_data$bmt_date)), 
                                         NA)

  
  filtered_data$rel_time = as.integer(as.Date(filtered_data$rl_date) - 
                                        as.Date(filtered_data$bmt_date))
  
  
  # create different lists for plot func input based on selection  
  if (ci_type == "ANC engraftment") {
    list(
      subset_data = filtered_data, 
      ci_status = 'ptp_g_drop_recov_yn',
      ci_time = 'anc_engraftment_time',
      surv_status = 'alive', 
      surv_time = 'os_time', 
      ci_type = ci_type, 
      group_names = 'group'
    )
  } else if (ci_type == 'Plt engraftment') {
    list(
      subset_data = filtered_data, 
      ci_status = 'ptp_plt20_yn',
      ci_time = 'plt_engraftment_time',
      surv_status = 'alive', 
      surv_time = 'os_time', 
      ci_type = ci_type, 
      group_names = 'group'
    )
  } else if (ci_type == 'G2-4 aGvHD') {
    list(
      subset_data = filtered_data, 
      ci_status = 'g2_4_gvhd_status',
      ci_time = 'g2_4_gvhd_time',
      surv_status = 'alive', 
      surv_time = 'os_time', 
      ci_type = ci_type, 
      group_names = 'group'
    )
  } else if (ci_type == 'G3-4 aGvHD') {
    list(
      subset_data = filtered_data, 
      ci_status = 'g3_4_gvhd_status',
      ci_time = 'g3_4_gvhd_time',
      surv_status = 'alive', 
      surv_time = 'os_time', 
      ci_type = ci_type, 
      group_names = 'group'
    )
  } else if (ci_type == 'NRM') {
    list(
      subset_data = filtered_data, 
      ci_status = 'alive',
      ci_time = 'os_time',
      surv_status = 'rlap', 
      surv_time = 'rel_time', 
      ci_type = ci_type, 
      group_names = 'group'
    )
  } 
}


# A dictionary of mapping variable name to variable labels in the dataset
# work for a single element of a vector
#' @import dplyr
#' @import tidyr
#' @import readxl
map_variable_name = function(input) {
  # Define the mapping between the inputs and the corresponding tbl_variable items
  mapping = list(
    'Age' = 'new_age',
    'Sex' = 'Gn',
    'Race' = 'race',
    'Diagnosis' = 'dx',
    'Prep type' = 'tx_type',
    'Donor type' = 'donor',
    'Disease status' = 'RemSta',
    'Age (>=65)' = 'age_group_65',
    'HCT-CI (>=3)' = 'hct_ci_3',
    'Group' = 'group'
  )
  
  # Helper function to map individual elements
  map_single_element = function(element) {
    if (element %in% names(mapping)) {
      return(mapping[[element]])
    } else {
      stop(paste("The mapping for the label '", element, "' does not exist", sep = ""))
    }
  }
  
  # Check if the input is a vector
  if (is.vector(input)) {
    return(sapply(input, map_single_element, USE.NAMES = FALSE))
  } else {
    return(map_single_element(input))
  }
}
