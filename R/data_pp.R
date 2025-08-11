# --- packages ---
library(dplyr)
library(tidyr)
library(readxl)
library(survival)
library(lubridate)

# =============================================================================
# 1) Loader + standardizer: read the *new* sheet and create the old field names
# =============================================================================
# Expecting the new dataset columns (sample provided):
# BMT_date, alivex, alive, survival_d, rlap, DayRlp, NtEng, PltEng,
# MaxAgvhd, AGVHDONSET, MaxExtentCGVHD, CGVHDONSET, MaxSeverityCGVHD, ...
# plus baseline vars used in map_variable_name

get_bmtdata <- function(dir) {
  raw = read_excel(fs::path(dir, 'final_merged_052025_upload.xlsx'))
  
  # Normalize date columns first
  # BMT_date may come as POSIXct-like text; coerce to Date
  raw <- raw %>%
    mutate(
      bmt_date = as.Date(BMT_date),
      # survival_d = days to last follow-up or death
      LastUpdate = if_else(!is.na(survival_d),
                           bmt_date + days(as.integer(survival_d)),
                           bmt_date),
      # relapse date: DayRlp is days from BMT; build a Date if present
      rl_date = if_else(!is.na(DayRlp) & DayRlp != "",
                        bmt_date + days(as.integer(DayRlp)),
                        as.Date(NA)),
      # acute GVHD date: AGVHDONSET is days from BMT in your example
      DateMaxAGVHD = if_else(!is.na(AGVHDONSET) & AGVHDONSET != "",
                             bmt_date + days(as.integer(AGVHDONSET)),
                             as.Date(NA)),
      # chronic GVHD systemic-therapy proxy:
      #   You don't have a direct "systemic therapy yes/no".
      #   Common GRFS approximations: cGVHD requiring systemic IS ≈
      #   (moderate/severe) or "EXTENSIVE". We mark 1 if either holds.
      MaxExtentCGVHD_flag = case_when(
        !is.na(MaxSeverityCGVHD) & MaxSeverityCGVHD %in% c("MODERATE", "SEVERE") ~ 1L,
        !is.na(MaxExtentCGVHD) & toupper(trimws(MaxExtentCGVHD)) == "EXTENSIVE" ~ 1L,
        TRUE ~ 0L
      ),
      DateOnsetCGVHD = if_else(!is.na(CGVHDONSET) & CGVHDONSET != "",
                               bmt_date + days(as.integer(CGVHDONSET)),
                               as.Date(NA))
    )
  raw = raw %>%
    mutate(
      alive = case_when(
        alivex == "N" ~ 1,
        TRUE ~ 0
      ),
      rlap = case_when(
        rlap == "Y" ~ 1,
        TRUE ~ 0
      )
    )
  
  # Standardize the exact old variable names used by your analysis code
  std <- raw %>%
    mutate(
      # OS event: your old code expects alive==1 meaning *death occurred*
      alive = as.integer(alive),        # already coded 1=dead, 0=alive in your sample
      rlap  = as.integer(rlap),         # relapse flag 1/0
      MaxAgvhd = as.integer(MaxAgvhd),
      MaxExtentCGVHD = MaxExtentCGVHD_flag,
      
      # Engraftment: new data give days; create yes/no + time to event used in cum_param
      ptp_g_drop_recov_yn = as.integer(!is.na(NtEng) & NtEng != ""),
      ptp_g_drop_recov_date = if_else(!is.na(NtEng) & NtEng != "",
                                      bmt_date + days(as.integer(NtEng)),
                                      as.Date(NA)),
      ptp_plt20_yn = as.integer(!is.na(PltEng) & PltEng != ""),
      ptp_plt20_date = if_else(!is.na(PltEng) & PltEng != "",
                               bmt_date + days(as.integer(PltEng)),
                               as.Date(NA))
    )
  
  # create a new column "dx", which contains "Disease_x" string before "-"
  std <- std %>%
    mutate(
      # Step 1: remove everything after "-" (if present)
      dx = ifelse(
        !is.na(Disease_x) & grepl("-", Disease_x),
        sub("-.*", "", Disease_x),
        Disease_x
      ),
      # Step 2: remove everything after "," (if present)
      dx = ifelse(
        !is.na(dx) & grepl(",", dx),
        sub(",.*", "", dx),
        dx
      ),
      # Step 3: trim spaces
      dx = trimws(dx)
    )
  
  # filter out some ineligible values in some columnes
  std <- std %>%
    mutate(
      AB = case_when(
        AB == "ric" ~ "RIC",
        AB == "abl" ~ "MAC",
        TRUE        ~ "RIC"
      ),
      Tx_Type = case_when(
        Tx_Type == "MUD" ~ "MUD/MMUD",
        TRUE ~ Tx_Type
      )
    )
  
  
  
  # Also preserve commonly used baseline variables so map_variable_name works
  std <- std %>%
    mutate(
      new_age = as.integer(age_at_bmt),
      hct_ci = CMI,
      tx_type = Tx_Type,
      donor = donRel,
      RemSta = RemSt,
    )
  
  std <- std %>%
    mutate(
      # approximate DOB so existing helpers keep working
      dob = as.Date(bmt_date) - lubridate::years(as.integer(age_at_bmt))
    )
  
  std
}

# =============================================================================
# 2) GRFS calculator (unchanged, but now uses standardized variables)
# =============================================================================
calculate_grfs <- function(data) {
  data$grfs_status <- with(data, {
    grfs <- rep(0, nrow(data))
    grfs[alive == 1] <- 1                               # death
    grfs[!is.na(rl_date) & rlap == 1] <- 1              # relapse
    grfs[MaxAgvhd >= 3 & !is.na(DateMaxAGVHD)] <- 1     # grade III–IV aGVHD
    grfs[MaxExtentCGVHD == 1 & !is.na(DateOnsetCGVHD)] <- 1  # systemic IS proxy for cGVHD
    grfs
  })
  
  data$grfs_time <- with(data, {
    event_date <- pmin(LastUpdate,
                       ifelse(!is.na(rl_date) & rlap == 1, rl_date, LastUpdate),
                       ifelse(MaxAgvhd >= 3, DateMaxAGVHD, LastUpdate),
                       ifelse(MaxExtentCGVHD == 1, DateOnsetCGVHD, LastUpdate),
                       na.rm = TRUE)
    as.integer(as.Date(event_date) - as.Date(bmt_date))
  })
  data
}

# =============================================================================
# 3) Survival params (minor change: make sure LastUpdate/rl_date exist)
# =============================================================================
surv_param <- function(surv_type, filtered_data) {
  
  filtered_data$os_time <- as.integer(as.Date(filtered_data$LastUpdate) -
                                        as.Date(filtered_data$bmt_date))
  
  filtered_data$rfs_status <- ifelse(filtered_data$alive == 1 | filtered_data$rlap == 1, 1, 0)
  
  filtered_data$rfs_time <- with(filtered_data, {
    days_to_LastUpdate <- as.integer(as.Date(LastUpdate) - as.Date(bmt_date))
    days_to_relapse    <- as.integer(as.Date(rl_date) - as.Date(bmt_date))
    pmin(days_to_LastUpdate, days_to_relapse, na.rm = TRUE)
  })
  
  filtered_data <- calculate_grfs(filtered_data)
  
  if (surv_type == "OS") {
    list(subset_data = filtered_data, surv_status = 'alive',    surv_time = 'os_time',
         surv_type = surv_type, group_names = 'group')
  } else if (surv_type == "RFS") {
    list(subset_data = filtered_data, surv_status = 'rfs_status', surv_time = 'rfs_time',
         surv_type = surv_type, group_names = 'group')
  } else if (surv_type == "GRFS") {
    list(subset_data = filtered_data, surv_status = 'grfs_status', surv_time = 'grfs_time',
         surv_type = surv_type, group_names = 'group')
  }
}

# =============================================================================
# 4) Cumulative incidence params (adapted to NtEng/PltEng days)
# =============================================================================
cum_param <- function(ci_type, filtered_data) {
  
  filtered_data$os_time <- as.integer(as.Date(filtered_data$LastUpdate) -
                                        as.Date(filtered_data$bmt_date))
  
  filtered_data$anc_engraftment_time <- as.integer(as.Date(filtered_data$ptp_g_drop_recov_date) -
                                                     as.Date(filtered_data$bmt_date))
  filtered_data$plt_engraftment_time <- as.integer(as.Date(filtered_data$ptp_plt20_date) -
                                                     as.Date(filtered_data$bmt_date))
  
  filtered_data$g2_4_gvhd_status <- ifelse(filtered_data$MaxAgvhd > 1, 1, 0)
  filtered_data$g2_4_gvhd_time   <- ifelse(filtered_data$MaxAgvhd > 1,
                                           as.integer(as.Date(filtered_data$DateMaxAGVHD) -
                                                        as.Date(filtered_data$bmt_date)),
                                           NA_integer_)
  filtered_data$g3_4_gvhd_status <- ifelse(filtered_data$MaxAgvhd > 2, 1, 0)
  filtered_data$g3_4_gvhd_time   <- ifelse(filtered_data$MaxAgvhd > 2,
                                           as.integer(as.Date(filtered_data$DateMaxAGVHD) -
                                                        as.Date(filtered_data$bmt_date)),
                                           NA_integer_)
  
  filtered_data$rel_time <- as.integer(as.Date(filtered_data$rl_date) -
                                         as.Date(filtered_data$bmt_date))
  
  if (ci_type == "ANC engraftment") {
    list(subset_data = filtered_data, ci_status = 'ptp_g_drop_recov_yn',
         ci_time = 'anc_engraftment_time', surv_status = 'alive', surv_time = 'os_time',
         ci_type = ci_type, group_names = 'group')
  } else if (ci_type == "Plt engraftment") {
    list(subset_data = filtered_data, ci_status = 'ptp_plt20_yn',
         ci_time = 'plt_engraftment_time', surv_status = 'alive', surv_time = 'os_time',
         ci_type = ci_type, group_names = 'group')
  } else if (ci_type == "G2-4 aGvHD") {
    list(subset_data = filtered_data, ci_status = 'g2_4_gvhd_status',
         ci_time = 'g2_4_gvhd_time', surv_status = 'alive', surv_time = 'os_time',
         ci_type = ci_type, group_names = 'group')
  } else if (ci_type == "G3-4 aGvHD") {
    list(subset_data = filtered_data, ci_status = 'g3_4_gvhd_status',
         ci_time = 'g3_4_gvhd_time', surv_status = 'alive', surv_time = 'os_time',
         ci_type = ci_type, group_names = 'group')
  } else if (ci_type == "NRM") {
    list(subset_data = filtered_data, ci_status = 'alive', ci_time = 'os_time',
         surv_status = 'rlap', surv_time = 'rel_time',
         ci_type = ci_type, group_names = 'group')
  }
}

# =============================================================================
# 5) Variable label mapper for the new dataset
# =============================================================================
map_variable_name <- function(input) {
  mapping <- list(
    'Age' = 'new_age',
    'Sex' = 'Gn',
    'Race' = 'race',
    'Ethnicity' = 'eth',
    'Diagnosis' = 'dx',
    'Donor type' = 'tx_type',
    'HLA match' = 'HLA',
    'Stem cell source' = 'Source_x',
    'Detailed diagnosis' = 'Diagnosis_x',
    'Conditioning regimen' = 'Prep',
    'Conditioning type' = 'AB',
    'GVHD ppx' = 'gvhdpr',
    'Disease status' = 'RemSta',
    'Donor gender' = 'donRn',
    'Donor relationship' = 'donor',
    'Donor ABO' = 'donabo',
    'Donor CMV' = 'doncmv',
    'Cause of death' = 'COD',
    'Detailed disease type' = 'Disease_x',
    'Age (>=65)' = 'age_group_65',
    'HCT-CI (>=3)' = 'hct_ci_3',
    'Group' = 'group'
  )
  map_single <- function(x) {
    if (x %in% names(mapping)) mapping[[x]] else
      stop(sprintf("No mapping for label '%s'", x))
  }
  if (is.vector(input)) sapply(input, map_single, USE.NAMES = FALSE) else map_single(input)
}
