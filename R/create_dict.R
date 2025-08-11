library(dplyr)
library(tidyr)
library(readxl)
library(survival)
library(lubridate)
library(writexl)


std <- final_merged_052025_upload %>%
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

is_categorical <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(FALSE)
  is.factor(x) || is.character(x) || is.logical(x)
}

# ---- main: build dict from `std` (assumes `std` exists) ----
build_dict <- function(std, max_levels = Inf) {
  stopifnot(is.data.frame(std))
  
  vars <- names(std)
  out <- lapply(vars, function(v) {
    x <- std[[v]]
    if (!is_categorical(x)) return(NULL)
    
    # levels: unique, drop NA/blank, as character, sorted
    lv <- unique(x)
    lv <- lv[!is.na(lv)]
    lv <- as.character(lv)
    lv <- trimws(lv)
    lv <- lv[lv != ""]
    if (length(lv) == 0) return(NULL)
    
    lv <- sort(lv, na.last = TRUE)
    if (is.finite(max_levels) && length(lv) > max_levels) {
      lv <- lv[seq_len(max_levels)]
    }
    
    # attributes_name: "a", "b", "c" (quoted, comma-separated)
    attrs <- paste(sprintf('"%s"', lv), collapse = ", ")
    
    data.frame(
      variable = v,
      attributes_name = attrs,
      stringsAsFactors = FALSE
    )
  })
  
  dict <- bind_rows(out)
  dict
}

# create label column

# Mapping from label -> variable name
label_to_var <- c(
  'Age'                 = 'new_age',
  'Sex'                 = 'Gn',
  'Race'                = 'race',
  'Diagnosis'           = 'dx',
  'Donor type'          = 'tx_type',
  'HLA match'           = 'HLA',
  'Stem cell source'    = 'Source_x',
  'Conditioning regimen'= 'Prep',
  'Conditioning type'   = 'AB',
  'GVHD ppx'            = 'gvhdpr',
  'Disease status'      = 'RemSta',
  'Donor relationship'  = 'donor',
  'Donor ABO'           = 'donabo',
  'Donor CMV'           = 'doncmv',
  'Cause of death'      = 'COD',
  'Donor age'           = 'donage',
  'Ethnicity'           = 'eth',
  'Detailed diagnosis' = 'Diagnosis_x',
  'Donor gender' = 'donRn'
)

# Reverse mapping: variable -> label
var_to_label <- setNames(names(label_to_var), label_to_var)

# Build dictionary as before
dict <- build_dict(std)

# Add label column
dict <- dict %>%
  mutate(
    label = ifelse(variable %in% names(var_to_label),
                   var_to_label[variable],
                   variable)  # keep original name if no mapping
  )

write_xlsx(dict, "/workspace/jason_workspace/shinybmt_data/dict.xlsx")

