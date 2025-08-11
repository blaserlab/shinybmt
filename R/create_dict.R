library(dplyr)
library(tidyr)
library(readxl)
library(survival)
library(lubridate)
library(writexl)

build_and_write_dict <- function(
    data_dir = "/workspace/jason_workspace/shinybmt_data/",
    max_levels = Inf
) {
  stopifnot(dir.exists(data_dir))
  
  # ---- locate and load dataset ----
  if (is.null(dataset_path)) {
    # look for merged_2025*.[xlsx|csv|rds]
    cand <- list.files(data_dir, pattern = "^final_merged_.*\\.(xlsx|csv|rds)$", full.names = TRUE, ignore.case = TRUE)
    if (length(cand) == 0) stop("No dataset matching '^final_merged_.*.(xlsx|csv|rds)$' found in: ", data_dir)
    dataset_path <- cand[[1]]
  }
  ext <- tools::file_ext(dataset_path)
  df <- switch(
    tolower(ext),
    "xlsx" = readxl::read_xlsx(dataset_path),
    "csv"  = read.csv(dataset_path, check.names = FALSE),
    "rds"  = readRDS(dataset_path),
    stop("Unsupported file extension: ", ext)
  )
  
  # ---- your std pipeline ----
  std <- df %>%
    dplyr::mutate(
      dx = dplyr::case_when(
        !is.na(Diagnosis_x) & grepl("-", Diagnosis_x) ~ sub("-.*", "", Diagnosis_x),
        TRUE ~ Diagnosis_x
      ),
      dx = dplyr::case_when(
        !is.na(dx) & grepl(",", dx) ~ sub(",.*", "", dx),
        TRUE ~ dx
      ),
      dx = trimws(dx)
    ) %>%
    dplyr::mutate(
      AB = dplyr::case_when(
        AB == "ric" ~ "RIC",
        AB == "abl" ~ "MAC",
        TRUE        ~ "RIC"
      ),
      Tx_Type = dplyr::case_when(
        Tx_Type == "MUD" ~ "MUD/MMUD",
        TRUE ~ Tx_Type
      )
    ) %>%
    dplyr::mutate(
      new_age = as.integer(age_at_bmt),
      hct_ci  = CMI,
      tx_type = Tx_Type,
      donor   = donRel,
      RemSta  = RemSt
    )
  
  # ---- helpers ----
  is_categorical <- function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(FALSE)
    is.factor(x) || is.character(x) || is.logical(x)
  }
  
  build_dict <- function(std, max_levels = Inf) {
    stopifnot(is.data.frame(std))
    vars <- names(std)
    out <- lapply(vars, function(v) {
      x <- std[[v]]
      if (!is_categorical(x)) return(NULL)
      lv <- unique(x)
      lv <- lv[!is.na(lv)]
      lv <- as.character(lv)
      lv <- trimws(lv)
      lv <- lv[lv != ""]
      if (length(lv) == 0) return(NULL)
      lv <- sort(lv, na.last = TRUE)
      if (is.finite(max_levels) && length(lv) > max_levels) lv <- lv[seq_len(max_levels)]
      attrs <- paste(sprintf('"%s"', lv), collapse = ", ")
      data.frame(variable = v, attributes_name = attrs, stringsAsFactors = FALSE)
    })
    dplyr::bind_rows(out)
  }
  
  # ---- labels ----
  label_to_var <- c(
    'Age'                  = 'new_age',
    'Sex'                  = 'Gn',
    'Race'                 = 'race',
    'Diagnosis'            = 'dx',
    'Donor type'           = 'tx_type',
    'HLA match'            = 'HLA',
    'Stem cell source'     = 'Source_x',
    'Conditioning regimen' = 'Prep',
    'Conditioning type'    = 'AB',
    'GVHD ppx'             = 'gvhdpr',
    'Disease status'       = 'RemSta',
    'Donor relationship'   = 'donor',
    'Donor ABO'            = 'donabo',
    'Donor CMV'            = 'doncmv',
    'Cause of death'       = 'COD',
    'Donor age'            = 'donage',
    'Ethnicity'            = 'eth',
    'Detailed diagnosis'   = 'Diagnosis_x',
    'Donor gender'         = 'donRn'
  )
  var_to_label <- setNames(names(label_to_var), label_to_var)
  
  dict <- build_dict(std, max_levels = max_levels) %>%
    dplyr::mutate(
      label = dplyr::if_else(variable %in% names(var_to_label),
                             var_to_label[variable],
                             variable)
    )
  
  # ---- write and return ----
  out_path <- file.path(data_dir, "dict.xlsx")
  writexl::write_xlsx(dict, out_path)
  message("Wrote dictionary to: ", out_path)
  invisible(dict)
}
