#' @import dplyr
#' @import tidyr
#' @import readxl
# get_dict <- function(dir) {
#   # pre-process data dictionary
#   
#   dict = read_excel(fs::path(dir, "dict.xlsx"))
#   dict = dict %>%
#     rename ('num' = '#') %>%
#     rename ('variable' = 'Variable / Field Name') %>%
#     rename ('label' = 'Field Label') %>%
#     rename ('attributes' = 'Field Attributes (Field Type, Validation, Choices, Calculations, etc.)') %>%
#     unite ('attributes_name', c('...5', '...6'), sep = ' ') %>%
#     fill (num, .direction = 'down')
#   
#   # create an instrument column
#   # Create a new column that identifies rows containing 'instrument' names
#   dict$instrument = ifelse(grepl('Instrument:', dict$num), dict$num, NA)
#   
#   # Fill down NA values with the last observed non-NA value
#   dict = dict %>% tidyr::fill(instrument, .direction = 'down')
#   # change e.g. "Instrument:Identification(identification)" to "identification"
#   dict$instrument = sub(".*\\((.*)\\).*", "\\1", dict$instrument)
#   
#   # print (unique(dict$instrument))
#   # [1] "identification"             "status"                     "comorbidities"
#   # [4] "diagnosis"                  "venous_thromboembolism_vte" "cardiovascular_events_cve"
#   # [7] "laboratory_follow_up"       "treatment"                  "transplant_pre"
#   # [10] "transplant_post"
#   
#   # remove instrument "comorbidities", "venous_thromboembolism_vte"
#   # "cardiovascular_events_cve", "laboratory_follow_up" for simplification
#   values_to_remove = c(
#     "comorbidities",
#     "venous_thromboembolism_vte",
#     "cardiovascular_events_cve",
#     "laboratory_follow_up"
#   )
#   dict = dict %>%
#     filter (!instrument %in% values_to_remove)
#   
#   # attributes_name: get rid of NA, NA NA, complete
#   dict$attributes_name[dict$attributes_name == 'NA NA'] = NA
#   dict$attributes_name = gsub (' NA', '', dict$attributes_name)
#   
#   # attributes: get rid of non-int
#   dict$attributes = as.numeric(dict$attributes)
#   
#   # label: for each unique num, consolidate all labels and fill all the rows
#   # of that same num
#   dict$num = as.integer(dict$num)
#   dict = dict %>%
#     group_by(num) %>%
#     # Create a single string for 'label' by concatenating all non-NA 'label' entries
#     mutate(label = paste(na.omit(label), collapse = " ")) %>%
#     ungroup()
#   
#   # variable: for each unique num, only keep the 1st row of variable and then fill
#   dict = dict %>%
#     group_by(num) %>%
#     mutate(variable = first(variable)) %>%
#     ungroup()
#   
#   # final clean-up
#   # remove the first row where num appears two or more times
#   dict = dict %>%
#     distinct() %>%
#     filter(!is.na(num)) %>%
#     group_by(num) %>%
#     mutate(count = n()) %>%
#     filter(!(count >= 2 & row_number() == 1)) %>%
#     select(-count) %>%
#     filter (!grepl('complete', attributes_name, ignore.case = T)) %>%
#     filter (!grepl('verified', attributes_name, ignore.case = T))
#   
#   # condense attributes_names
#   dict = dict %>%
#     group_by(num, variable, label, instrument) %>%
#     summarise(
#       attributes = list(attributes),
#       attributes_name = list(attributes_name),
#       .groups = 'drop'
#     )
#   dict$variable = gsub("\\[|\\]", "", dict$variable)
#   
#   # jsonlite::write_json(condensed_dict, path = "data/dict.json", pretty = F)
#   dict
#   
# }


get_dict = function (dir) {
  dict = read_excel(fs::path(dir, "joe_data_dict.xlsx"))
  
  dict$attributes_name = strsplit(dict$attributes_name, ", ")
  
  dict$attributes_name = lapply(dict$attributes_name, function(x) gsub('"', '', x))
  
  return(dict)
}