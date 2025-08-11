#!/usr/bin/env Rscript

.libPaths(c("/workspace/rst/cache_4_5/user_project/wan354/shinybmt",
  "/usr/local/lib/R/site-library",
  "/usr/local/stow/R-4.5.0/lib/R/library"))

source("/workspace/jason_workspace/shinybmt/R/baseline_tbl.R")
source("/workspace/jason_workspace/shinybmt/R/data_pp.R")
source("/workspace/jason_workspace/shinybmt/R/dict_pp.R")
source("/workspace/jason_workspace/shinybmt/R/reg.R")
source("/workspace/jason_workspace/shinybmt/R/surv.R")
source("/workspace/jason_workspace/shinybmt/R/app.R")

shinyBMT(data_dir = "/workspace/jason_workspace/shinybmt_data/",
                   shiny_host = "10.221.152.46",
                   shiny_port = 3275)