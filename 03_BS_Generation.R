# Clear workspace
rm(list=ls())

# Load functions
source("functions/load_packages.R")
source("functions/list_check.R")
source("functions/quick_case.R")
source("functions/tzkt_api.R")

# Load packages
load_packages(c("tidyverse", "jsonlite", "magrittr", "readr"))

# Load data
load(file="data/addresses.RData")
load(file="data/currency.RData")
load(file="data/date_span.RData")
load(file="data/cb_tx.RData")
load(file="data/cb_data.RData")
load(file="data/operations.RData")
load(file="data/is.RData")

# 
