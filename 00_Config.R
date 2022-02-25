# Set working directory
setwd("C:\\Users\\Nick\\OneDrive\\tez-tax")

# Load functions
source("functions/load_packages.R")
source("functions/list_check.R")
source("functions/quick_case.R")
source("functions/tzkt_api.R")

# Load packages
load_packages(c("tidyverse", "jsonlite", "magrittr", "readr"))

# Create data folder, if necessary
dir.create("data", showWarnings=FALSE)

# Define wallet addresses
addresses <- c(
  "tz1a2ZeWmyNQ8BiuFNTE4vmFEP9MBaP76QPX", # datcsv
  "tz1L5vaycmTzEDekjDJSFZJ1V8FPwAUCVSDM", # datcsv1
  "tz1Sbt4C1MXm1AWPK6qdgfDzciJCCUVADmUt", # datcsv2
  "tz2TXkQS6kPPXbuVMsNBcdtpjWf9DPYWHfqV"  # gmail
)

# Define currency
currency <- "usd"

# Define date span [min, max]
date_span <- c("2021-01-01T00:00:00Z", "2021-12-31T23:59:59Z")

# Include Coinbase transaction data?
cb_tx <- TRUE

# Path to Coinbase transaction data
cb_data <- "data/cb_transactions.csv"

# Save data
save(addresses, file="data/addresses.RData")
save(currency, file="data/currency.RData")
save(date_span, file="data/date_span.RData")
save(cb_tx, file="data/cb_tx.RData")
save(cb_data, file="data/cb_data.RData")