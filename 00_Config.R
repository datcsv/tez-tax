
# Import functions
source("functions/list_check.R")
source("functions/load_packages.R")
source("functions/quick_case.R")
source("functions/tzkt_api.R")

# Import libraries
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

# Path to Coinbase transaction data, if applicable (Otherwise set to NA)
cb_data <- "data/cb_transactions.csv"

# Run next steps in process
#source("01_Operations_Data.R")
#source("02_IS_Generation.R")
#source("03_BS_Generation.R")
