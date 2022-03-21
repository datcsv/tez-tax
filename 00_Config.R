# Import packages
library("tidyverse")
library("jsonlite")
library("lubridate")
library("magrittr")
library("readr")
library("staplr")

# Load functions
source("functions/list_check.R")
source("functions/quick_case.R")
source("functions/tzkt_api.R")

# Create data folder, if applicable
dir.create("data", showWarnings=FALSE)

# Define wallet addresses
wallets <- c(
  "tz1a2ZeWmyNQ8BiuFNTE4vmFEP9MBaP76QPX", # datcsv
  "tz1L5vaycmTzEDekjDJSFZJ1V8FPwAUCVSDM", # datcsv1
  "tz1Sbt4C1MXm1AWPK6qdgfDzciJCCUVADmUt", # datcsv2
  "tz2TXkQS6kPPXbuVMsNBcdtpjWf9DPYWHfqV", # gmail
  "tz1bxnm2CyNNn9cTKdotn1sqFwCYZ1UBbFwU"  # Random account
)

# Define currency
currency <- "usd"

# Define date span [min, max]
date_span <- c("2021-01-01T00:00:00Z", "2021-12-31T23:59:59Z")

# Path to Coinbase transaction data, if applicable (Otherwise set to NA)
cb_path <- "data/cb_transactions.csv"

# Include early OBJKT auction data (TRUE/FALSE)
objkt_v1 <- TRUE

# Assume RCS tokens with no transaction history were minted at 5tz
rcs_mint <- TRUE

# Assume NFT tokens are collectibles (TRUE/FALSE)
collectible <- FALSE

# Legal name (to be used on tax documents)
legal_name <- "datcsv"

# Social security # (to be used on tax documents)
ssn <- "000-00-0000"

# Run next steps in process
# Note: Once a step has been run, it is not necessary to run the step again
# unless the code has been updated.
#source("01_Operations_Data.R")
source("02_IS_Generation.R")
source("data/XX_IS_Adjustments.R")
source("03_BS_Generation.R")
source("04_Tax_Generation.R")
