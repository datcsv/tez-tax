################################################################################
#                                                                              #
# Copyright 2022 datcsv                                                        #
#                                                                              #
# Licensed under the Apache License, Version 2.0 (the "License");              #
# you may not use this file except in compliance with the License.             #
# You may obtain a copy of the License at                                      #
#                                                                              #
#   http://www.apache.org/licenses/LICENSE-2.0                                 #
#                                                                              #  
# Unless required by applicable law or agreed to in writing, software          #
# distributed under the License is distributed on an "AS IS" BASIS,            #
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.     #
# See the License for the specific language governing permissions and          #
# limitations under the License.                                               #
#                                                                              #
################################################################################

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
