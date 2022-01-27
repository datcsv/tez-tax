# Clear workspace
rm(list=ls())

# Import packages
packages <- c("tidyverse", "jsonlite", "magrittr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only=TRUE))

# User settings
addresses <- c(
  "tz1a2ZeWmyNQ8BiuFNTE4vmFEP9MBaP76QPX", # datcsv
  "tz1L5vaycmTzEDekjDJSFZJ1V8FPwAUCVSDM", # datcsv1
  "tz1Sbt4C1MXm1AWPK6qdgfDzciJCCUVADmUt", # datcsv2
  "tz2TXkQS6kPPXbuVMsNBcdtpjWf9DPYWHfqV"  # gmail
)

# Define tzkt API functions
tzkt_operations <- function(
  address, level=NA, limit=NA, quote="usd", base="https://api.tzkt.io/"
) {
  sfx <- paste0("v1/accounts/", address, "/operations?quote=", quote)
  if (!is.na(level)) sfx <- paste0(sfx, "&level.lt=", level)
  if (!is.na(limit)) sfx <- paste0(sfx, "&limit=", limit)
  url <- paste0(base, sfx)
  x <- fromJSON(url)
  return(x)
}

tzkt_operations_hash <- function(
  hash, quote="usd", base="https://api.tzkt.io/"
) {
  sfx <- paste0("v1/operations/", hash, "?quote=", quote)
  url <- paste0(base, sfx)
  x <- fromJSON(url)
  return(x)
}

# Download operations - first pass (wallet search)
limit_ops <- 1000
for (i in 1:length(addresses)) {
  operations_i <- tzkt_operations(address=addresses[i], limit=limit_ops)
  while ((nrow(operations_i) %% limit_ops) == 0) {
    level <- min(operations_i$level + 1)
    operations_i <- bind_rows(
      operations_i,
      tzkt_operations(address=addresses[i], level=level, limit=limit_ops)
    )
  }
  if (i == 1) operations <- operations_i
  else operations %<>% bind_rows(., operations_i)
}

# Clean operations data
operations %<>% 
  distinct(.) %>%
  arrange(., id) 

# Download operations - second pass (hash search)
operations_hash <- operations %>% 
  filter(., target[[2]] %in% addresses, amount > 0) %>%
  distinct(., hash)

for (i in 1:nrow(operations_hash)) {
  operations_i <- tzkt_operations_hash(operations_hash[i, ])
  if ("storage" %in% names(operations_i)) operations_i %<>% select(., -storage)
  if ("diffs" %in% names(operations_i))   operations_i %<>% select(., -diffs)
  if ("parameter" %in% names(operations_i)) {
    if ("value" %in% names(operations_i$parameter)) {
      if (class(operations_i$parameter$value) != "list") {
        operations_i$parameter$value <- list(operations_i$parameter$value)
      }
    }
  }
  if (i == 1) operations2 <- operations_i
  else operations2 %<>% bind_rows(., operations_i)
}

# Combine and clean operations data
operations %<>% 
  bind_rows(., operations2) %>%
  arrange(., id, hash) %>%
  distinct(.)

# Export data
save(addresses, file="addresses.RData")
save(operations, file="operations.RData")
