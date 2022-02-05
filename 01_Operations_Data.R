# Clear workspace
rm(list=ls())

# Load function
source("functions/load_packages.R")
source("functions/tzkt_operations.R")
source("functions/tzkt_operations_hash.R")

# Load packages
load_packages(c("tidyverse", "jsonlite", "magrittr"))

# Load data
load(file="data/addresses.RData")
load(file="data/currency.RData")
load(file="data/date_span.RData")

# Download operations - first pass (wallet search)
limit_ops <- 1000
for (i in 1:length(addresses)) {
  operations_i <- tzkt_operations(
    address=addresses[i], limit=limit_ops, span=date_span, quote=currency
  )
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
  filter(., target[[2]] %in% addresses) %>%
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
save(operations, file="data/operations.RData")
