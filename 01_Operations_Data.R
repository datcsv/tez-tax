# Download operations - first pass (wallet search)
limit_ops <- 1000
for (i in 1:length(addresses)) {
  operations_i <- tzkt_operations(
    address=addresses[i], limit=limit_ops, span=date_span, quote=currency
  )
  while (nrow(operations_i) > 0 & (nrow(operations_i) %% limit_ops) == 0) {
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
  operations_i <- tzkt_operations_hash(operations_hash[i, ], quote=currency)
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

# Download contract data
contracts <- c("KT1Dno3sQZwR5wUCWxzaohwuJwG3gX1VWj1Z")
for (i in 1:length(contracts)) {
  operations_i <- tzkt_operations(
    address=contracts[i], limit=limit_ops, span=date_span, quote=currency
  )
  while ((nrow(operations_i) %% limit_ops) == 0) {
    level <- min(operations_i$level + 1)
    operations_i <- bind_rows(
      operations_i,
      tzkt_operations(address=contracts[i], level=level, limit=limit_ops)
    )
  }
  if (i == 1) operations3 <- operations_i
  else operations3 %<>% bind_rows(., operations_i)
}

# Clean contract data
operations_hash <- operations3 %>% distinct(., hash)
operations_temp <- operations3[0, ]
for (i in 1:nrow(operations_hash)) {
  operations_i <- filter(operations3, hash == operations_hash[i, ])
  parameter_entry <- operations_i$parameter$entrypoint
  parameter_value <- operations_i$parameter$value
  token_receiver <- list_check(operations_i$parameter$value, "to_")
  if (("swap" %in% parameter_entry > 0) & (token_receiver %in% addresses)) {
    operations_temp %<>% bind_rows(., operations_i)
  }
}
operations3 <- operations_temp

# Combine and clean operations data
operations %<>% 
  bind_rows(., operations2, operations3) %>%
  arrange(., id, hash) %>%
  distinct(.)

# Save operations data
save(operations, file="data/operations.RData")