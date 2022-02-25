# Load data
load(file="data/addresses.RData")
load(file="data/currency.RData")
load(file="data/date_span.RData")
load(file="data/cb_tx.RData")
load(file="data/cb_data.RData")
load(file="data/operations.RData")
load(file="data/is.RData")

# Clean income statement
is %<>% 
  arrange(., timestamp) %>%
  mutate(., 
    tokenSent     = ifelse(tokenSender   %in% addresses, tokenAmount, 0),
    tokenReceived = ifelse(tokenReceiver %in% addresses, tokenAmount, 0),
    gainLoss      = NA
  ) %>% 
  filter(., 
    (tokenSent == 0) | (tokenReceived == 0)
  ) %>%
  select(., 
    timestamp, quote, xtzSent, xtzReceived, tokenID, tokenSent, tokenReceived, 
    walletTx, proceeds, costBasis, gainLoss
  )

# First pass through balance sheet, log all acqusitions
for (i in 1:nrow(is)) {
  
  
}

# Form 8949
# (a) Description of property
# (b) Date acquired
# (c) Date sold or disposal of 
# (d) Proceeds (sales price)
# (e) Cost or other basis
# (f) Code(s) from instructions
# (g) Amount of adjustment
# (h) Gain or (loss)