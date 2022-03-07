# Load income statement data
load(file="data/is_updated.RData")
is <- is_updated

# Generate 8949 form
tax_8949 <- tibble(
  Description   = character(),
  Date_Acquired = date(),
  Date_Sold     = date(),
  Proceeds      = double(),
  Cost_Basis    = double(),
  Codes         = character(),
  Adjustment    = double(),
  Gain_Loss     = double()
)

for (i in 1:nrow(is)) {
  
}

# Generate 8949 condensed form
tax_8949c <- tibble(
  Description   = character(),
  Date_Acquired = date(),
  Date_Sold     = date(),
  Proceeds      = double(),
  Cost_Basis    = double(),
  Codes         = character(),
  Adjustment    = double(),
  Gain_Loss     = double()
)
