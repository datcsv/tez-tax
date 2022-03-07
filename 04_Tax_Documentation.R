# Load income statement data
load(file="data/is_updated.RData")
is <- is_updated

# Generate tax spreadsheet
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
