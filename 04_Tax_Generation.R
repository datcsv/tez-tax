# Load income statement, balance sheet, and tax data
load(file="data/is_updated.RData")
load(file="data/bs.RData")
load(file="data/tax_8949.RData")
load(file="data/xtzIncome_data")

# Define form filepaths
f8949 <- "forms/f8949.pdf"

f8949_fields <- get_fields(input_filepath=f8949)
f8949_fields$`topmostSubform[0].Page1[0].f1_1[0]`$value <- "Test"

set_fields(
  input_filepath=f8949,
  output_filepath=paste0("data/f8949_", ssn, ".pdf"),
  fields=f8949_fields,
  overwrite=TRUE,
)

