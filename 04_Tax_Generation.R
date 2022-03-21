# Load income statement, balance sheet, and tax data
load(file="data/is_updated.RData")
load(file="data/bs.RData")
load(file="data/tax_8949.RData")
load(file="data/xtzIncome_data.RData")

# Testing data
tax_8949 <- tax_8949[1:14, ]

# Define form filepaths
f8949 <- "forms/f8949.pdf"

# Update form identification fields
f8949_fields <- get_fields(input_filepath=f8949)
f8949_fields[[1]][[3]] <- legal_name
f8949_fields[[2]][[3]] <- ssn
f8949_fields[[5]][[3]] <- "3"

# Update form tax form rows
for (i in seq(1, nrow(tax_8949), by=14)) {
  for (j in 1:min(14, nrow(tax_8949) + 1 - i)) {
    f8949_fields[[6  + (j - 1) * 8]][[3]] <- tax_8949[i + j - 1, 1]
    f8949_fields[[7  + (j - 1) * 8]][[3]] <- tax_8949[i + j - 1, 2]
    f8949_fields[[8  + (j - 1) * 8]][[3]] <- tax_8949[i + j - 1, 3]
    f8949_fields[[9  + (j - 1) * 8]][[3]] <- tax_8949[i + j - 1, 4]
    f8949_fields[[10 + (j - 1) * 8]][[3]] <- tax_8949[i + j - 1, 5]
    f8949_fields[[11 + (j - 1) * 8]][[3]] <- tax_8949[i + j - 1, 6]
    f8949_fields[[12 + (j - 1) * 8]][[3]] <- tax_8949[i + j - 1, 7]
    f8949_fields[[13 + (j - 1) * 8]][[3]] <- tax_8949[i + j - 1, 8]
  }
}

# Generate PDF file
set_fields(
  input_filepath=f8949,
  output_filepath=paste0("data/f8949_", ssn, ".pdf"),
  fields=f8949_fields,
  overwrite=TRUE,
)

