# Load income statement, balance sheet, and tax data
load(file="data/is_updated.RData")
load(file="data/bs.RData")
load(file="data/tax_8949.RData")
load(file="data/xtzIncome_data.RData")

# Define form filepaths
f8949 <- "forms/f8949.pdf"

# Update form identification fields
f8949_fields <- get_fields(input_filepath=f8949)
f8949_fields[[1]][[3]] <- legal_name
f8949_fields[[2]][[3]] <- ssn
f8949_fields[[5]][[3]] <- "3"

# Update form tax form rows
for (i in 1:14) {
  
  f8949_fields[[6 + (i - 1) * 8]][[3]]  <- "TEST"
  f8949_fields[[7 + (i - 1) * 8]][[3]]  <- "TEST"
  f8949_fields[[8 + (i - 1) * 8]][[3]]  <- "TEST"
  f8949_fields[[9 + (i - 1) * 8]][[3]]  <- "TEST"
  f8949_fields[[10 + (i - 1) * 8]][[3]] <- "TEST"
  f8949_fields[[11 + (i - 1) * 8]][[3]] <- "TEST"
  f8949_fields[[12 + (i - 1) * 8]][[3]] <- "TEST"
  f8949_fields[[13 + (i - 1) * 8]][[3]] <- "TEST"
  
}

# Generate PDF file
set_fields(
  input_filepath=f8949,
  output_filepath=paste0("data/f8949_", ssn, ".pdf"),
  fields=f8949_fields,
  overwrite=TRUE,
)

