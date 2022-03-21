# Load income statement, balance sheet, and tax data
load(file="data/is_updated.RData")
load(file="data/bs.RData")
load(file="data/tax_8949.RData")
load(file="data/xtzIncome_data.RData")

# Testing data
tax_8949 <- tax_8949[1:59, ]

# Define template filepaths
f8949 <- "forms/f8949.pdf"

# Create pdf directories
dir.create("data/pdf", showWarnings=FALSE)

# Generate tax form 8949
for (i in seq(1, nrow(tax_8949), by=14)) {
  
  if (i == 1) k <- 1
  else k <- k + 1

  # Update form identification fields
  f8949_fields <- get_fields(input_filepath=f8949)
  f8949_fields[[1]][[3]] <- legal_name
  f8949_fields[[2]][[3]] <- ssn
  f8949_fields[[5]][[3]] <- "3"
  
  # Update capital gain/loss entry fields
  for (j in 1:min(14, nrow(tax_8949) + 1 - i)) {
    f8949_fields[[6  + (j - 1) * 8]][[3]] <- tax_8949[[i + j - 1, 1]]
    f8949_fields[[7  + (j - 1) * 8]][[3]] <- tax_8949[[i + j - 1, 2]]
    f8949_fields[[8  + (j - 1) * 8]][[3]] <- tax_8949[[i + j - 1, 3]]
    f8949_fields[[9  + (j - 1) * 8]][[3]] <- sprintf("%.2f", tax_8949[[i + j - 1, 4]])
    f8949_fields[[10 + (j - 1) * 8]][[3]] <- sprintf("%.2f", tax_8949[[i + j - 1, 5]])
    f8949_fields[[11 + (j - 1) * 8]][[3]] <- tax_8949[[i + j - 1, 6]]
    if (!is.na(tax_8949[[i + j - 1, 7]])) {
      f8949_fields[[12 + (j - 1) * 8]][[3]] <- sprintf("%.2f", tax_8949[[i + j - 1, 7]])
    }
    f8949_fields[[13 + (j - 1) * 8]][[3]] <- sprintf("%.2f", tax_8949[[i + j - 1, 8]])
  }
  
  # Update capital gain/loss total fields
  f8949_fields[[118]][[3]] <- sprintf("%.2f", sum(tax_8949[i:(i + 13), 4], na.rm=TRUE))
  f8949_fields[[119]][[3]] <- sprintf("%.2f", sum(tax_8949[i:(i + 13), 5], na.rm=TRUE))
  f8949_fields[[121]][[3]] <- sprintf("%.2f", sum(tax_8949[i:(i + 13), 7], na.rm=TRUE))
  f8949_fields[[122]][[3]] <- sprintf("%.2f", sum(tax_8949[i:(i + 13), 8], na.rm=TRUE))
  
  # Generate PDF file
  set_fields(
    input_filepath=f8949,
    output_filepath=paste0("data/pdf/f8949_", ssn, "_", str_pad(k, 4, pad="0"), ".pdf"),
    fields=f8949_fields,
    overwrite=TRUE,
  )
}
