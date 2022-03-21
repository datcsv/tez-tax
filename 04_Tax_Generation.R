# Load income statement, balance sheet, and tax data
load(file="data/is_updated.RData")
load(file="data/bs.RData")
load(file="data/tax_8949.RData")
load(file="data/xtzIncome_data.RData")

# Define template file paths
f8949   <- "forms/f8949.pdf"
f1040s1 <- "forms/f1040s1.pdf"

# Create pdf directories
tax_8949_dir <- "data/pdf_8949"
dir.create(tax_8949_dir, showWarnings=FALSE)
pdf_f1040s1_dir <- "data/pdf_f1040s1"
dir.create(pdf_f1040s1_dir, showWarnings=FALSE)

# Generate tax form 1040 schedule 1
xtzIncome <- sum(xtzIncome_data$quote * (xtzIncome_data$xtzReceived - xtzIncome_data$xtzSent))
if (xtzIncome > 100) {
  
  # Update fields
  f1040s1_fields <- get_fields(input_filepath=f1040s1)
  f1040s1_fields[[1]][[3]]  <- legal_name
  f1040s1_fields[[2]][[3]]  <- ssn
  f1040s1_fields[[27]][[3]] <- ""
  f1040s1_fields[[28]][[3]] <- "Cryptocurrency (Tezos) staking rewards, NFT royalties, and payments."
  f1040s1_fields[[29]][[3]] <- sprintf("%.2f", xtzIncome)
  f1040s1_fields[[30]][[3]] <- sprintf("%.2f", xtzIncome)
  f1040s1_fields[[31]][[3]] <- sprintf("%.2f", xtzIncome)
  f1040s1_fields[[62]][[3]] <- sprintf("%.2f", xtzIncome)
  
  # Generate PDF file
  set_fields(
    input_filepath=f1040s1,
    output_filepath=paste0(pdf_f1040s1_dir, "/f1040s1_", ssn, ".pdf"),
    fields=f1040s1_fields,
    overwrite=TRUE
  )
}

# Generate tax form 8949
for (i in seq(1, nrow(tax_8949), by=14)) {
  
  # Iterator for file name
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
    output_filepath=paste0(tax_8949_dir, "/f8949_", ssn, "_", str_pad(k, 4, pad="0"), ".pdf"),
    fields=f8949_fields,
    overwrite=TRUE
  )
}

# Merge PDF files
staple_pdf(
  input_directory=tax_8949_dir,
  output_filepath=paste0(tax_8949_dir, "/f8949_Final_", ssn, ".pdf"),
  overwrite=TRUE
)
