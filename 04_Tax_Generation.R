################################################################################
#                                                                              #
# Copyright 2022 datcsv                                                        #
#                                                                              #
# Licensed under the Apache License, Version 2.0 (the "License");              #
# you may not use this file except in compliance with the License.             #
# You may obtain a copy of the License at                                      #
#                                                                              #
#   http://www.apache.org/licenses/LICENSE-2.0                                 #
#                                                                              #  
# Unless required by applicable law or agreed to in writing, software          #
# distributed under the License is distributed on an "AS IS" BASIS,            #
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.     #
# See the License for the specific language governing permissions and          #
# limitations under the License.                                               #
#                                                                              #
################################################################################

# Load income statement, balance sheet, and tax data
load(file="data/is_updated.RData")
load(file="data/bs.RData")
load(file="data/tax_8949.RData")
load(file="data/xtzIncome_data.RData")

# Define template file paths
f8949   <- "forms/f8949.pdf"
f1040sd <- "forms/f1040sd.pdf"
f1040s1 <- "forms/f1040s1.pdf"

# Create pdf directories
tax_8949_dir <- "data/8949"
dir.create(tax_8949_dir, showWarnings=FALSE)

pdf_f1040sd_dir <- "data/f1040sd"
dir.create(pdf_f1040sd_dir, showWarnings=FALSE)

pdf_f1040s1_dir <- "data/f1040s1"
dir.create(pdf_f1040s1_dir, showWarnings=FALSE)

# Remove zero proceed sales 
# tax_8949 %<>% filter(., round(Proceeds, 2) > 0.00)

# Format tax form 8949 data for export
tax_8949_intuit <- tax_8949 %>%
  select(.,
    `Asset Amount`     = Token_Quantity,
    `Asset Name`       = Token_ID_Short,
    `Received Date`    = Date_Acquired,
    `Date Sold`        = Date_Sold,
    `Proceeds (USD)`   = Proceeds,
    `Cost Basis (USD)` = Cost_Basis,
    `Gain (USD)`       = Gain_Loss
  ) %>%
  mutate(.,
    `Asset Amount`     = sprintf("%.8f", as.numeric(`Asset Amount`)), 
    `Asset Name`       = toupper(str_replace(`Asset Name`, "/", "_")),
    `Received Date`    = format(`Received Date`, "%m/%d/%Y"),
    `Date Sold`        = format(`Date Sold`, "%m/%d/%Y"),
    `Proceeds (USD)`   = sprintf("%.8f", `Proceeds (USD)`), 
    `Cost Basis (USD)` = sprintf("%.8f", `Cost Basis (USD)`), 
    `Gain (USD)`       = sprintf("%.8f", `Gain (USD)`), 
    `Type`             = "Short Term"
  )

# Write tax form 8949 data to CSV file
tax_8949_intuit %>% write_csv(., file=paste0(tax_8949_dir, "/tax_8949_full.csv"))

# Write tax form 8949 to turbotax compatible CSV files
for (i in 1:ceiling(nrow(tax_8949) / 3999)) {
  tax_8949_intuit %>%
    filter(., row_number() %in% (1 + (i - 1) * 3999):(i * 3999)) %>%
    write_csv(., file=paste0(tax_8949_dir, "/tax_8949_", str_pad(i, 2, pad="0"), ".csv"))
}

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
  
  # Generate PDF file
  set_fields(
    input_filepath=f1040s1,
    output_filepath=paste0(pdf_f1040s1_dir, "/f1040s1.pdf"),
    fields=f1040s1_fields,
    overwrite=TRUE
  )
}

# Generate tax form 1040 schedule D
f1040sd_fields <- get_fields(input_filepath=f1040sd)
f1040sd_fields[[1]][[3]]  <- legal_name
f1040sd_fields[[2]][[3]]  <- ssn
f1040sd_fields[[4]][[3]]  <- "2"
f1040sd_fields[[17]][[3]] <- sprintf("%.2f", sum(tax_8949$Proceeds))
f1040sd_fields[[18]][[3]] <- sprintf("%.2f", sum(tax_8949$Cost_Basis))
if (sum(!is.na(tax_8949$Adjustment)) > 0) {
  f1040sd_fields[[19]][[3]] <- sprintf("%.2f", sum(tax_8949$Adjustments), na.rm=TRUE) 
}
f1040sd_fields[[20]][[3]] <- sprintf("%.2f", sum(tax_8949$Gain_Loss))
f1040sd_fields[[24]][[3]] <- sprintf("%.2f", sum(tax_8949$Gain_Loss))
f1040sd_fields[[46]][[3]] <- sprintf("%.2f", sum(tax_8949$Gain_Loss))
f1040sd_fields[[48]][[3]] <- "2"
f1040sd_fields[[55]][[3]] <- "2"

# Generate PDF file
set_fields(
  input_filepath=f1040sd,
  output_filepath=paste0(pdf_f1040sd_dir, "/f1040sd.pdf"),
  fields=f1040sd_fields,
  overwrite=TRUE
)


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
    output_filepath=paste0(tax_8949_dir, "/f8949_", str_pad(k, 4, pad="0"), ".pdf"),
    fields=f8949_fields,
    overwrite=TRUE
  )
  
}

# Merge PDF files (Note: staple_pdf will fail with a large number of files)
staple <- staple_pdf(
  input_directory=tax_8949_dir,
  output_filepath=paste0(tax_8949_dir, "/f8949_Final.pdf"),
  overwrite=TRUE
)
if (staple > 0) warning("Failed to merge 8949 forms.")
