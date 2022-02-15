# tez-tax

Tezos tax tracking

# R Crash Course

R is a free software environment for statistical computing and graphics. It compiles and runs on a wide variety of UNIX platforms, Windows and MacOS. The most commonly used IDE for R is "RStudio".

1. Assignment is "<-".
2. Indexing starts at 1.
3. We like the tidyvese (https://r4ds.had.co.nz/tidy-data.html). 
4. Piping is taken from the "magrittr" package.
    1. Pipe operator is "%<%".
    2. Pipe + assignment operator is "%<>%".

# Instructions

1. Create a copy of the configuration file, "00_Config.R", and update it with your own information. 
  1. Until a 'prior year import' feature is available, it is necessary to set the start date to include all transactions. 

2. Run the files in numerical order, starting with the updated configuration file:
    1. 00_Config.R
    2. 01_Operations_Data.R*
    3. 02_IS_Generation.R

** The operations data code is used to download transaction data from the blockchain via the tzkt API. All outputs are saved to the 'data' directory and, thus, it is only necessary to run this program once. 
