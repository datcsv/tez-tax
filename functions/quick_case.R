
# Function to quickly classify and filter common operation groups
quick_case <- function(x, entry=NA, case=NA, type=1) {
  if (type == 1)      y <- filter(x, parameterEntry == entry)
  else if (type == 2) y <- top_n(x, n=1, wt=id)
  y <- mutate(y, case=case)
  return(y)
}
