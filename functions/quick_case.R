
# Function to quickly classify and filter common operation groups
quick_case <- function(x, entry=NA, case=NA, type=1) {
  if (type == 1)      x %>% filter(., entry %in% parameterEntry) 
  else if (type == 2) x %>% top_n(., n=-1, wt=id)
  y %<>% mutate(., case = case)
  return(y)
}
