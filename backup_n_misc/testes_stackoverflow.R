df <- data.frame(
  country = c("arg", "arg", "arg", "arg", "arg", "arg", "arg", "arg", "arg", "bra", "bra", "bra", "bra", "bra", "bra", "bra", "bra", "bra"),
  year = c(2000, 2001, 2002, 2003, 2004, 2001, 2002, 2003, 2004, 2000, 2001, 2002, 2003, 2004, 2001, 2002, 2003, 2004),
  group = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "A", "A", "A", "A", "A", "B", "B", "B", "B"),
  value = c(324, 134, 334, 344, 364, 334, 634, 734, 364, 324, 1124, 56574, 3564, 3564, 334, 614, 534, 784)
)

x <- df %>% 
  group_by(country,year, group) %>% 
  mutate(fd = lag(value))
  
print(df)
