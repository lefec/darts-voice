single <- c(0:20, 25)
double <- single * 2
triple <- single[1:21] * 3

values <- c(single, double, triple) %>% unique() %>% sort()

throws <- 
  crossing(d1 = values, d2 = values, d3 = values) %>% 
  filter(d1 >= d2, d2 >= d3) %>% 
  mutate(id = row_number())


double_outs <- 
  throws %>% 
  pivot_longer(d1:d3) %>% 
  mutate(could_be_double = value %in% double[-1]) %>% 
  group_by(id) %>% 
  summarise(at_least_one_double = any(could_be_double))


possible_scores <- 
  throws %>% 
  left_join(double_outs, by = "id") %>% 
  mutate(score = d1 + d2 + d3) %>% 
  group_by(score) %>% 
  summarise(double_out = any(at_least_one_double))
