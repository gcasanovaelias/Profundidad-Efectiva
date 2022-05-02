

n_topo <- topo %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  tibble() %>% 
  pivot_longer(
    cols = -c(aspect, x, y),
    names_to = "variables",
    values_to = "values"
  ) %>% 
  group_by(variables) %>% 
  nest() %>% 
  mutate(data2 = map2(
    .x = data,
    .y = variables,
    .f = ~ rename(.data = .x, .y = values)
  ))

attach(n_topo)

n_topo %>%
  mutate(symbols = map(.x = variables,
                       .f = ~ as.symbol(.x)))

n_topo %>% 
  transmute(data2 = map2(
    .x = data,
    .y = symbols,
    .f = ~ rename(.x, .y = values)
  )) %>% pluck("data2", 1)



