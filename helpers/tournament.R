# tournament --------------------------------------------------------------

buttons_dt <- function(id, lab) {
  map_chr(seq_along(id), ~ {
    as.character(actionButton(id[.x], label = lab[.x]))
  })
}

crosstab_players <- function(players, rounds) {
  tab <- crossing(p1 = players, p2 = players) 

  if (rounds == 1) tab <- filter(tab, p1 <= p2)
  
  tab %>% 
    mutate(btn  = buttons_dt(glue("button_{p1}_{p2}"), 
                             lab = glue("{p1} vs. {p2}")), 
           btn = if_else(p1 == p2, "0 : 0", btn)) %>% 
    pivot_wider(names_from = p2, values_from = btn, names_sort = TRUE) %>% 
    rename("Tournament" = p1) %>% 
    datatable(escape    = FALSE, 
              rownames  = FALSE, 
              selection = "none", 
              options = list(
                paging    = FALSE, 
                info      = FALSE, 
                searching = FALSE, 
                ordering  = FALSE
              ))
}
