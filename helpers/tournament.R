# tournament --------------------------------------------------------------

buttons_dt <- function(id, lab) {
  map_chr(seq_along(id), ~ {
    as.character(actionButton(id[.x], label = lab[.x]))
  })
}


tournament_init <- function(players, rounds) {
  tab <- crossing(p1 = players, p2 = players) 
  
  if (rounds == 1) tab <- filter(tab, p1 <= p2)
  
  tab
}

tournamet_games <- function(tab) {
  tab %>% 
    filter(p1 != p2) %>% 
    transmute(id = glue("trngame_{p1}_{p2}")) %>% 
    pull(id)
}

crosstab_players <- function(tab, tab2) {
  df <- 
    tab %>% 
    full_join(tab2, by = c("p1", "p2"))
  
  
  pts <-
    tab2 %>%
    separate(score, c("s1", "s2"), sep = " : ")
  
  if (nrow(pts) > 0) {
    pts <-
      bind_rows(
      pts %>%
        select(p1, s1, s2) %>%
        rename(p1 = 1, score = 2, score_neg = 3),
      pts %>%
        select(p2, s2, s1) %>%
        rename(p1 = 1, score = 2, score_neg = 3)) %>%
      group_by(p1) %>%
      summarise(score     = sum(as.numeric(score)), 
                score_neg = sum(as.numeric(score_neg)), 
                .groups = "drop") %>% 
      mutate(p2 = p1) %>% 
      unite(score_new, score, score_neg, sep = " : ") %>% 
      select(p1, p2, score_new)
    
    df <- df %>% 
      left_join(pts, by = c("p1", "p2")) %>% 
      mutate(score = if_else(is.na(score_new), score, score_new)) %>% 
      select(-score_new)
    
  }

  df %>% 
    mutate(btn  = buttons_dt(glue("trngame_{p1}_{p2}"), 
                             lab = glue("{p1} vs. {p2}")), 
           btn = if_else(p1 == p2, "0 : 0", btn), 
           btn = if_else(is.na(score), btn, score)) %>% 
    select(-score) %>% 
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
