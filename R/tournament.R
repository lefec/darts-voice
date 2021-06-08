# tournament --------------------------------------------------------------

buttons_dt <- function(id, lab) {
  purrr::map_chr(seq_along(id), ~ {
    as.character(shiny::actionButton(id[.x], label = lab[.x]))
  })
}


tournament_init <- function(players, rounds) {
  tab <- tidyr::crossing(p1 = players, p2 = players)

  if (rounds == 1) tab <- dplyr::filter(tab, p1 <= p2)

  tab
}

tournamet_games <- function(tab) {
  tab %>%
    dplyr::filter(p1 != p2) %>%
    dplyr::transmute(id = glue::glue("trngame_{p1}_{p2}")) %>%
    dplyr::pull(id)
}

crosstab_players <- function(tab, tab2) {
  df <-
    tab %>%
    dplyr::full_join(tab2, by = c("p1", "p2"))


  pts <-
    tab2 %>%
    tidyr::separate(score, c("s1", "s2"), sep = " : ")

  if (nrow(pts) > 0) {
    pts <-
      dplyr::bind_rows(
      pts %>%
        dplyr::select(p1, s1, s2) %>%
        dplyr::rename(p1 = 1, score = 2, score_neg = 3),
      pts %>%
        dplyr::select(p2, s2, s1) %>%
        dplyr::rename(p1 = 1, score = 2, score_neg = 3)) %>%
      dplyr::group_by(p1) %>%
      dplyr::summarise(score     = sum(as.numeric(score)),
                       score_neg = sum(as.numeric(score_neg)),
                       .groups = "drop") %>%
      dplyr::mutate(p2 = p1) %>%
      tidyr::unite(score_new, score, score_neg, sep = " : ") %>%
      dplyr::select(p1, p2, score_new)

    df <- df %>%
      dplyr::left_join(pts, by = c("p1", "p2")) %>%
      dplyr::mutate(score = dplyr::if_else(is.na(score_new), score, score_new)) %>%
      dplyr::select(-score_new)

  }

  df %>%
    dplyr::mutate(btn  = buttons_dt(glue::glue("trngame_{p1}_{p2}"),
                                    lab = glue::glue("{p1} vs. {p2}")),
                  btn = dplyr::if_else(p1 == p2, "0 : 0", btn),
                  btn = dplyr::if_else(is.na(score), btn, score)) %>%
    dplyr::select(-score) %>%
    tidyr::pivot_wider(names_from = p2, values_from = btn, names_sort = TRUE) %>%
    dplyr::rename("Tournament" = p1) %>%
    DT::datatable(escape    = FALSE,
                  rownames  = FALSE,
                  selection = "none",
                  options = list(
                    paging    = FALSE,
                    info      = FALSE,
                    searching = FALSE,
                    ordering  = FALSE
                  ))
}
