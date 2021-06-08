possible_scores <- function() {
  single <- c(0:20, 25)
  double <- single * 2
  triple <- single[1:21] * 3

  values <- c(single, double, triple) %>% unique() %>% sort()

  throws <-
    tidyr::crossing(d1 = values, d2 = values, d3 = values) %>%
    dplyr::filter(d1 >= d2, d2 >= d3) %>%
    dplyr::mutate(id = dplyr::row_number())


  double_outs <-
    throws %>%
    tidyr::pivot_longer(d1:d3) %>%
    dplyr::mutate(could_be_double = value %in% double[-1]) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(at_least_one_double = any(could_be_double), .groups = "drop")

  throws %>%
    dplyr::left_join(double_outs, by = "id") %>%
    dplyr::mutate(score = d1 + d2 + d3) %>%
    dplyr::group_by(score) %>%
    dplyr::summarise(double_out = any(at_least_one_double))
}
