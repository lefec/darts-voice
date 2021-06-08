gif_search_custom <- function(.query, .limit) {
  tibble::tibble(url = giphyr::gif_search(query = .query, limit = .limit)$original)
}

gifs <-
  purrr::map_dfr(c("winner", "champion", "darts"), gif_search_custom, .limit = 3)
save(gifs, file = "data/gifs.csv")

