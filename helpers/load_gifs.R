gif_search_custom <- function(.query, .limit) {
  tibble(url = giphyr::gif_search(query = .query, limit = .limit)$original)
}

gifs <- 
  map_dfr(c("winner", "champion", "darts"), gif_search_custom, .limit = 100)

readr::write_csv(gifs, path = "data/gifs.csv")
