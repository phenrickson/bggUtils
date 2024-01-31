#' get_bgg_ids: load universe of ids for games and expansions on boardgamegeek
#'
#' @description This function loads the universe of BGG ids from https://bgg.activityclub.org/bggdata/thingids.txt
#'
#' @param names character vector of length two with names for columns
#' @param id_type boardgame, boardgameexpansion, boardgameaccessory
#' @importFrom tidyr as_tibble
#' @importFrom dplyr filter
#' @importFrom glue glue
#'
#' @return data frame with bgg ids
#' @export get_bgg_ids
#'
get_bgg_ids <- function(id_type = NULL, names = c("id", "type")) {
  tab <- read.table(bgg_ids_url())
  prepped <- prep_bgg_ids(tab,
    names = names
  )

  filtered <- filter_bgg_ids(prepped,
    id_type = id_type
  )
  filtered
}

bgg_ids_url <- function() {
  "https://bgg.activityclub.org/bggdata/thingids.txt"
}

prep_bgg_ids <- function(tab, names = c("id", "type")) {
  names(tab) <- names

  tab |> as_tibble()
}

filter_bgg_ids <- function(tab, id_type = NULL) {
  if (is.null(id_type)) {
    tab
  } else {
    types <- paste(id_type, collapse = ", ")
    message(glue::glue("filtering {types}"))
    tab |>
      filter(type %in% id_type)
  }
}
