#' request game data from BGG API (v2)
#'
#' #' @description
#'
#' @description This function submits game ids to BGG's API and returns data associated with a game in a nested table
#'
#' @import bigrquery
#'
#' @importFrom DBI dbConnect
#'
#' @param game_ids vector of game_ids
#' @param ... additional arguments to pass to request request bgg api
#' @export request_games
#'
#' @examples
#' request_games(game_ids = c(12, 7))
request_games <- function(game_ids, ...) {
  # submit request
  resp <- game_ids |>
    request_bgg_api(...)

  # parse response
  xml <- resp |>
    parse_bgg_xml()

  # tidy xml
  xml |>
    tidy_bgg_xml()
}

# creates request for bgg games
build_request_url <- function(game_ids) {
  bgg_prefix <- "https://www.boardgamegeek.com/xmlapi2/thing?id="
  bgg_suffix <- "&stats=1"
  collapsed_ids <- paste(game_ids, collapse = ",")

  url <- paste0(bgg_prefix, collapsed_ids, bgg_suffix)

  url
}

# submits request to bgg api with throttle and rate
request_bgg_api <- function(game_ids, max_tries = 5, rate = 5 / 60) {
  # if duplicated
  if (identical(game_ids, unique(game_ids)) == F) {
    # warning
    warning("duplicated game ids in request. submitting only unique game ids")
    # deduplicate
    game_ids <- unique(game_ids)
  }

  # request to bgg api
  # submit game ids in comma delimited to api
  req <- httr2::request(
    build_request_url(game_ids)
  )

  # submit request and get response
  resp <-
    req |>
    # throttle rate of request
    httr2::req_throttle(rate = rate) |>
    # set policies for retry
    httr2::req_retry(
      max_tries = max_tries
    ) |>
    # perform
    httr2::req_perform()

  return(resp)
}

# parses xml returned from bgg
parse_bgg_xml <- function(resp) {
  # check status
  if (resp$status_code == 200) {
    message("request status ok...")
  } else {
    stop("request status not ok")
  }

  # parse
  parse_bgg_api(resp)
}

# converts bgg xml into nested table
tidy_bgg_xml <- function(xml) {
  # extract data
  game_data <- furrr::future_map(xml, extract_bgg_data)

  # convert to to data frame
  game_info <-
    lapply(
      game_data, "[",
      c(
        "type",
        "ids",
        "yearpublished",
        "minplayers",
        "maxplayers",
        "playingtime",
        "minplaytime",
        "maxplaytime",
        "minage",
        "description",
        "thumbnail",
        "image"
      )
    ) |>
    dplyr::bind_rows() |>
    tidyr::unnest(everything(), keep_empty = T) |>
    tidyr::nest(info = c(
      yearpublished,
      minplayers,
      maxplayers,
      playingtime,
      minplaytime,
      maxplaytime,
      minage,
      description,
      thumbnail,
      image
    ))

  # game names
  game_names <-
    lapply(game_data, "[[", "names") |>
    dplyr::bind_rows(.id = "game_id") |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      game_id = as.integer(game_id),
      sortindex = as.integer(sortindex)
    ) |>
    tidyr::nest(names = one_of(c("type", "sortindex", "value")))

  # game links
  game_links <-
    lapply(game_data, "[[", "links") |>
    dplyr::bind_rows(.id = "game_id") |>
    dplyr::mutate(
      game_id = as.integer(game_id),
      id = as.integer(id)
    ) |>
    tidyr::nest(links = one_of("type", "id", "value"))

  # game statistics
  game_statistics <-
    lapply(game_data, "[[", "statistics") |>
    dplyr::bind_rows(.id = "game_id") |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        c(
          "game_id",
          "usersrated",
          "median",
          "owned",
          "trading",
          "wanting",
          "wishing",
          "numcomments",
          "numweights"
        ),
        as.integer
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(
          "averageweight",
          "average",
          "bayesaverage",
          "stddev"
        ),
        as.numeric
      )
    ) |>
    tidyr::nest(
      statistics = c(
        averageweight,
        usersrated,
        average,
        bayesaverage,
        stddev,
        median,
        owned,
        trading,
        wanting,
        wishing,
        numcomments,
        numweights
      )
    )

  # game ranks
  game_ranks <-
    lapply(game_data, "[[", "ranks") |>
    dplyr::bind_rows(.id = "game_id") |>
    dplyr::mutate(
      dplyr::across(
        c(
          "game_id",
          "id"
        ),
        as.integer
      )
    ) |>
    tidyr::nest(ranks = c(
      type,
      id,
      name,
      friendlyname,
      value,
      bayesaverage
    ))

  # game polls
  game_polls <-
    lapply(game_data, "[[", "polls") |>
    dplyr::bind_rows(.id = "game_id") |>
    dplyr::mutate(
      game_id = as.integer(game_id),
      numvotes = as.integer(numvotes)
    ) |>
    tidyr::nest(polls = c(
      value,
      numvotes,
      numplayers
    ))

  # join up
  tidy_bgg_data <-
    game_info |>
    dplyr::left_join(
      game_names,
      by = c("game_id")
    ) |>
    dplyr::left_join(
      game_links,
      by = c("game_id")
    ) |>
    dplyr::left_join(
      game_statistics,
      by = c("game_id")
    ) |>
    dplyr::left_join(
      game_ranks,
      by = c("game_id")
    ) |>
    dplyr::left_join(
      game_polls,
      by = c("game_id")
    )

  tidy_bgg_data
}
