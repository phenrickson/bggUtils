#' query active games data
#'
#' @param conn connection to database
#' @param game_ids vector of game ids
#' @param eval whether to evaluate query or not
#'
#' @importFrom glue glue_sql
#' @importFrom dplyr mutate
#' @return data frame with info for selected game ids
#' @export query_games
query_games <- function(conn = bigquery_connect(),
                        game_ids = NULL,
                        eval = T) {
  tidy_query_games <- function(data) {
    data |>
      mutate(
        info = map(info, fromJSON),
        statistics = map(statistics, fromJSON),
        names = map(names, fromJSON),
        links = map(links, fromJSON),
        ranks = map(ranks, fromJSON),
        polls = map(polls, fromJSON)
      )
  }

  if (is.null(game_ids)) {
    query <- glue::glue_sql(
      "SELECT
                        *
                        FROM bgg.active_games_api",
      .con = conn,
    )
  } else {
    query <- glue::glue_sql(
      "SELECT
                        *
                        FROM bgg.active_games_api
                        WHERE game_id IN ({game_ids*})",
      .con = conn,
    )
  }

  out <-
    get_query(
      conn = conn,
      query = query,
      eval = eval
    )

  if (is.data.frame(out)) {
    tidy_query_games(out)
  } else {
    out
  }
}
