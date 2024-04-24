#' query info about selected game ids
#'
#' @param conn DBI connection to database
#' @param game_ids vector of game ids
#' @param eval whether to evaluate query or not
#'
#' @importFrom glue glue_sql
#' @return data frame with info for selected game ids
#' @export query_info
query_info = function(conn = bigquery_connect(),
                      game_ids,
                      eval = T) {

        query = glue::glue_sql(
                "SELECT
                *
                FROM bgg.games_info
                WHERE game_id IN ({game_ids*})",
                .con = conn,
        )

        get_query(
                conn = conn,
                query = query,
                eval = eval
        )
}
