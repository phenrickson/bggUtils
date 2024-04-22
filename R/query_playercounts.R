#' query playercount data for selected games
#'
#' @param conn DBI connection to database
#' @param game_ids vector of game ids
#' @param rec_values type of recommendation ('Best', 'Not Recommended')
#' @param vote_types vote types
#' @param minvotes minimum number of votes
#' @param eval whether to evaluate query or not
#' @param ...
#'
#' @importFrom glue glue_sql
#'
#' @return a data frame with number of votes for playercounts
#' @export query_playercounts
#' @examples
#' query_playercounts(conn = bigquery_connect(), game_ids = c(12, 7))
query_playercounts = function(conn = bigquery_connect(),
                              game_ids,
                              rec_values = c('Best', 'Recommended', 'Not Recommended'),
                              vote_types = c('yes'),
                              minvotes = 10,
                              eval = T,
                              ...) {

    query = glue::glue_sql(
        "SELECT
        game_id,
        value,
        numplayers,
        numvotes
        FROM bgg.games_playercounts
        WHERE game_id IN ({game_ids*})
        AND value IN ({rec_values*})
        AND vote IN ({vote_types*})
        AND totalvotes >= ({minvotes*})
        ORDER BY game_id, numplayers",
        .con = conn
    )

    get_query(query = query,
              conn = conn,
              eval = eval)
}
