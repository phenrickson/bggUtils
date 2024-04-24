#' get query via DBI
#'
#' runs query given a connection
#' @param query a SQL query as a string
#' @param conn a DBI connection
#' @param eval to evaluate the query or to just show the query with connection
# runs query given connection/query or returns sql for query
get_query = function(query,
                     conn,
                     eval = T,
                     ...) {

    if (eval == T) {

        DBI::dbGetQuery(
            conn,
            statement = query,
            ...
        )
    }

    else {
        query
    }
}
