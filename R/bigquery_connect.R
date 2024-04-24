#' connect to bigquery
#'
#' @import bigrquery
#'
#' @importFrom DBI dbConnect
#'
#' @param project gcp project id
#' @param dataset bigquery schema
#' @export bigquery_connect
bigquery_connect = function(project = "gcp-demos-411520",
                            dataset = "bgg") {

        # authenticate
        bigquery_authenticate()

        # connect
        DBI::dbConnect(
                bigrquery::bigquery(),
                project = project,
                dataset = dataset
        )
}

# authenticate via json
bigquery_authenticate = function(path = bigquery_auth_file()) {
    bigrquery::bq_auth(path = path)
}

# location of json auth file
bigquery_auth_file = function() {
    Sys.getenv("GCS_AUTH_FILE")
}
