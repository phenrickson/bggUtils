#' connect to bigquery
#'
#' @import bigrquery
#'
#' @importFrom DBI dbConnect
#'
#' @param project gcp project id
#' @param dataset bigquery schema
#' @export bigquery_connect
bigquery_connect <- function(project = Sys.getenv("GCS_PROJECT_ID"),
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
bigquery_authenticate <- function(path = Sys.getenv("GCS_AUTH_FILE")) {
  bigrquery::bq_auth(path = path)
}
