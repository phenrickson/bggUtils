#' get_user_collection: pulling collection data for a boardgamegeek user
#'
#' @description This function pulls boardgamegeek collection data for a given username
#'
#'
#'
#' @param username a username on boardgamegeek; case sensitive
#' @param max_tries number of tries with BGG's api before stopping
#' @import httr2
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_text
#'
#' @return data frame with user collection
#' @export get_user_collection
#'
#' @examples
#' user_collection <- get_user_collection("mrbananagrabber")
#'
get_user_collection <- function(username,
                                max_tries = 5) {
  xml <-
    request_collection(username, max_tries = max_tries) |>
    get_collection_xml()

  collection <-
    xml |>
    get_collection() |>
    remove_duplicate_games()

  dplyr::tibble(
    username = username,
    url = build_url(username),
    collection,
    load_ts = Sys.time()
  ) |>
    # distinct
    distinct(
      username,
      url,
      game_id,
      name,
      type,
      rating,
      own,
      preordered,
      prevowned,
      fortrade,
      want,
      wanttoplay,
      wanttobuy,
      wishlist,
      wishlistpriority,
      load_ts,
      lastmodified
    ) |>
    # set nas
    mutate(rating = na_if(rating, "N/A")) |>
    # set types
    mutate(username,
      url,
      game_id = as.integer(game_id),
      name = name,
      type,
      rating = as.numeric(rating),
      own = as.numeric(own),
      preordered = as.numeric(preordered),
      prevowned = as.numeric(prevowned),
      fortrade = as.numeric(fortrade),
      want = as.numeric(want),
      wanttoplay = as.numeric(wanttoplay),
      wanttobuy = as.numeric(wanttobuy),
      wishlist = as.numeric(wishlist),
      wishlistpriority = as.numeric(wishlistpriority),
      load_ts,
      lastmodified,
      .keep = "none"
    )
}

# remove duplicate games based on lastmodified
remove_duplicate_games <- function(collection) {
  collection |>
    group_by(game_id) |>
    filter(lastmodified == max(lastmodified)) |>
    ungroup()
}

# build url for username api request
build_url <- function(username) {
  paste0(
    "https://www.boardgamegeek.com/xmlapi2/collection?username=",
    username,
    "&subtype=boardgame",
    "&stats=1"
  )
}

# build request to keep trying for up to two minutes based on status
build_request <- function(url,
                          max_tries = 5) {
  request(url) |>
    req_retry(
      is_transient = ~ resp_status(.x) == 202,
      max_tries = max_tries,
      backoff = ~30
    )
}

# perform user request
request_collection <- function(username,
                               max_tries = 5) {
  username |>
    build_url() |>
    build_request(max_tries = max_tries) |>
    req_perform()
}

# get xml from response
get_collection_xml <- function(response) {
  response |>
    resp_body_xml(simplifyVector = T) |>
    xml_children()
}

# get collection info
get_collection <- function(xml) {
  get_status_vars <- function(xml, var) {
    vec <-
      xml |>
      xml_find_all("//status") |>
      xml_attr(var)

    col <-
      dplyr::tibble(vec)

    names(col) <- var

    col
  }

  types <-
    xml |>
    xml_attr("subtype")

  ids <- xml |>
    xml_attr("objectid")

  collids <-
    xml |>
    xml_attr("collid")

  names <-
    xml %>%
    xml_find_all("//name") |>
    xml_text()

  status_vars <-
    c(
      "own",
      "preordered",
      "prevowned",
      "fortrade",
      "want",
      "wanttoplay",
      "wanttobuy",
      "wishlist",
      "wishlistpriority",
      "lastmodified"
    )

  status <-
    purrr::map(
      status_vars,
      ~ get_status_vars(xml, .x)
    ) |>
    dplyr::bind_cols()

  rating <-
    xml |>
    xml_find_all("//rating") |>
    xml_attr("value")

  dplyr::bind_cols(
    dplyr::tibble(game_id = ids),
    dplyr::tibble(collection_id = collids),
    dplyr::tibble(type = types),
    dplyr::tibble(name = names),
    status,
    dplyr::tibble(rating = rating)
  )
}
