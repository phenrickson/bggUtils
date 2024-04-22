#' filter wide player count data to find games matching crtieria
#'
#' @param data wide player count data
#' @param type type of vote, such as 'best', 'recommended', or 'not recommended'
#' @param numplayers number of players
#'
#' @return data frame with game ids matching type and numplayers
#' @export find_playercounts
#'
#' @examples
#' query_playercounts(game_id = c(12, 7, 21)) |>
#' wider_playercounts() |>
#' find_playercounts(type = "best", numplayers = 3)
#'
find_playercounts = function(data,
                             type = c("best"),
                             numplayers
                             ) {

        type = case_when(
                tolower(type) %in% c('recommended', 'rec') ~ 'rec',
                tolower(type) %in% c('not recommended', 'recommended') ~ 'notrec',
                .default = tolower(type)
        )

        search_type = paste("playercount", type, sep = "_")
        search_players = paste(numplayers, collapse = "|")

        data |>
                filter(if_any(c(search_type), ~ grepl(search_players, .)))
}
