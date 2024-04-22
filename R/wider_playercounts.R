#' pivot and summarize longer player count data for presentation in tables
#'
#' @param data plong layercounts data by game
#'
#' @import dplyr tidyr
#' @return playercounts data in wide format
#' @export wider_playercounts
#'
wider_playercounts = function(data) {

        data |>
                pivot_playercounts() |>
                consolidate_playercounts()
}

slice_playercounts = function(data) {

                data |>
                mutate(value = tolower(gsub("\\s+", "", value))) |>
                group_by(game_id, numplayers) %>%
                slice_max(numvotes, n=1, with_ties = F) |>
                ungroup()
}

pivot_playercounts = function(data) {

        data |>
                mutate(value = tolower(gsub("\\s+", "", value))) |>
                group_by(game_id, numplayers) %>%
                slice_max(numvotes, n=1, with_ties = F) |>
                select(game_id, value, numplayers) |>
                pivot_wider(values_from = c("numplayers"),
                            names_from = c("value"),
                            id_cols = c("game_id"),
                            names_prefix = c("playercount_"),
                            values_fn = ~ paste(.x, collapse=",")
                ) |>
                ungroup() |>
                select(
                        game_id,
                        playercount_best,
                        playercount_rec = playercount_recommended,
                        playercount_notrec = playercount_notrecommended
                )
}

consolidate_playercounts = function(data) {

        data |>
                mutate(across(c(contains("playercount")),
                              ~ gsub(pattern = paste(as.character(seq(9, 100, by = 1)), collapse = "|"),
                                     replacement = "8+",
                                     x = .x))) %>%
                mutate(playercount_rec = case_when(
                        !is.na(playercount_best) & !is.na(playercount_rec) ~
                                paste(playercount_best, playercount_rec, sep = ","),
                        !is.na(playercount_best) & is.na(playercount_rec) ~
                                playercount_best,
                        TRUE ~ playercount_rec)) %>%
                rowwise() %>%
                mutate(across(c(contains("playercount")),
                              ~ stringr::str_split(.x, ",", ) %>%
                                      unlist() %>%
                                      unique() %>%
                                      sort() %>%
                                      paste(collapse = ","))) |>
                ungroup()

}
