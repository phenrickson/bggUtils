test_that("function produces tidy data", {

        ids = c(12, 13, 164153)
        api_resp =
                get_bgg_games(ids, simplify = T)
        api_processed =
                api_resp %>%
                unnest(data) %>%
                preprocess_bgg_games()

        api_names = c("game_id",
                         "name",
                         "yearpublished",
                         "averageweight",
                         "usersrated",
                         "average",
                         "bayesaverage",
                         "numweights",
                         "minplayers",
                         "maxplayers",
                         "playingtime",
                         "minplaytime",
                         "maxplaytime",
                         "minage",
                         "description",
                         "thumbnail",
                         "image",
                         "categories",
                         "mechanics",
                         "publishers",
                         "designers",
                         "artists",
                         "families",
                         "mechanisms",
                         "components",
                         "themes")

  expect_equal(length(ids), nrow(api_resp))
  expect_equal(api_names, names(api_processed))
})
