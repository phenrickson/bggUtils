test_that("bgg api is returning a response to request for a single game", {
  # submit request
  resp <- req_bgg_api(12)

  # check status code
  expect_equal(resp$status_code, 200)
})

test_that("bgg api is returning a response to request for multiple games", {
  # submit request
  resp <- req_bgg_api(c(12, 7, 13, 174430))

  # check status code
  expect_equal(resp$status_code, 200)
})


test_that("returning data when submitting a request for one game", {
  # id to test with
  input_id <- 12

  # names expected in list
  resp_template <- c(
    "game_ids",
    "problem_game_ids",
    "bgg_games_data",
    "timestamp"
  )

  # names expected in data
  game_template <- c(
    "type",
    "game_id",
    "info",
    "names",
    "links",
    "statistics",
    "ranks",
    "polls"
  )

  # get data
  game_data <- get_bgg_games(input_id)

  # tests
  expect_identical(input_id, game_data$game_ids)
  # check names of response
  expect_identical(resp_template, names(game_data))
  # check names of data
  expect_identical(game_template, names(game_data$bgg_games_data))
  # check that number of rows of data is less than the number of ids
  expect_true(nrow(game_data$bgg_games_data) <= length(input_id))
})


test_that("returning data when submitting a request for multiple games", {
  # id to test with
  input_id <- c(12, 7, 13, 174430)

  # names expected in list
  resp_template <- c(
    "game_ids",
    "problem_game_ids",
    "bgg_games_data",
    "timestamp"
  )

  # names expected in data
  game_template <- c(
    "type",
    "game_id",
    "info",
    "names",
    "links",
    "statistics",
    "ranks",
    "polls"
  )

  # get data
  game_data <- get_bgg_games(input_id)

  # tests
  expect_identical(input_id, game_data$game_ids)
  # check names of response
  expect_identical(resp_template, names(game_data))
  # check names of data
  expect_identical(game_template, names(game_data$bgg_games_data))
  # check that number of rows of data is less than the number of ids
  expect_true(nrow(game_data$bgg_games_data) <= length(input_id))
})
