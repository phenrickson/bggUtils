test_that("bgg api is returning a response to request for a single game", {
  # submit request
  resp <- request_bgg_api(12)

  # check status code
  expect_equal(resp$status_code, 200)
})

test_that("bgg api is returning a response to request for multiple games", {
  # submit request
  resp <- request_bgg_api(c(12, 7, 13, 174430))

  # check status code
  expect_equal(resp$status_code, 200)
})


test_that("returning data when submitting a request for one game", {
  # id to test with
  input_id <- 12

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
  game_data <- request_games(input_id)

  # tests
  expect_equal(input_id, game_data$game_id)
  # check names of data
  expect_identical(game_template, names(game_data))
  # check that number of rows of data is less than the number of ids
  expect_true(nrow(game_data) <= length(input_id))
})

test_that("returning data when submitting a request for one game", {
  # id to test with
  input_id <- c(12, 7, 50, 13)

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
  game_data <- request_games(input_id)

  # tests
  expect_equal(input_id, game_data$game_id)
  # check names of data
  expect_identical(game_template, names(game_data))
  # check that number of rows of data is less than the number of ids
  expect_true(nrow(game_data) <= length(input_id))
})
