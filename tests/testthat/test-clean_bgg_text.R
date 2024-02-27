test_that("clean_bgg_text works", {
  expect_equal(
    clean_bgg_text("Fantasy Flight Games"),
    "fantasy_flight_games"
  )
  expect_equal(
    clean_bgg_text("Fantasy        Flight Games"),
    "fantasy_flight_games"
  )
  expect_equal(
    clean_bgg_text("Fantasy Flight Games?"),
    "fantasy_flight_games"
  )
})
