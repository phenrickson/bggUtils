test_that("url exists and table exists", {
  bgg_ids_raw <- read.table(bgg_ids_url())
  # exists
  expect_type(
    bgg_ids_raw,
    type = "list"
  )
  # two columns
  expect_equal(
    ncol(bgg_ids_raw), 2
  )
})

test_that("table contains expected data", {
  bgg_ids <- get_bgg_ids()
  game_ids <- get_bgg_ids(id_type = c("boardgame"))
  expansion_ids <- get_bgg_ids(id_type = c("boardgameexpansion"))
  accessory_ids <- get_bgg_ids(id_type = c("boardgameaccessory"))
  expect_equal(
    names(bgg_ids), c("id", "type")
  )
  expect_equal(
    unique(bgg_ids$type), c("boardgame", "boardgameexpansion", "boardgameaccessory")
  )
  expect_true(
    nrow(bgg_ids) > 150, 000
  )
  expect_equal(
    unique(game_ids$type), c("boardgame")
  )
  expect_equal(
    unique(expansion_ids$type), c("boardgameexpansion")
  )
  expect_equal(
    unique(accessory_ids$type), c("boardgameaccessory")
  )
})

test_that("no duplicate ids", {
  bgg_ids <- get_bgg_ids()
  expect_equal(
    anyDuplicated(bgg_ids$id), 0
  )
})
