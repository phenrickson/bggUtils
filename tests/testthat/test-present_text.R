# returns expected output
test_that("present_text returns output", {
  expect_equal(present_text("mechanics_worker_placement"), "Mech: Worker Placement")
})

# returns expected output for some annoying strings
test_that("present_text returns output", {
  expect_equal(present_text("world_war_ii"), "World War II")
})

# abbereviation works
test_that("present_text returns", {
  char <- 30
  expect_lt(
    nchar(
      present_text(
        text = "A very long variable with many characters and numbers 2342323",
        minlength = char
      )
    ),
    char + 1
  )
})
