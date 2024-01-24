test_that("ggplot runs without error", {
  library(ggplot2)

  p <-
    economics |>
    tidyr::pivot_longer(cols = -c(date)) |>
    ggplot(aes(
      x = date,
      y = value
    )) +
    geom_line() +
    facet_wrap(name ~ .,
      scales = "free_y"
    ) +
    theme_bgg()

  expect_no_error(p)
})
