context("test-team_1")

test_that("creating dataframe for a country's map works", {
  expect_warning(team_1(file = "bla"), "file does not exist")
  expect_warning(team_1(tolerance = "bla"), "tolerance is not numeric")
  expect_equal(is.data.frame(team_1(tolerance = 0.1)), TRUE)
})
