context("test-team_7")

test_that("creating dataframe for a country's map works", {
  expect_warning(team_7(file = "bla"), "file does not exist")
  expect_warning(team_7(tolerance = "bla"), "tolerance is not numeric")
})
