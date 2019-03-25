context("test-team_5")

# Test block
test_that("creating dataframe for a country's map works", {
  expect_warning(team_5(file = "bla"), "file does not exist")
  expect_warning(team_5(tolerance = "bla"), "tolerance is not numeric")
  expect_warning(team_5(tolerance = -1), "tolerance must be a positive number")
  expect_equal(is.data.frame(team_5(tolerance = 0.1)), TRUE)
}
)

