context("test-team5_BL")

# Test block
test_that("creating dataframe for a country's map works", {
  expect_warning(team5_BL(file = "bla"), "file does not exist")
  expect_warning(team5_BL(tolerance = "bla"), "tolerance is not numeric")
  expect_warning(team5_BL(tolerance = -1), "tolerance must be a positive number")
  expect_equal(is.data.frame(team5_BL(tolerance = 0.1)), TRUE)
}
)

