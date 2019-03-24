context("test-team_7")

# Test block
test_that("creating dataframe for a country's map works", {
  expect_warning(team_7(file = "bla"), "file does not exist")
  expect_warning(team_7(file = system.file("extdata","gadm36_AUS_1.cpg",package="Lab3R")), "file is not a shapefile")
  expect_warning(team_7(tolerance = "bla"), "tolerance is not numeric")
  expect_warning(team_7(tolerance = -1), "tolerance must be a positive number")
  expect_equal(is.data.frame(team_7(tolerance = 0.1)), TRUE)
}
)