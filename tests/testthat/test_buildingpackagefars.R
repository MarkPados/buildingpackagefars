library(testthat)
library(buildingpackagefars)

context('Errors')

test_that('Throws errors', {
  throws_error(fars_read_years(years = 2000))})
