# run_coverage.R
library(covr)
library("DT")

cat('Test code coverage\n')


# Run the coverage analysis
coverage = file_coverage('/app/utils.R', '/app/tests/testthat/test_unit.R')

# Print the coverage report
report(coverage, file = "/tmp/coverage_report.html")

