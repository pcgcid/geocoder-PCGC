library(testthat)
library(dplyr)

context("Docker Image Tests")

test_that("Docker image runs successfully", {
  # Pull the Docker image
  system("docker pull ghcr.io/dohn5r/geocoder_pcgc:0.0.1")
  
  # Get the current working directory
  current_dir <- getwd()
  
  old_output = dir(path = current_dir, pattern = 'output', ignore.case = T, full.names = T)
  file.remove(old_output)
  
  # Construct the Docker run command with the correct path
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/dohn5r/geocoder_pcgc:0.0.1",
    " -s PCGC_UTAH -i /tmp/address-sample-date-UTAH.csv -o output"
  )
  
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  
  #expected results of test cases:
  case_1_drivetime = 120
  case_2_drivetime = c(120,135)
  case_3_drivetime = 45
  case_4_drivetime = NA #out of range (> 16 hrs)
  case_5_drivetime = 600
  case_6_drivetime = 720
  
  #actual results of test cases:
  case_1_result = output[1,'drivetime_selected_center']
  case_2_result = output[2,'drivetime_selected_center']
  case_3_result = output[3,'drivetime_selected_center']
  case_4_result = output[4,'drivetime_selected_center']
  case_5_result = output[5,'drivetime_selected_center']
  case_6_result = output[6,'drivetime_selected_center']
  
  
  # Assert that the output contains some expected result
  expect_equal(case_1_result, case_1_drivetime)
  expect_in(case_2_result, case_2_drivetime)
  expect_equal(case_3_result, case_3_drivetime)
  expect_identical(is.na(case_4_result), is.na(case_4_drivetime))
  expect_equal(case_5_result, case_5_drivetime)
  expect_equal(case_6_result, case_6_drivetime)
  
  
})
