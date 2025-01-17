library(testthat)
library(dplyr)

context("Docker Image Tests")

test_that("Docker image runs successfully", {
  # Pull the Docker image
  system("docker pull ghcr.io/pcgcid/geocoder_pcgc:alpha")
  
  # Get the current working directory
  current_dir <- getwd()
  
  # old_output = dir(path = current_dir, pattern = 'output', ignore.case = T, full.names = T)
  # file.remove(old_output)
  
  # Construct the Docker run command with the correct path
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/address-sample-date-UTAH.csv -o output --force"
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


test_that("Docker image works as expected for irregular case", {
  # Pull the Docker image
  #system("docker pull ghcr.io/dohn5r/geocoder_pcgc:alpha")
  
  # Get the current working directory
  current_dir <- getwd()
  
  # old_output = dir(path = current_dir, pattern = 'output', ignore.case = T, full.names = T)
  # file.remove(old_output)
  
  # case 1
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/output_v2.csv -o output --force"
  )
  
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  
  #expected results of test cases:
  case_1_result = output[10,'drivetime_selected_center']
  expect_identical(is.na(case_1_result), T)
  
  # case 2
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/output-with-phi_1.csv --force"
  )
  
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  
  #expected results of test cases:
  case_2_result = output[15,'drivetime_selected_center']
  expect_identical(is.na(case_2_result), T)
  
  
  # case 3
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/my_address_file_geocoder_3.3.0_score_threshold_0.5.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_3_result = output[25,'drivetime_selected_center']
  expect_identical(is.na(case_3_result), T)
  
  # case 4  
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/address-sample.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_4_result = output[6,'drivetime_selected_center']
  expect_identical(is.na(case_4_result), T)
  
  
  # case 5
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/output-with-phi_2.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_5_result = output[16,'drivetime_selected_center']
  expect_equal(case_5_result, 600)
  expect_identical(is.na(output[1,'drivetime_selected_center']), T)
  
  # case 6
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/geocoded_input.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_6_result = output[25,'drivetime_selected_center']
  expect_identical(is.na(case_6_result), T)
  
  # case 7
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/my_address_file_geocoder_3.3.0_score_threshold_0.5.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_7_result = output[25,'drivetime_selected_center']
  expect_identical(is.na(case_7_result), T) 
  
})


test_that("Docker image works as expected for cases with errors", {
  current_dir <- getwd()
  # case 1 - expected error  
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/address-sample_no_quote.csv --force"
  )
  
  # Run the Docker container
  expect_warning(system(docker_command, intern = T))
  
  # case 2 - expected error  
  docker_command_2 <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/input_no_address.csv --force"
  )
  
  # Run the Docker container
  expect_warning(system(docker_command_2, intern = T))
})


test_that("Docker image works as expected for cases with errors", {
  current_dir <- getwd()
  # case 1 - expected error  
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/address-sample_no_quote.csv --force"
  )
  
  # Run the Docker container
  expect_warning(system(docker_command, intern = T))
  
  # case 2 - expected error  
  docker_command_2 <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/input_no_address.csv --force"
  )
  
  # Run the Docker container
  expect_warning(system(docker_command_2, intern = T))
})


test_that("Docker image works as expected for cases with each consortium", {
  current_dir <- getwd()
  # case 1 - PCGC_YALE
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_YALE -i /tmp/address-sample.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_1_result = output[41,'drivetime_selected_center']
  expect_equal(case_1_result, 120)
  
  # case 2 - PCGC_BOSTON
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_BOSTON -i /tmp/address-sample.csv  --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_2_result = output[44,'drivetime_selected_center']
  expect_equal(case_2_result, 300)
  
  
  # case 3 - PCGC_MTSINAI
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_MTSINAI -i /tmp/address-sample.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_3_result = output[44,'drivetime_selected_center']
  expect_equal(case_3_result, 15)
  
  # case 4 - PCGC_COLUMBIA
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_COLUMBIA -i /tmp/address-sample.csv  --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_4_result = output[40,'drivetime_selected_center']
  expect_lt(case_4_result, 150)
  expect_gt(case_4_result, 105)
  
  
  # case 5 - PCGC_CHOP
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_CHOP -i /tmp/address-sample.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_5_result = output[40,'drivetime_selected_center']
  expect_equal(case_5_result, 45)
  
  # case 6 - PCGC_UTAH
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UTAH -i /tmp/address-sample.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_6_result = output[49,'drivetime_selected_center']
  expect_equal(case_6_result, 600)
  
  # case 7 - PCGC_CHLA
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_CHLA -i /tmp/address-sample.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_7_result = output[52,'drivetime_selected_center']
  expect_equal(case_7_result, 420)
  

  
  # case 8 - PCGC_CHOA
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_CHOA -i /tmp/address-sample.csv --force"
  )
  
  #docker run -v $PWD:/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha -s PCGC_CHOA -i /tmp/address-sample.csv
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_8_result = output[29,'drivetime_selected_center']
  expect_lt(case_8_result, 240)
  expect_gt(case_8_result, 150)
  
  
  # case 9 - PCGC_UMICH
  docker_command <- paste0(
    "docker run -v '", current_dir, "':/tmp ghcr.io/pcgcid/geocoder_pcgc:alpha",
    " -s PCGC_UMICH -i /tmp/address-sample.csv --force"
  )
  
  # Run the Docker container
  system(docker_command, intern = T)
  
  filepath = paste0(current_dir, '/output-deid.csv')
  output <- read.csv(filepath)
  case_9_result = output[25,'drivetime_selected_center']
  expect_equal(case_9_result, 720)
})



