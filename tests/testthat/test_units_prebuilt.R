#!/usr/local/bin/Rscript
source("/app/utils.R")

dht::greeting()

library(digest)
library(dplyr)
library(knitr)
library(TeachingDemos)
library(testthat)

test_that("Docker image works as expected", {
  force <- TRUE
  
  input_file <- '/app/tests/testthat/address-sample-date-UTAH.csv'
  fields <- "id, address_date, matched_state, precision, geocode_result, fraction_assisted_income, fraction_high_school_edu, median_income, fraction_no_health_ins, fraction_poverty, fraction_vacant_housing, dep_index, drivetime_selected_center, nearest_center_pcgc, drivetime_pcgc, version"
  field_list = trimws(unlist(strsplit(fields,",")))
  if (!file.exists(input_file)) {
    stop("Cannot find input file. Please check if the input file exists.")
  }
  
  args_list <- list(site = "PCGC_CCHMC", filename = input_file, output_prefix = 'output', score_threshold = 0.5, include_deid_fields = fields)
  expect_error(rdcrn_run(args_list), 'site argument is not one of our available sites or missing; \nRun `docker run ghcr.io/pcgcid/geocoder_pcgc:latest --site-list` to see all available sites')
  
  site <- 'PCGC_UTAH'
  
  # Test case 1
  args_list <- list(site = site, filename = input_file, score_threshold = 0.5, include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_equal(output[1, 'drivetime_selected_center'], 120)
  expect_in(field_list, colnames(output))
  
  # Test case 2
  expect_in(output[2, 'drivetime_selected_center'], c(120, 135))
  
  # Test case 3, 4, 5, 6
  expect_equal(output[3, 'drivetime_selected_center'], 45)
  expect_identical(is.na(output[4, 'drivetime_selected_center']), is.na(NA))
  expect_equal(output[5, 'drivetime_selected_center'], 600)
  expect_equal(output[6, 'drivetime_selected_center'], 720)
  
  
})


test_that("Docker image works as expected for irregular case", {
  site <- 'PCGC_UTAH'
  fields <- "id, address_date, matched_state, precision, geocode_result, fraction_assisted_income, fraction_high_school_edu, median_income, fraction_no_health_ins, fraction_poverty, fraction_vacant_housing, dep_index, drivetime_selected_center, nearest_center_pcgc, drivetime_pcgc, version"
  
  # Test case 1
  args_list <- list(site = site, filename = '/app/tests/testthat/output_v2.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_identical(is.na(output[10, 'drivetime_selected_center']), TRUE)
  
  # Test case 2
  args_list <- list(site = site, filename = '/app/tests/testthat/output-with-phi_1.csv', output_prefix = NULL, score_threshold = NULL)
  output <- rdcrn_run(args_list)
  expect_identical(is.na(output[15, 'drivetime_selected_center']), TRUE)
  
  # Test case 3
  args_list <- list(site = site, filename = '/app/tests/testthat/my_address_file_geocoder_3.3.0_score_threshold_0.5.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_identical(is.na(output[25, 'drivetime_selected_center']), TRUE)
  
  # Test case 4
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_identical(is.na(output[6, 'drivetime_selected_center']), TRUE)
  
  # Test case 5
  args_list <- list(site = site, filename = '/app/tests/testthat/output-with-phi_2.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_equal(output[16,'drivetime_selected_center'], 600)
  expect_identical(is.na(output[1,'drivetime_selected_center']), T)
  
  # Test case 6
  args_list <- list(site = site, filename = '/app/tests/testthat/geocoded_input.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_identical(is.na(output[25,'drivetime_selected_center']), T)

  # Test case 7
  args_list <- list(site = site, filename = '/app/tests/testthat/my_address_file_geocoder_3.3.0_score_threshold_0.5.csv', score_threshold = 'all', include_deid_fields = NULL)
  output <- rdcrn_run(args_list)
  expect_identical(is.na(output[25, 'drivetime_selected_center']), TRUE) 
  
})


test_that("Docker image works as expected for cases with errors", { 
  site <- 'PCGC_UTAH'
  fields <- "id, address_date, matched_state, precision, geocode_result, fraction_assisted_income, fraction_high_school_edu, median_income, fraction_no_health_ins, fraction_poverty, fraction_vacant_housing, dep_index, drivetime_selected_center, nearest_center_pcgc, drivetime_pcgc, version"
  
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample_no_quote.csv', include_deid_fields = fields)
  expect_error(rdcrn_run(args_list), "Please check input file.\nPlease make sure to enclose the address information in quotation marks (e.g., “) if it contains commas.\n", fixed = TRUE)
  
 
  
  # Test case 11
  args_list <- list(site = site, filename = '/app/tests/testthat/input_no_address.csv')
  expect_error(rdcrn_run(args_list), "no column called address found in the input file")
})


test_that("Docker image works as expected for cases with each consortium", {
  fields <- "id, address_date, matched_state, precision, geocode_result, fraction_assisted_income, fraction_high_school_edu, median_income, fraction_no_health_ins, fraction_poverty, fraction_vacant_housing, dep_index, drivetime_selected_center, nearest_center_pcgc, drivetime_pcgc, version"
  
  # case 1 - PCGC_YALE
  site <- 'PCGC_YALE'
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_equal(output[41,'drivetime_selected_center'], 120)
  expect_equal(output[41,'drivetime_selected_center'], 20)
  # case 2 - PCGC_BOSTON
  site <- 'PCGC_BOSTON'
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_equal(output[44,'drivetime_selected_center'], 300)
  
  # case 3 - PCGC_MTSINAI
  site <- 'PCGC_MTSINAI'
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_equal(output[44,'drivetime_selected_center'], 15)
  
  # case 4 - PCGC_COLUMBIA
  site <- 'PCGC_COLUMBIA'
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  case_4_result = output[40,'drivetime_selected_center']
  expect_lt(case_4_result, 150)
  expect_gt(case_4_result, 105)

  
  # case 5 - PCGC_CHOP
  site <- 'PCGC_CHOP'
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_equal(output[40,'drivetime_selected_center'], 45)
  
  
  # case 6 - PCGC_UTAH
  site <- 'PCGC_UTAH'
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_equal(output[49,'drivetime_selected_center'], 600)
  
  # case 7 - PCGC_CHLA
  site <- 'PCGC_CHLA'
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_equal(output[52,'drivetime_selected_center'], 420)
  
  # case 8 - PCGC_CHOA
  site <- 'PCGC_CHOA'
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  case_8_result = output[29,'drivetime_selected_center']
  expect_lt(case_8_result, 240)
  expect_gt(case_8_result, 150)
  
  # case 9 - PCGC_UMICH
  site <- 'PCGC_UMICH'
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_equal(output[25,'drivetime_selected_center'], 720)
  })

  
