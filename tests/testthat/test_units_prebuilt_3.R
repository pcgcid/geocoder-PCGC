#!/usr/local/bin/Rscript
source("/app/utils.R")

dht::greeting()

library(digest)
library(dplyr)
library(knitr)
library(TeachingDemos)
library(testthat)



test_that("Docker image works as expected for cases with each consortium", {
  fields <- "id, address_date, matched_state, precision, geocode_result, fraction_assisted_income, fraction_high_school_edu, median_income, fraction_no_health_ins, fraction_poverty, fraction_vacant_housing, dep_index, drivetime_selected_center, nearest_center_pcgc, drivetime_pcgc, version"
  
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
  print(output[25,'drivetime_selected_center'])
  expect_equal(output[25,'drivetime_selected_center'], 720)
})
