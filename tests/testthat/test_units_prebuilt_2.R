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
  #case 1 - PCGC_YALE
  site <- 'PCGC_YALE'
  args_list <- list(site = site, filename = '/app/tests/testthat/address-sample.csv', include_deid_fields = fields)
  output <- rdcrn_run(args_list)
  expect_equal(output[41,'drivetime_selected_center'], 120)
  
  
  
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
})

