#!/usr/local/bin/Rscript
# utils.R
library(sf)
library(purrr)
library(dplyr)
library(digest)
library(knitr)


rdcrn_drivetime <- function(filename, out_filename, consortium = "ctsa") {
  #browser()
  consortium = tolower(consortium)
  if (!consortium %in% c("ctsa","cegir") ){
    cat("`consortium` should only be either 'CTSA' or 'CEGIR' (case insensitive)")
  }
  
  if (consortium == "ctsa"){
    iso_filename <- Sys.getenv("ISO_FILENAME", "/app/isochrones.rds")
  
    centers_filename <- Sys.getenv("CENTERS_FILENAME", '/app/ctsa_centers.csv')
    output_filename <- Sys.getenv("OUTPUT_FILENAME", "/app/output.csv")
  }else if(consortium == "cegir"){  
    iso_filename <- Sys.getenv("ISO_FILENAME", "/app/isochrones_cegir_no_overlap.rds")
  
    centers_filename <- Sys.getenv("CENTERS_FILENAME", '/app/CEGIRSites.csv')
    output_filename <- Sys.getenv("OUTPUT_FILENAME", "/app/output.csv")
  }
  
  centers <- readr::read_csv(centers_filename) 

  #rename CEGIR data columns so that it has similar structure to that of CTSA
  if(consortium == "cegir"){  
    colnames(centers) <- c("abbreviation", "consortium" ,"name","address","city","state","country",
                               "zipcode","website_url","geometry","lat","lon")
  }

  centers = centers %>% arrange(abbreviation)
  
  d <- dht::read_lat_lon_csv(filename, nest_df = T, sf_out = T, project_to_crs = 5072)
  isochrones <- readRDS(glue::glue(iso_filename))
  dx <- sapply(isochrones, function(x) {
    message("isochrones -- match start")
    # st_join(d$d, x, largest = TRUE)$value
    result = st_join(d$d, x, largest = F) %>%
      tidyr::unnest(.rows) %>%
      dplyr::distinct(.row, .keep_all = TRUE) 
    
    result$drive_time
    # this interferers with results -- message("isochrones -- match done")
  }) 
  
  
  
  
  #dx <- sapply(dx, FUN = function(x) x[1])
  
  if (dim(as.matrix(dx))[2] == 1){
    dx =t(as.matrix(dx))
  }
  
  df <- as.data.frame(dx)
  # colnames(df)[apply(df,1,which.max)]
  
  mins <- apply(df, 1, which.min)
  # Extract values using sapply
  distance <- sapply(1:nrow(df), function(i) as.numeric(df[i, mins[i]]))
  
  not_found <- length(centers$abbreviation) + 1
  mins[is.na(mins == 0)] <- not_found
  min_centers <- centers$abbreviation[unlist(mins)]
  
  # skip duplicates -- FIXME: flag them
  indexes <- apply(d$d[,1,drop=FALSE], 1, function(x) { unlist(x)[1] })
  suppressWarnings(d$raw_data$nearest_center[indexes] <- min_centers)
  suppressWarnings(d$raw_data$distance[indexes] <- distance)
  
  # save(d, mins, dx, df,  min_centers, file="/Users/kouzy6/tmp/geocoder-drivetime/x.rds")
  
  #output <- cbind(d$raw_data, min_centers)
  output <- d$raw_data %>% dplyr::select(-c(".row"))
  write.csv(output, file = out_filename)
  output
}


rdcrn_geocode <- function(filename, out_filename, score_threshold = 0.5) {
  #browser()
  
  d <- readr::read_csv(filename, show_col_types = FALSE)
  # d <- readr::read_csv('test/my_address_file.csv')
  # d <- readr::read_csv('test/my_address_file_missing.csv')
  
  ## must contain character column called address
  if (!"address" %in% names(d)) stop("no column called address found in the input file", call. = FALSE)
  
  ## clean up addresses / classify 'bad' addresses
  d$address <- dht::clean_address(d$address)
  d$po_box <- dht::address_is_po_box(d$address)
  d$cincy_inst_foster_addr <- dht::address_is_institutional(d$address)
  d$non_address_text <- dht::address_is_nonaddress(d$address)
  
  ## exclude 'bad' addresses from geocoding (unless specified to return all geocodes)
  if (score_threshold == "all") {
    d_for_geocoding <- d
  } else {
    d_excluded_for_address <- dplyr::filter(d, cincy_inst_foster_addr | po_box | non_address_text)
    d_for_geocoding <- dplyr::filter(d, !cincy_inst_foster_addr & !po_box & !non_address_text)
  }
  
  out_template <- tibble(
    street = NA, zip = NA, city = NA, state = NA,
    lat = NA, lon = NA, score = NA, precision = NA,
    fips_county = NA, number = NA, prenum = NA
  )
  
  ## geocode
  cli::cli_alert_info("now geocoding ...", wrap = TRUE)
  geocode <- function(addr_string) {
    stopifnot(class(addr_string) == "character")
    
    out <- system2("ruby",
                   args = c("/app/geocode.rb", shQuote(addr_string)),
                   stderr = FALSE, stdout = TRUE
    )
    
    if (length(out) > 0) {
      out <- out %>%
        jsonlite::fromJSON()
      
      out <-
        bind_rows(out_template, out) %>%
        .[2, ]
    } else {
      out <- out_template
    }
    
    out
  }
  
  # if any geocodes are returned, regardless of score_threshold...
  if (nrow(d_for_geocoding) > 0) {
    d_for_geocoding$geocodes <- mappp::mappp(d_for_geocoding$address,
                                             geocode,
                                             parallel = TRUE,
                                             # cache = TRUE,
                                             # cache_name = "geocoding_cache"
    )
    
    ## extract results, if a tie then take first returned result
    d_for_geocoding <- d_for_geocoding %>%
      dplyr::mutate(
        row_index = 1:nrow(d_for_geocoding),
        geocodes = purrr::map(geocodes, ~ .x %>%
                                purrr::map(unlist) %>%
                                as_tibble())
      ) %>%
      tidyr::unnest(cols = c(geocodes)) %>%
      dplyr::group_by(row_index) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        matched_street = street,
        matched_city = city,
        matched_state = state,
        matched_zip = zip
      ) %>%
      dplyr::select(-fips_county, -prenum, -number, -row_index) %>%
      dplyr::mutate(precision = factor(precision,
                                       levels = c("range", "street", "intersection", "zip", "city"),
                                       ordered = TRUE
      )) %>%
      dplyr::arrange(desc(precision), score)
  } else if (nrow(d_for_geocoding) == 0 & score_threshold != "all") {
    # if no geocodes are returned and not returning all geocodes,
    # then bind non-geocoded with out template
    d_excluded_for_address <-
      bind_rows(d_excluded_for_address, out_template) %>%
      .[1:nrow(.) - 1, ]
  }
  
  ## clean up 'bad' address columns / filter to precise geocodes
  cli::cli_alert_info("geocoding complete; now filtering to precise geocodes...", wrap = TRUE)
  if (score_threshold == "all") {
    out_file <- d_for_geocoding
  } else {
    out_file <- dplyr::bind_rows(d_excluded_for_address, d_for_geocoding) %>%
      dplyr::mutate(
        geocode_result = dplyr::case_when(
          po_box ~ "po_box",
          cincy_inst_foster_addr ~ "cincy_inst_foster_addr",
          non_address_text ~ "non_address_text",
          (!precision %in% c("street", "range")) | (score < score_threshold) ~ "imprecise_geocode",
          TRUE ~ "geocoded"
        ),
        lat = ifelse(geocode_result == "imprecise_geocode", NA, lat),
        lon = ifelse(geocode_result == "imprecise_geocode", NA, lon)
      ) %>%
      select(-po_box, -cincy_inst_foster_addr, -non_address_text) # note, just "PO" not "PO BOX" is not flagged as "po_box"
  }
  
  ## write out file
  if (!is.null(out_filename)) {
    dht::write_geomarker_file(
      out_file,
      filename = out_filename,
      geomarker = "", version = ""
      # , argument = glue::glue("score_threshold_{score_threshold}")
    )
    
    # out__.csv ->  out_filename
    base <- tools::file_path_sans_ext(out_filename)
    ext <- tools::file_ext(out_filename)
    tmp_filename <- paste0(base,"__.",ext)
    file.rename(tmp_filename, out_filename)
  }
  
  ## summarize geocoding results and
  ## print geocoding results summary to console
  if (score_threshold != "all") {
    geocode_summary <- out_file %>%
      mutate(geocode_result = factor(geocode_result,
                                     levels = c(
                                       "po_box", "cincy_inst_foster_addr", "non_address_text",
                                       "imprecise_geocode", "geocoded"
                                     ),
                                     ordered = TRUE
      )) %>%
      group_by(geocode_result) %>%
      tally() %>%
      mutate(
        `%` = round(n / sum(n) * 100, 1),
        `n (%)` = glue::glue("{n} ({`%`})")
      )
    
    n_geocoded <- geocode_summary$n[geocode_summary$geocode_result == "geocoded"]
    n_total <- sum(geocode_summary$n)
    pct_geocoded <- geocode_summary$`%`[geocode_summary$geocode_result == "geocoded"]
    cli::cli_alert_info("{n_geocoded} of {n_total} ({pct_geocoded}%) addresses were successfully geocoded. See detailed summary below.",
                        wrap = TRUE
    )
    knitr::kable(geocode_summary %>% dplyr::select(geocode_result, `n (%)`))
  }
  
  return(out_file)
}


rdcrn_run <- function(opt){
  # #browser()
  
  if (is.null(opt$score_threshold)) opt$score_threshold <- 0.5
  d <- readr::read_csv(opt$filename, show_col_types = FALSE)
  
  # check if we have coordinates -- if not let's geocode first
  if (!"lat" %in% names(d) || !"lon" %in% names(d)) {
    geocoded_df <- rdcrn_geocode(filename = opt$filename, score_threshold = opt$score_threshold, out_filename = opt$out_filename)
    drivetime_input <- opt$out_filename
  } else {
    drivetime_input <- opt$filename
  }
  output_ctsa_df = rdcrn_drivetime(drivetime_input, opt$out_filename,"ctsa")
  output_cegir_df = rdcrn_drivetime(drivetime_input, opt$out_filename,"cegir")

  return(list(ctsa = output_ctsa_df, cegir = output_cegir_df))


}
