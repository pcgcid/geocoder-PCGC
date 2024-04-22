#!/usr/local/bin/Rscript
# utils.R
library(sf)
library(purrr)
library(dplyr)
library(digest)
library(knitr)


rdcrn_drivetime_selected_center <- function(filename, out_filename, selected_site) {
  require(dplyr)
  #browser()
  centers_filename <- Sys.getenv("CENTERS_FILENAME", '/app/pcgc_isochrones.csv')
  output_filename <- Sys.getenv("OUTPUT_FILENAME", "/app/output.csv")
  
  centers <- readr::read_csv(centers_filename) 
  
  centers = centers %>% arrange(abbreviation)
  
  if (! selected_site %in% centers$abbreviation){
    stop('site argument is invalid or missing; please consult documentation for details', call. = FALSE)
  }
  
  d <- dht::read_lat_lon_csv(filename, nest_df = T, sf_out = T, project_to_crs = 5072)
  
  
  d$d = d$d %>% dplyr::rename_with(., stringr::str_to_lower) %>% tidyr::unnest(.rows)
  
  d$raw_data = d$raw_data %>% dplyr::rename_with(., stringr::str_to_lower) 
  
  # dht::check_for_column(d$raw_data, "lat", d$raw_data$lat)
  # dht::check_for_column(d$raw_data, "lon", d$raw_data$lon)
  # 
  cat('loading isochrone shape file...\n')
  isochrones <- readRDS(glue::glue("/app/isochrones_pcgc_no_overlap.rds"))[[selected_site]] # 5072 projection
  
  ## add code here to calculate geomarkers
  cat('finding drive time for each point...\n')
  #d$d <- suppressWarnings( st_join(d$d, isochrones, largest = F) )
  
  cat('finding drivetime (m) for each point...\n')
  
  centers <- centers %>% 
    filter(abbreviation == selected_site) %>% 
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    st_transform(5072)


  d$d$drivetime_selected_center <-(st_join(d$d, isochrones, largest = F) %>% 
                    dplyr::distinct(.row, .keep_all = TRUE))$drive_time 

  # merge back on .row after unnesting .rows into .row
  if (!is.null(d$raw_data)) {

    if ("sf" %in% class(d$d)) d$d <- sf::st_drop_geometry(d$d)
    out <- dplyr::left_join(d$raw_data, d$d, by = ".row") %>% dplyr::select(-'.row')
  }
  
  if (is.null(d$raw_data)) {
    out <- d$d
  }
  

  #colnames(out)[which(colnames(out)=="drive_time")] <- paste0("drive_time_", selected_site)
  write.csv(out,out_filename,row.names = F, na = "")
  
  out

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
  d$non_address_text <- dht::address_is_nonaddress(d$address)
  
  ## exclude 'bad' addresses from geocoding (unless specified to return all geocodes)
  if (score_threshold == "all") {
    d_for_geocoding <- d
  } else {
    d_excluded_for_address <- dplyr::filter(d, po_box | non_address_text)
    if ('lat' %in% colnames(d) & 'lon' %in% colnames(d)){
      d <- d %>%
        dplyr::mutate(geocode_result = ifelse(!is.na(lat & lon),'geocoded_input', NA))
    }
    
    d_for_geocoding <- d %>%
      dplyr::filter( !po_box & !non_address_text)
    
    if ('lat' %in% colnames(d) & 'lon' %in% colnames(d)){    
      d_for_geocoding <-d_for_geocoding %>%
        dplyr::select(-c('lat','lon')) 
    }
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
    out_file <- d_for_geocoding %>%
      dplyr::mutate(
        geocode_result = dplyr::case_when(
          po_box ~ "po_box",
          non_address_text ~ "non_address_text",
          (!precision %in% c("street", "range")) ~ "imprecise_geocode",
          TRUE ~ "geocoded"
        )
        # ,
        # lat = ifelse(geocode_result != "geocoded", NA, lat),
        # lon = ifelse(geocode_result != "geocoded", NA, lon)
      ) %>%
      select(-po_box, -non_address_text) # note, just "PO" not "PO BOX" is not flagged as "po_box"
    
  } else {
    out_file <- dplyr::bind_rows(d_excluded_for_address, d_for_geocoding) %>%
      dplyr::mutate(
        geocode_result = dplyr::case_when(
          po_box ~ "po_box",
          non_address_text ~ "non_address_text",
          (!precision %in% c("street", "range")) | (score < score_threshold) ~ "imprecise_geocode",
          TRUE ~ "geocoded"
        ),
        lat = ifelse(geocode_result == "imprecise_geocode", NA, lat),
        lon = ifelse(geocode_result == "imprecise_geocode", NA, lon)
      ) %>%
      select(-po_box, -non_address_text) # note, just "PO" not "PO BOX" is not flagged as "po_box"
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
                                       "po_box", "non_address_text",
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


rdcrn_dep_idx <- function(filename, out_filename){
  library(digest);library(dplyr);library(knitr);library(tidyr)
  
  dht::greeting()
  
  ## load libraries without cats or warnings
  # withr::with_message_sink("/dev/null", library(dplyr))
  # withr::with_message_sink("/dev/null", library(tidyr))
  # withr::with_message_sink("/dev/null", library(sf))
  
  
  cat("reading input file...\n")
  d <- dht::read_lat_lon_csv(filename, nest_df = T, sf = T, project_to_crs = 5072)
  
  dht::check_for_column(d$raw_data, "lat", d$raw_data$lat)
  dht::check_for_column(d$raw_data, "lon", d$raw_data$lon)
  
  ## add code here to calculate geomarkers
  cat("reading tract shapefile...\n")
  tracts10 <- readRDS('/opt/tracts_2010_sf_5072.rds')
  
  cat("joining to 2010 TIGER/Line+ census tracts using EPSG:5072 projection\n")
  d_tract <- st_join(d$d, tracts10) %>%
    st_drop_geometry()
  
  cat("reading deprivation index data...\n")
  dep_index18 <- readRDS('/opt/tract_dep_index_18.rds')
  
  cat("joining 2018 tract-level deprivation index\n")
  d_tract <- left_join(d_tract, dep_index18, by = c('fips_tract_id' = 'census_tract_fips'))
  d_tract <- rename(d_tract, census_tract_id = fips_tract_id)
  
  ## merge back on .row after unnesting .rows into .row
  if (!is.null(d$raw_data)) {
    d_tract <- tidyr::unnest(d_tract, cols = c(.rows))
    if ("sf" %in% class(d_tract)) d_tract <- sf::st_drop_geometry(d_tract)
    out <- dplyr::left_join(d$raw_data, d_tract, by = ".row") %>% dplyr::select(-.row)
  }
  
  if (is.null(d$raw_data)) {
    out <- d_tract
  }
  
  readr::write_csv(out, out_filename)
}



rdcrn_drivetime <- function(filename, out_filename, consortium = "pcgc") {
  #browser()
  consortium = tolower(consortium)
  # if (!consortium %in% c("ctsa","cegir") ){
  #   cat("`consortium` should only be either 'CTSA' or 'CEGIR' (case insensitive)")
  # }
  
  # if (consortium == "ctsa"){
  iso_filename <- Sys.getenv("ISO_FILENAME", "/app/isochrones_pcgc_no_overlap.rds")
  
  centers_filename <- Sys.getenv("CENTERS_FILENAME", '/app/pcgc_isochrones.csv')
  output_filename <- Sys.getenv("OUTPUT_FILENAME", "/app/output.csv")

  centers <- readr::read_csv(centers_filename) 

  
  centers = centers %>% arrange(abbreviation)
  
  d <- dht::read_lat_lon_csv(filename, nest_df = T, sf_out = T, project_to_crs = 5072)
  
  d$d = d$d %>% dplyr::rename_with(., stringr::str_to_lower) 
  d$raw_data = d$raw_data %>% dplyr::rename_with(., stringr::str_to_lower) 
  
  isochrones <- readRDS(glue::glue(iso_filename))
  dx <- sapply(isochrones, function(x) {
    cat("isochrones -- match start\n")
    # st_join(d$d, x, largest = TRUE)$value
    result = st_join(d$d, x, largest = F) %>%
      tidyr::unnest(.rows) %>%
      dplyr::distinct(.row, .keep_all = TRUE) 
    
    
    result$drive_time
    # this interferers with results -- cat("isochrones -- match done")
  }) 
  
  
  
  #dx <- sapply(dx, FUN = function(x) x[1])
  
  if (dim(as.matrix(dx))[2] == 1){
    dx =t(as.matrix(dx))
  }
  
  df <- as.data.frame(dx)
  # colnames(df)[apply(df,1,which.max)]
  
  mins <- apply(df, 1, which.min) 
  mins[rowSums(is.na(df)) == ncol(df)] <- NA
  mins = mins %>% unlist()
  
  #print(sapply(1:nrow(df), function(i) as.numeric(df[i, mins[i]])))
  
  # Extract values using sapply
  drivetime <- sapply(1:nrow(df), function(i) as.numeric(df[i, mins[i]]))
  drivetime[rowSums(is.na(df)) == ncol(df)] <- NA
  drivetime = drivetime %>% unlist()
  
  not_found <- length(centers$abbreviation) + 1
  mins[is.na(mins == 0)] <- not_found
  min_centers <- colnames(df)[unlist(mins)]
  
  # skip duplicates -- FIXME: flag them
  indexes <- unlist(d$d[[".rows"]])
  
  d$raw_data$nearest_center <- NA
  d$raw_data$drivetime <- NA
  d$raw_data$d_to_centers <- NA
  
  
  d$raw_data$nearest_center[indexes] <- min_centers
  d$raw_data$drivetime[indexes] <- drivetime
  
  
  
  # Slice df into a list, with each row as an element
  list_of_rows <-  split(df, seq(nrow(df)))
  
  d$raw_data$d_to_centers[indexes] <- list_of_rows
  d_to_centers <- setNames(d$raw_data$d_to_centers, d$raw_data$id)
  
  d$raw_data = d$raw_data %>% 
    dplyr::select(-d_to_centers)
  
  
  # save(d, mins, dx, df,  min_centers, file="/Users/kouzy6/tmp/geocoder-drivetime/x.rds")
  
  #output <- cbind(d$raw_data, min_centers)
  output <- d$raw_data %>% dplyr::select(-c(".row"))
  
  #rename nearest_center column
  output <- output %>%     
    dplyr::rename_with(~ paste0(.x,"_", consortium, recycle0=T),
                       all_of(c("nearest_center", "drivetime")))
  # output <- output %>%
  #   dplyr::rename(all_of(c("nearest_center", "drivetime"),
  #                        ~ paste0(.x,"_", consortium), .override = TRUE))
  
  #modify file name
  #out_filename = sub("\\.csv", paste0("_",consortium, ".csv"), out_filename)
  write.csv(output, file = out_filename,row.names = F, na = "")
  
  # if (consortium == "ctsa"){
  return(list(output = output, d_to_pcgc_centers = d_to_centers))
  # }else if (consortium == "cegir"){
  #   return(list(output = output, d_to_cegir_centers = d_to_centers))
  # }
}


rdcrn_run <- function(opt){
  if (is.null(opt$output_prefix)){opt$output_prefix = 'output'}
  log_filename = paste0(output_prefix, "-log.txt")
  

  
  # #browser()
  out_filename = paste0(opt$output_prefix, "-with-phi.csv")
  deid_filename = paste0(opt$output_prefix, "-deid.csv")

  zz <- file(log_filename, open = "wt")
  sink(zz, type = "output",split=TRUE)
  sink(zz,type = "message")  
  #require(logr)

  if (is.null(opt$score_threshold)) opt$score_threshold <- 0.5
  tryCatch({
    d <- read.csv(opt$filename)
    
  },error = function(e){
    cat("Please check input file.\nPlease make sure to enclose the address information in quotation marks (e.g., “) if it contains commas.\n")
    stop()
  })
  
  if (!"address_date" %in% colnames(d)){
    cat("`address_date` is missing from data.\n`address_date` is recommended to record participant's address history.
        \nCreating empty `address_date` column...",'\n')
    d = d %>% mutate(address_date = NA) %>%
      relocate(address_date, .before = address)

    # Generate a temporary file path
    temp_file <- tempfile(fileext = ".csv")
    
    write.csv(d,temp_file , row.names = F, na = "")
    opt$filename = temp_file
  }
  
  
  cat("Geocoding data...","\n")
  #if there are geocoded data with non-missing addresses in data with `lat` and `lon` columns, we still do the geocoding
  if ("lat" %in% colnames(d) &"lon" %in% colnames(d)) {
    d_lat_lon = d %>%
      dplyr::filter(is.na(lat & lon) & !is.na(address))
  
   if (nrow(d_lat_lon) == 0){rm('d_lat_lon')}
  }

  # check if we have coordinates -- if not let's geocode first
  if (!"lat" %in% names(d) || !"lon" %in% names(d) || 'd_lat_lon' %in% ls()) {
    
    geocoded_df <- rdcrn_geocode(filename = opt$filename, score_threshold = opt$score_threshold, out_filename = out_filename)
    drivetime_input <- out_filename
  } else {
    cat("Input data has already been geocoded. Skip geocoding","\n")
    drivetime_input <- opt$filename
  }
  
  #Skip calculating deprivation index if testing
  cat("\nComputing deprivation index:\n")
  output_dep = rdcrn_dep_idx(drivetime_input, out_filename)
  write.csv(output_dep, file = out_filename,row.names = F, na = "")
  
  cat("\nFinished computing deprivation index\n")
  
  
  selected_site <- opt$site

  cat(paste0("\nComputing drivetimes to ", selected_site,":\n"))

  rdcrn_drivetime_selected_center(out_filename, out_filename,selected_site) 
  
  cat(paste0("\nFinished computing drivetimes to ", selected_site,"\n"))
  
  
  output = rdcrn_drivetime(out_filename, out_filename,"pcgc")$output %>%
    dplyr::mutate(version = "geoocoder_PCGC_0.0.1")
  
  include_deid_fields = opt$include_deid_fields
  if(is.null(include_deid_fields)){
    include_deid_fields = c("id","address_date","matched_state","precision","geocode_result","fraction_assisted_income",
                            "fraction_high_school_edu","median_income","fraction_no_health_ins","fraction_poverty","fraction_vacant_housing",
                            "dep_index","drivetime_selected_center","nearest_center_pcgc","drivetime_pcgc","version")
    field_list = include_deid_fields
  }else{
    field_list = unlist(strsplit(include_deid_fields,","))
    
    if (length(include_deid_fields) == 1 &  field_list > 1){
        field_list = trimws(field_list)
    }
  }
  
  
   

    
  output_deid = output %>% dplyr::select(any_of(field_list))
  
  
  
  
  write.csv(output,out_filename,row.names = F, na = "")
  write.csv(output_deid,deid_filename,row.names = F, na = "")
  
  sink(type = "output",split=TRUE)
  sink(type = "message")



  #return(list(output_df = output_pcgc_df, d_pcgc_list = d_pcgc_list, output_dep = output_dep))
  
  # if (opt$shiny){
  #   output_pcgc = rdcrn_drivetime(out_filename, out_filename,"pcgc")
  #   output_pcgc_df = output_pcgc$output
  #   d_pcgc_list = output_pcgc$d_to_pcgc_centers
  # 
  #   return(list(output_df = output_pcgc_df, d_pcgc_list = d_pcgc_list))
  # }
  

}
