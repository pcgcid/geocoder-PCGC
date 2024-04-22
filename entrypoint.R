#!/usr/local/bin/Rscript
source("/app/utils.R")

dht::greeting()

# withr::with_message_sink("/dev/null", library(dplyr))
# withr::with_message_sink("/dev/null", library(digest))
# withr::with_message_sink("/dev/null", library(knitr))
# withr::with_message_sink("/dev/null", library(sf))


library(digest);library(dplyr);library(knitr);library(TeachingDemos)

doc <- "
      Usage:
        entrypoint.R [-h | --help] [-v | --version] [-i <filename> | --input-file <filename>] [-s <selected site> | --site <selected site>] [--site-list] [-o <output-prefix> | --output-file-prefix=<output-prefix>] [-f <fields> | --include-deid-fields=<fields>] [--force]

         
      Options:
        -h --help             Show available parameters.
        -v --version          Show version.
        -i --input-file <filename>
                              Specify input csv file.
        --force               Overwrite existing files if they exist.
        -s --site <selected site>
                              Specify site.
        --site-list           Print all available sites.
        -o <output-prefix> --output-file-prefix <output-prefix>
                              Specify output prefix ( By default, the prefix is `output`, which will generate output.log, output-phi.csv, output-deid.csv).
        -f --include-deid-fields <fields>
                              Specify list of fields to include in output.
                              Dafault fields: 'id','address_date','precision','geocode_result','fraction_assisted_income','fraction_high_school_edu','median_income','fraction_no_health_ins','fraction_poverty','fraction_vacant_housing','dep_index','drivetime_selected_center','nearest_center_pcgc','drivetime_pcgc','version'
        

      "
opt <- docopt::docopt(doc)

# Access the parsed arguments
input_file <- opt[["--input-file"]]
site <- opt[["--site"]]
output_prefix <- opt[["--output-file-prefix"]]
include_deid_fields <- opt[["--include-deid-fields"]]
force <- opt[["--force"]]

if (is.null(input_file)){
  stop("Input csv is missing. Please specify a .csv address file")
}

if (is.null(site)){
  stop("PCGC site argument is missing. Please use `docker run ghcr.io/dohn5r/geocoder_pcgc:0.0.1 --site-list` to see a list of available site")
}


sites = readr::read_csv('/app/pcgc_isochrones.csv')$abbreviation %>% unlist()
if (!site %in% sites){
  stop('The site you specified is not one of our available sites.
       \nRun `docker run ghcr.io/pcgcid/geocoder_pcgc:0.0.1 --site-list` to see all available sites')
}

if (!file.exists(input_file)){
  stop("Cannot find input file. Please check if the input file exists.")
}




if (is.null(output_prefix)){output_prefix = 'output'}

log_filename = paste0(output_prefix, "-log.txt")
out_filename = paste0(output_prefix, "-with-phi.csv")
deid_filename = paste0(output_prefix, "-deid.csv")

if (!force & (file.exists(log_filename) | file.exists(out_filename) | file.exists(deid_filename))){
  stop("One or more of the output files already exist. 
       \nPlease specify a different output prefix with `-o` or `--output-file-prefix` argument or use `--force` to overwrite existing output.
       \nExiting program...")
}

args_list = list(site = site, filename = input_file, output_prefix = output_prefix, score_threshold = 0.5, include_deid_fields = include_deid_fields)

if (!is.null(args_list$filename) & !is.null(args_list$site)){

  result = rdcrn_run(args_list)
  

  
  # etxtStart(dir = ".", file =log_filename )
  # rdcrn_run(args_list)
  # etxtStop()

  
  # writeLines(capture.output(rdcrn_run(args_list)),  log_filename)
  }


# Handle version option
if (opt$version | opt$ver) {
  cat("Version: geoocoder_PCGC_0.0.1\n")
  q(status = 0)
}

# Handle version option
if (opt[['--site-list']]) {
  centers_list <- read.csv('/app/pcgc_isochrones.csv')$abbreviation
  
  cat("List of available sites:\n")
  cat(paste(centers_list, collapse = "\n"),"\n")
  q(status = 0)
}



