#!/usr/local/bin/Rscript
source("/app/utils.R")

dht::greeting()

withr::with_message_sink("/dev/null", library(dplyr))
withr::with_message_sink("/dev/null", library(digest))
withr::with_message_sink("/dev/null", library(knitr))
# withr::with_message_sink("/dev/null", library(sf))


doc <- "
      Usage:
        entrypoint.R [-h | --help] [-v | --version] [-i <filename> | --input-file <filename>] [-s <selected site> | --site <selected site>] [--site-list] [-o <output-file-prefix> | --output-file-prefix=<output-prefix>] [-f <fields> | --include-deid-fields=<fields>]
        entrypoint.R (-h | --help)
        entrypoint.R (-v | --version)




         
      Options:
        -h --help             Show available parameters.
        --shiny               Start shiny server on port 3838.
        -v --version          Show version.
        -i --input-file <filename>
                              Specify input csv file.
        -s --site <selected site>
                              Specify site.
        --site-list           Print all available sites.
        -o <out_filename> --output-file-prefix <out_filename>
                              Specify output prefix (it will generate output.log, output-phi.csv, output-deid.csv).
        -f --include-deid-fields <fields>
                              Specify list of fields to include in output.

      "
opt <- docopt::docopt(doc)

# Access the parsed arguments
input_file <- opt[["--input-file"]]
site <- opt[["--site"]]
output_prefix <- opt[["--output-file-prefix"]]
include_deid_fields <- opt[["--include-deid-fields"]]

if (is.null(include_deid_fields)){
  include_deid_fields = c("id","date","precision","geocode_result","fraction_assisted_income",
  "fraction_high_school_edu","median_income","fraction_no_health_ins","fraction_poverty","fraction_vacant_housing",
  "dep_index","drive_time_pcgc_chop","nearest_center_pcgc","distance_pcgc")
}

args_list = list(site = site, filename = input_file, out_filename = output_prefix, score_threshold = 0.5, include_deid_fields = include_deid_fields)



if (!is.null(args_list$filename) & !is.null(args_list$site)){
  rdcrn_run(args_list) }

if (opt$shiny) {
  shiny::runApp(appDir="/app",host='0.0.0.0',port=3838)
}

# Handle version option
if (opt$version | opt$ver) {
  cat("Version 3.3.0\n")
  q(status = 0)
}

# Handle version option
if (opt[['--site-list']]) {
  centers_list <- read.csv('/app/pcgc_isochrones.csv')$abbreviation
  
  cat("List of available sites:\n")
  cat(paste(centers_list, collapse = "\n"),"\n")
  q(status = 0)
}

