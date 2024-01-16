#!/usr/local/bin/Rscript
source("/app/utils.R")

dht::greeting()

withr::with_message_sink("/dev/null", library(dplyr))
withr::with_message_sink("/dev/null", library(digest))
withr::with_message_sink("/dev/null", library(knitr))
# withr::with_message_sink("/dev/null", library(sf))


doc <- "
      Usage:
        entrypoint.R [<filename>] [<out_filename>] [<score_threshold>] [--shiny] 
        entrypoint.R (-h | --help)
         
      Options:
        -h --help     Show this screen.
        --shiny       Start shiny server on port 3838.
      "
opt <- docopt::docopt(doc)
if (!is.null(opt$filename)){
  rdcrn_run(opt) }

if (opt$shiny) {
  shiny::runApp(appDir="/app",host='0.0.0.0',port=3838)
}


