library('rvest')
library('httr')
library('readr')
library('pbapply')
library('dplyr')
library('lubridate')

options(stringsAsFactors = FALSE)

#### Scrape the files names of the CSV-files and download the files ####

get_data <- function(x, path = "data-raw") {
   if (!file.exists(path)) dir.create(path, showWarnings = FALSE, recursive = TRUE)
   download.file(x, file.path(path, basename(x)), mode = "wb", quiet = TRUE)
}

get_file_names <- function(url) {

  res <- GET(url)
  stop_for_status(res)

  files <- content(res) %>% html_nodes("td a") %>% html_attr("href")
  files <- sort(files[grep("\\_CSV.zip$", files)])
  files
}

baseURL <- "http://sira.arpat.toscana.it/sira/inspire/data/qualita_aria/aria_web.php"

DataFiles <- get_file_names(baseURL)

z <- pbsapply(DataFiles, get_data)

#### Import the CSV-Files ####

m <- list.files("data-raw", pattern = "\\.zip$")

# remove "GR_DATI_QA_2011_CSV.zip",
# file "GR-SONNINO_1_2011.csv" has zero values
m <- m[m != "GR_DATI_QA_2011_CSV.zip"]

ReadZip <- function(zipfile) {

  import_data <- function(m, col_types) {
    # Get the full name of the file
    file <- file.path(zipdir, m)

    cc <- col_types

    loc <- readr::locale(date_names = "it", decimal_mark = ".")

    z <- lapply(file, function(x) read_delim(x, delim = ";", skip = 1,
                                             col_types = cc,
                                             locale = loc,
                                             na = c("", "NA", "NaN"),
                                             col_names = FALSE))
    z <- do.call(rbind, z)
    return(z)
  }

  # Create a name for the dir where we'll unzip
  zipdir <- tempfile()
  # Create the dir using that name
  dir.create(zipdir)
  # Unzip the file into the dir
  unzip(file.path("data-raw", zipfile), exdir=zipdir, junkpaths = TRUE)
  # Get the files into the dir
  files <- list.files(zipdir)
  files <- files[grep("\\.csv$", files)]

  # http://stackoverflow.com/a/7664655
  toMatch <- c("_PM10_", "_PM2.5_")
  is_PM <- grep(paste(toMatch, collapse = "|"), files)

  inq <- files[-is_PM]
  inq <- import_data(inq, col_types = 'ccdddd')

  pm <- files[is_PM]
  pm <- import_data(pm, col_types = 'ccddd')

  out <- list(inq = inq, pm = pm)
  return(out)
}

fin <- pblapply(m, ReadZip)

#### Extract ####

at_inq <- do.call(rbind, sapply(fin, "[", "inq"))
rownames(at_inq) <- NULL

at_pm <- do.call(rbind, sapply(fin, "[", "pm"))
rownames(at_pm) <- NULL

rm(fin)

at_inq <- tbl_df(at_inq) %>%
            filter(X1 != "LI-GIARDINI-PUBBLICI" & X2 != "PTS") %>%
            mutate(stazione  = X1,
                   parameter = X2,
                   year      = as.numeric(substr(X3, 1, 4)),
                   month     = as.numeric(substr(X3, 5, 6)),
                   day       = as.numeric(substr(X3, 7, 8)),
                   hour      = as.numeric(X4),
                   value     = X5,
                   valid     = X6) %>%
            select(stazione, parameter, year, month,
                   day, hour, value, valid)

devtools::use_data(at_inq, overwrite = TRUE)

at_pm <- tbl_df(at_pm) %>%
          mutate(stazione  = X1,
                 parameter = X2,
                 year      = as.numeric(substr(X3, 1, 4)),
                 month     = as.numeric(substr(X3, 5, 6)),
                 day       = as.numeric(substr(X3, 7, 8)),
                 value     = X4,
                 valid     = X5) %>%
          select(stazione, parameter, year, month,
                 day, value, valid)

devtools::use_data(at_pm, overwrite = TRUE)
