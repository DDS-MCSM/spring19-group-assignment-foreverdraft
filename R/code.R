#' Title
#'
#' @details This are the details
#' @return
#' @export
#'
#' @examples
sample_function <- function() {
  print("Hello world")
}


# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

# Create.directory
create.directory <- function(){
  dir.data <- file.path(getwd(), "data")
  if (!dir.exists(dir.data)) {
    dir.create(dir.data)
  }
}

# Maxmind - Downloads the requested data
download.data <- function(site,destfile) {
  create.directory()
  destination.path <- file.path(getwd(),"data",destfile)
  if (!file.exists(destination.path)){
    download.file(url = site, destfile = destination.path )
  }
}

# scans.io - Obtener datos en crudo
get.scansio <- function(){
  scansio.url <- "https://opendata.rapid7.com/sonar.tcp/2019-04-04-1554350684-ftp_21.csv.gz"
  scansio.source <- file.path(getwd(), "data","scans.io.tcp21.csv")
  scansio.file.gz <- paste(scansio.source, ".gz", sep = "")
  download.data(site = scansio.url, destfile = scansio.file.gz)
  R.utils::gunzip(scansio.file.gz)
  df.tcp21 <- read.csv(scansio.source, stringsAsFactors = FALSE)
  return(df.tcp21)
  rm(scansio.file.gz)
}

# Maxmind - Obtener datos en crudo (city)
get.maxdata <- function(){
  maxmind.file <- "maxmind.zip"
  download.data(site = "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip", destfile = maxmind.file)
  zipfiles <- unzip(zipfile = maxmind.file, list = T)
  maxmind.source <- zipfiles$Name[grep(pattern = ".*GeoLite2-City-Blocks-IPv4.csv", x = zipfiles$Name)]
  unzip(zipfile = maxmind.file, exdir = dir.data, files = maxmind.source)
  maxmind.source <- file.path(getwd(), "data", maxmind.source)
  df.maxmind <- read.csv(maxmind.source, stringsAsFactors = FALSE)
  return(df.maxmind)
  rm(maxmind.file, zipfiles)
}

# Returns a DF with first n rows.
generate.df <- function(df,nrows){
  muestra.df <- df(1:nrow(),)
  return(muestra.df)
  rm(muestra)
}
