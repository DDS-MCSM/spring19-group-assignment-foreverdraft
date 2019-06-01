#librarieeess
library(kableExtra)
library(XML)
library(httr)
library(ggplot2)
library(dplyr)
library(iptools)
library(chron)
library(rgeolocate)

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

#' create.directory
#'
#' create Directory?
#'
#' @return None
#'
#' @examples
#' TBD
#'
#' @export
create.directory <- function(){
  dir.data <- file.path(getwd(), "data")
  if (!dir.exists(dir.data)) {
    dir.create(dir.data)
  }
}

#' download.data
#'
#' Maxmind - Downloads the requested data
#'
#' @param site URL to get the data from
#' @param destfile local path to save downloaded data
#'
#' @return None
#'
#' @examples
#' TBD
#'
#' @export
download.data <- function(site,desfile) {
  if (!file.exists(desfile)){
    create.directory()
    download.file(url = site, destfile = desfile )
  }
}

#' get.feodo
#'
#' Obtener datos en crudo
#'
#' @return feodo dataframe
#'
#' @examples
#' TBD
#'
#' @export
get.feodo <- function(){
  feodo.url <- "https://feodotracker.abuse.ch/downloads/ipblocklist.csv"
  feodo.source <- file.path(getwd(), "data","feodo.csv")
  download.data(site = feodo.url, desfile = feodo.source)
  df.feodo <- read.csv(file=feodo.source, header=TRUE, sep=",", skip = 8)
  return(df.feodo)
}


#' generate.df
#'
#' Returns a DF with first n rows.
#'
#' @param df specific DataFrame to extract info from
#' @param n number of rows to extract from the data frame to return
#'
#' @return muestra which will be the requested n first rows of the DF
#'
#' @examples
#' TBD
#'
#' @export
generate.df <- function(df,nrows){
  muestra.df <- df(1:nrow(),)
  return(muestra.df)
}

#' clean.df
#'
#' filters a DF for invalid values
#'
#' @param df specific DataFrame to filter/clean info from
#'
#'
#' @examples
#' TBD
#'
#' @export
clean.df <- function(df3){
    df4<-df3[!is.na(df3[1]),]
    return(df4)
}

#' get geocodes MIGHT BE REMOVED
#'
#' auxiliary function to get coordinates of IP
#'
#'
#' @examples
#' TBD
#'
#' @export
get.geocode <- function(ip) {  # returns lat/long given an ip address
  url   <- "http://www.freegeoip.net"
  library(httr)
  xml   <- content(GET(url,path=paste("xml",ip,sep="/")),type="text/xml")
  xpath <- c(lat="//Latitude",long="//Longitude")
  sapply(xpath,function(xp) as.numeric(xmlValue(xml[xp][[1]])))
}

#' get maxmind database
#'
#' Database to use with geolocation function
#'
#'
#' @examples
#' TBD
#'
#' @export
get.maxmind <- function(){
  maxmind.url <- "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip"
  maxmind.file <- file.path(getwd(), "data", "maxmind.zip")
  download.file(url = maxmind.url, destfile = maxmind.file)
  zipfiles <- unzip(zipfile = maxmind.file, list = T)
  maxmind.source <- zipfiles$Name[grep(pattern = ".*GeoLite2-City-Blocks-IPv4.csv", x = zipfiles$Name)]
  dir.data <- file.path(getwd(), "data", maxmind.file)
  unzip(zipfile = maxmind.file, exdir = getwd(), files = maxmind.source)
  maxmind.source <- file.path(getwd(), maxmind.source)
  df.maxmind <- read.csv(maxmind.source, stringsAsFactors = FALSE)
  df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
  df.maxmind$rowname <- as.integer(row.names(df.maxmind))
  rm(maxmind.file, zipfiles)
  df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
  df.maxmind$rowname <- as.integer(row.names(df.maxmind))
}

#' extra info
#'
#' to output to the console a lot of shit about df
#' investigate dataframe console
#'
#' @examples
#' TBD
#'
#' @export
extrainfo.df <- function(func_df1){
  #viewinfo(df1) if needed more info
  cat("Type of dataset : ", str(class(func_df1)),"\n")
  cat("Dimension(row x column) : ", dim(func_df1),"\n")
  cat("Current lenght: ", length(func_df1),"& Object size:",object.size(func_df1),"\n")
  cat("Types of dataset fields: Printing\n", str(sapply(func_df1,class)),"\n print done \n")
  cat("Now let's see the values of all non-repeated fields\n")
  #sapply(func_df1,unique)
  cat("Structure of the dataset fields:", str(func_df1),"\n")
  #cat("Now let's see the number of IPs for each malware\n")
  #tapply(flags$IPs?,flags$Malware,summary)
  table(func_df1$Malware)
  class(func_df1$DetectedDate)
  #View(func_df1)
  nrow(func_df1)
  nchar(func_df1)
  #sample kable uncomment also first line with library
  #kable(head(funct_df1))
}


#' maxmind G4 adaptation
#'
#' same code adapted for G4, parsing IPs to obtain lat and long
#'
#'
#'
#' @examples
#' TBD
#'
#' @export
maxmindg4.df <- function(){
  # Dataframes used
  #  df2.tcp21		scans.io csv - hosts and sites
  #  df2.maxmind 	maxmind csv - IP lat long
  #  df2.scans		subset df.tcp21	- muestra x samples de Ips
  #  df2			df.scans+df.maxmind - with all positioning info
  #  df2.dst		destination IP (extracted from df) (will be removed)


  # Default parameters
  verbose2 <- TRUE
  seed2 <- 666
  scansio.url <- "https://opendata.rapid7.com/sonar.tcp/2019-04-04-1554350684-ftp_21.csv.gz"
  scope2 <- 500
  output2.file <- "geoftps.rds"

  # Initial Setup
  if (verbose2) print("[*] Initial setup")
  tini <- Sys.time()
  set.seed(666)
  dir.data <- file.path(getwd(), "data")
  if (!dir.exists(dir.data)) {
    if (verbose2) print("[*] Create data directory")
    dir.create(dir.data)
  }

  ##NOT NEEDED
  # scans.io - Obtener datos en crudo
  #if (verbose2) print("[*] Read RAW data from scans.io")
  #scansio.source <- file.path(getwd(), "data","scans.io.tcp21.csv")
  #scansio.file.gz <- paste(scansio.source, ".gz", sep = "")
  #download.file(url = scansio.url, destfile = scansio.file.gz)
  #if(file.exists(scansio.source))
  #  rm(scansio.source)
  #R.utils::gunzip(scansio.file.gz)
  #rm(scansio.file.gz)
  #df2.tcp21 <- read.csv(scansio.source, stringsAsFactors = FALSE)

  if (verbose2) print("[*] Read data from source")
  my_dfsc1 <- analysis.df()

  # Maxmind - Obtener datos en crudo (city)
  if (verbose2) print("[*] Read RAW data from MaxMind")
  maxmind.url <- "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip"
  maxmind.file <- file.path(getwd(), "data", "maxmind.zip")
  download.file(url = maxmind.url, destfile = maxmind.file)
  zipfiles <- unzip(zipfile = maxmind.file, list = T)
  maxmind.source <- zipfiles$Name[grep(pattern = ".*GeoLite2-City-Blocks-IPv4.csv", x = zipfiles$Name)]
  unzip(zipfile = maxmind.file, exdir = dir.data, files = maxmind.source)
  maxmind.source <- file.path(getwd(), "data", maxmind.source)
  df2.maxmind <- read.csv(maxmind.source, stringsAsFactors = FALSE)
  rm(maxmind.file, zipfiles)

  # Seleccionamos una muestra de scans
  if (verbose2) print("[*] Subseting scans data set")
  my_dfsc1$DstIP <- iptools::ip_to_numeric(my_dfsc1$DstIP)
  #df2.tcp21$daddr.num <- iptools::ip_to_numeric(df2.tcp21$daddr)
  #muestra <- sample(1:nrow(df2.tcp21), scope2)
  #df2.scans <- df2.tcp21[muestra,]
  df2.scans <- my_dfsc1 #ALL scope
  #rm(muestra)

  # Para geolocalizar una IP en un rango comprobaremos si está entre la primera
  # y la ultima ip de cada rango en MaxMind.

  # Maxmind elegante
  if (verbose2) print("[*] Expanding MaxMind network ranges")
  df2.maxmind <- cbind(df2.maxmind, iptools::range_boundaries(df2.maxmind$network))
  df2.maxmind$rowname <- as.integer(row.names(df2.maxmind))

  # Usamos multiples cpu's para geolocalizar IPs en rangos
  if (verbose2) print("[*] Foreach IP (source and destination) identify network range using parallel computing")
  #no_cores <- parallel::detectCores() - 1
  #cl <- parallel::makeCluster(no_cores)
  #parallel::clusterExport(cl, "df2.maxmind")
  df2.scans$sloc <- sapply(df2.scans$DstIP,
                           function(ip)
                             which((ip >= df2.maxmind$min_numeric) &
                                     (ip <= df2.maxmind$max_numeric)))
  #df2.scans$dloc <- sapply(df2.scans$daddr.num,
  #                         function(ip)
  #                           which((ip >= df2.maxmind$min_numeric) &
  #                                   (ip <= df2.maxmind$max_numeric)))
  #parallel::stopCluster(cl)
  #rm(cl, no_cores)

  # Join and tidy data frame (source address)
  if (verbose2) print("[*] Joining source IP's with geolocation data")
  df2 <- dplyr::left_join(df2.scans, df2.maxmind, by = c("sloc" = "rowname"))
  df2 <- dplyr::select(df2, DetectedDate, DstIP, DstPort, LastOnlineDate, Malware,
                       DetectedWeekday, country_name,
                       latitude, longitude, is_anonymous_proxy, is_satellite_provider)
  #names(df2) <- c("timestamp_ts", "saddr", "slatitude", "slongitude",
  #                "accuracy_radius", "is_anonymous_proxy", "is_satellite_provider")
  #df2 <- dplyr::select(df2, timestamp_ts, saddr, latitude, longitude, accuracy_radius,
  #                     is_anonymous_proxy, is_satellite_provider)
  #names(df2) <- c("timestamp_ts", "saddr", "slatitude", "slongitude",
  #                "accuracy_radius", "is_anonymous_proxy", "is_satellite_provider")

  # Join and tidy data frame (destination address)
  #if (verbose2) print("[*] Joining destination IP's with geolocation data")
  #suppressMessages(library(dplyr))
  #df2.dst <- df2.scans %>%
  #  left_join(df2.maxmind, by = c("dloc" = "rowname")) %>%
  #  select(daddr, latitude, longitude)
  #names(df2.dst) <- c("daddr", "dlatitude", "dlongitude")
  #df2 <- dplyr::bind_cols(df2, df2.dst)
  #rm(df2.dst, df2.scans)

  # Set categoric variables as factors
  if (verbose2) print("[*] Tidy data and save it")
  df2$is_anonymous_proxy <- as.factor(df2$is_anonymous_proxy)
  df2$is_satellite_provider <- as.factor(df2$is_satellite_provider)
  saveRDS(object = df2, file = file.path(getwd(), "data", output2.file))
  fini <- Sys.time()

  # Summary
  fini - tini
  return(df2)
  summary(df2)
}

#' Parse botnet
#'
#' "main" file which downloads the file if needed
#' and processes it for a better understanding
#'
#'
#' @examples
#' TBD
#'
#' @export
analysis.df <- function(){
  func_df0 <- get.feodo()
  #renaming some ugly columns
  colnames(func_df0)[colnames(func_df0)=="X..Firstseen"] <- "DetectedDate"
  colnames(func_df0)[colnames(func_df0)=="LastOnline"] <- "LastOnlineDate"
  #Parsing the Time field
  func_df0$LastOnlineDate<-strptime(func_df0$LastOnlineDate,format="%Y-%m-%d")
  func_df0$DetectedDate<-strptime(func_df0$DetectedDate,format="%Y-%m-%d %H:%M:%S")
  #format date
  func_df0["DetectedDate"] <- lapply(func_df0["DetectedDate"],as.POSIXct)
  func_df0["LastOnlineDate"] <- lapply(func_df0["LastOnlineDate"],as.POSIXct)
  #setting the pending "factor" fields as characters (malware + IP)
  func_df0["Malware"] <- lapply(func_df0["Malware"],as.character)
  func_df0["DstIP"] <- lapply(func_df0["DstIP"],as.character)
  #Cleans any possible NA values
  func_df0<-clean.df(func_df0)

  #adding weekdays as a column
  func_df0<-cbind(func_df0,weekdays(func_df0$DetectedDate))
  colnames(func_df0)[colnames(func_df0)=="weekdays(func_df0$DetectedDate)"] <- "DetectedWeekday"

  #loading countries as extra column
  filecountry <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
  resultscountry <- rgeolocate::maxmind(func_df0$DstIP, filecountry, c("continent_name", "country_code", "country_name"))
  func_df0<-cbind(func_df0,resultscountry)

  #outputs info console (usually debug purpose)
  extrainfo.df(func_df0)
  return(func_df0)
}
