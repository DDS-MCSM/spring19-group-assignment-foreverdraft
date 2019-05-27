#librarieeess
library(kableExtra)
library(XML)
library(httr)
library(ggplot2)
library(dplyr)
library(iptools)

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

#' get.scansioscans.io
#'
#' Obtener datos en crudo
#'
#' @return scansio dataframe
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
  #setting the pending "factor" fields as characters (malware + IP)
  func_df0["Malware"] <- lapply(func_df0["Malware"],as.character)
  func_df0["DstIP"] <- lapply(func_df0["DstIP"],as.character)
  #Cleans any possible NA values
  func_df0<-clean.df(func_df0)
  #outputs info console (usually debug purpose)
  extrainfo.df(func_df0)
  #calculations on the dataset
  # my_df1_heo <- func_df0[func_df0$Malware=="Heodo"]
  #my_df1_heo

  #TO BE SOLVED localization
  #testmaxmind<-addIPgeolocation(func_df0$DstIP)

  #locs <- data.frame(t(sapply(func_df0$DstIP,get.geocode)))
  #ggplot(locs, aes(x=long,y=lat))+
  #  borders("world", color="grey50", fill="grey90")+
  #  geom_point(color="red", size=3)+
  #  labs(x="",y="")+
  #  theme_bw() + theme(axis.text=element_blank(),axis.ticks=element_blank())+
  #  coord_fixed()

  #TO BE REMOVED tidy.risk %>% group_by(zone) %>%
  #
  #  summarise(total = sum(n_events),
  #            mean = mean(n_events),
  #            n = n())
  return(func_df0)
}

