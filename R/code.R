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
clean.df <- function(df){
    rm(df)
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
analysis.df <- function(df1){
  df1 <- get.feodo()

  cat("Current df size: ", object.size(df1),"\n")
  cat("Current names df: ", names(df1),"\n")
  cat("Current lenght: ", length(df1),"\n")
  #nrow()
  #nchar()
}

