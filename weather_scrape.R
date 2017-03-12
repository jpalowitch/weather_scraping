library(RCurl)
library(XML)

# Data storage
mnths <- c(1, 2, 3)
ndays <- c(31, 28, 12)
yrs <- c(2016, 2017)
yeardf <- data.frame("Mean" = numeric(sum(ndays)),
                     "Min" = numeric(sum(ndays)),
                     "Max" = numeric(sum(ndays)),
                     "Snowfall" = numeric(sum(ndays)))
alldata <- rep(list(yeardf), length(yrs))

# Url suffix/prefix
urlsuff <- "https://www.wunderground.com/history/airport/KIGX/"
urlpref <- "/DailyHistory.html?req_city=Chapel+Hill&req_state=NC&req_statename=North+Carolina&reqdb.zip=27516&reqdb.magic=1&reqdb.wmo=99999&MR=1"

for (y in seq_along(years)) {
  
  year <- yrs[y]
  
    for (m in seq_along(mnths)) {
      
      month <- mnths[m]
      
      for (day in 1:ndays[m]) { 
        
        url <- paste0(urlsuff, file.path(year, month, day), urlpref)
        wp <- getURLContent(url)
        doc <- htmlParse(wp, asText = TRUE) 
        docName(doc) <- url
        tmp <- readHTMLTable(doc)
        
      }
      
    }
  
}