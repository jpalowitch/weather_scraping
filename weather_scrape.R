library(RCurl)
library(XML)

weather_scrape <- function (years = c(2017), months = c(1),
                            verbose = TRUE) {
  
  # useful functions
  tonum <- function (x) {
            y <- strsplit(x, substr(x, nchar(x) - 1, nchar(x)))[[1]]
            return(as.numeric(substr(y, 1, nchar(y) - 1)))
          }
  formatChar <- function (x) {
    y <- strsplit(x, "\n\t,\n")[[1]]
    return(y)
  }
  
  currentDate <- as.numeric(strsplit(as.character(Sys.Date()), "-")[[1]])
  
  monthDays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  ndays <- sum(monthDays[months]) * length(years)
  
  # Data storage
  fulldf <- data.frame("Mean" = numeric(sum(ndays)),
                       "Min" = numeric(sum(ndays)),
                       "Max" = numeric(sum(ndays)),
                       "Precip" = numeric(sum(ndays)),
                       "Fog" = logical(sum(ndays)),
                       "Rain" = logical(sum(ndays)),
                       "Snow" = logical(sum(ndays)),
                       "Month" = numeric(sum(ndays)),
                       "Year" = numeric(sum(ndays)),
                       "Day" = numeric(sum(ndays)),
                       stringsAsFactors = FALSE)
  precipTypes <- c("Fog", "Rain", "Snow")
  # Url suffix/prefix
  urlsuff <- "https://www.wunderground.com/history/airport/KIGX/"
  urlpref <- "/DailyHistory.html?req_city=Chapel+Hill&req_state=NC&req_statename=North+Carolina&reqdb.zip=27516&reqdb.magic=1&reqdb.wmo=99999&MR=1"
  
  for (y in seq_along(years)) {
    
    year <- years[y]
    
      for (m in seq_along(months)) {
        
        month <- months[m]
        
        for (day in 1:monthDays[m]) { 
          
          # Preliminaries
          dfpos <- (y - 1) * sum(monthDays[months]) + 
            ifelse(m > 1, sum(monthDays[months[1:(m - 1)]]), 0) + day
          dateStr <- file.path(year, month, day)
          if (verbose) cat("scraping", dateStr, "\n")
          
          # Data table
          url <- paste0(urlsuff, dateStr, urlpref)
          wp <- getURLContent(url)
          doc <- htmlParse(wp, asText = TRUE) 
          docName(doc) <- url
          tmp <- readHTMLTable(doc, stringsAsFactors = FALSE)

          # Temps
          fulldf[dfpos, "Min"] <- tonum(tmp[[1]][4, ]$Actual)
          fulldf[dfpos, "Mean"] <-tonum(tmp[[1]][2, ]$Actual)
          fulldf[dfpos, "Max"] <- tonum(tmp[[1]][3, ]$Actual)
          
          # Precip level
          precipdf <- subset(tmp[[1]], tmp[[1]][ , 1] == "Precipitation")
          preciplevel <- precipdf$Actual[nchar(precipdf$Actual) > 1]
          preciplevel <- ifelse(length(preciplevel) == 1, tonum(preciplevel), 0)
          fulldf[dfpos, "Precip"] <- preciplevel
          
          # Precip type
          fulldf[dfpos, precipTypes] <- precipTypes %in% formatChar(tmp[[1]][21, ]$Actual)
          
          # Date
          fulldf[dfpos, "Month"] <- month
          fulldf[dfpos, "Year"] <- year
          fulldf[dfpos, "Day"] <- day
            
          if (year == currentDate[1] && month == currentDate[2] && day == currentDate[3])
            break
          
        }
        
      }
    
  }
  
  if (dfpos < nrow(fulldf))
    fulldf <- fulldf[-c((dfpos + 1):nrow(fulldf)), ]
  
  return(fulldf)
  
}