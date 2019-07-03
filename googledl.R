set.seed(123)
library(tseries)
library(xts)
library(zoo)
library(gtrendsR)


startmon <- function(x){
  return(as.Date.factor(format(x , "%Y-%m-01")))
}

endmon <- function(x){
  return((startmon(startmon(as.Date.factor(x+32))) - 1))
}


## download gtrends data on a monthly basis ##

searches = list()
k <- 1
start.date <- as.Date.factor("2007-01-01")                          ## set starting date
end.date <- as.Date.factor("2008-12-31")                      ## set ending date
date <- start.date

while(date < end.date){                               ## might take a while
  a <- (c(as.character(date),as.character(endmon(date))))
  searches[[k]] <- gtrends(keyword = "stock market" ,                ## change keywords, categories etc to preference
                           geo ="US" ,                          
                           time = paste(a[1] , a[2]), 
                           low_search_volume = TRUE,
                           category = 784)
  k = k+1
  date <- startmon(as.Date.factor(date + 32))
}

length(searches)

### append start dates to each list object ###
k<-1
while(k<=length(searches)){
  searches[[k]]$startdate <- searches[[k]]$interest_over_time$date[1] 
  k <- k+1
}

## download monthly hit volume scores for the same period ##

## this part is necessary if period is greater than 90 days, otherwise skip it ## 

period <- c(as.character(start.date) , as.character(end.date))  

searchesmonthly <- gtrends(keyword = "stock market" ,
                           geo = "US" ,
                           time = paste(period[1] , period[2]) ,
                           low_search_volume = TRUE,
                           category = 784)

### normalize daily hit scores ###

monthlyhitscore <- searchesmonthly$interest_over_time$hits/100

k <- 1
hits <- list()

hits[[1]] <- searches[[1]]$interest_over_time$hits*monthlyhitscore[1]
hits.f <- c(hits[[1]])
k <- k+1
while(k<=length(searches)){
  hits[[k]] <- searches[[k]]$interest_over_time$hits*monthlyhitscore[k]
  hits.f <- c(hits.f , hits[[k]])
  k <- k+1
}

dates <- seq(as.Date("2007-01-01") , length = length(hits.f) , by = "days")
hits.f <- as.xts(hits.f , order.by = dates)/max(hits.f)
plot(hits.f)

write.csv("ghits.csv")     ## write hit data into a csv file
##############################################