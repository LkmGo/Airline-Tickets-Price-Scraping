library(RSelenium, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(RCurl, quietly = TRUE)
library(bitops, quietly = TRUE)
library(XML, quietly = TRUE)
library(stringr, quietly = TRUE)
library(rvest, quietly = TRUE)
require(Hmisc, quietly = TRUE)
require(dplyr, quietly = TRUE)

EtihadInput<- function(destination,departureDate,arrivalDate){
  if(destination %in% c("IXZ","VTZ","GAU","GAY","GOI","SXR","IXE","NAG","BHO","IMF","BBI","CJB","IXM","ATQ","TRZ","LKO","VNS")){
    print(paste(c(destination," not serviced by Etihad Airways"), collapse=""))
    return("no flights")}else{
      #destination<-"DEL"
      #departureDate<-"12/14/2016"
      #arrivalDate<-"1/13/2017"
      
      url<-"http://www.etihad.com/en-us/"
      remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = list(phantomjs.binary.path = "/Users/lkm/phantomjs"))
      remDr$open()
      remDr$navigate(url)
      departureDate<-as.Date(departureDate,"%m/%d/%Y")
      departureDate<-format(departureDate,"%d/%m/%Y")
      arrivalDate<-as.Date(arrivalDate,"%m/%d/%Y")
      arrivalDate<-format(arrivalDate,"%d/%m/%Y")
      wxbox <- remDr$findElement(using = 'id', "frm_2012158061206151234")
      wxbox$sendKeysToElement(list(key="down_arrow"))
      Sys.sleep(3)
      wxbox$sendKeysToElement(list("BOS"))
      Sys.sleep(3)
      wxbox$sendKeysToElement(list(key="enter"))
      Sys.sleep(3)
      wxbox <- remDr$findElement(using = 'id', "frm_20121580612061235")
      wxbox$sendKeysToElement(list(key="down_arrow"))
      Sys.sleep(3)
      wxbox$sendKeysToElement(list(destination))
      Sys.sleep(3)
      wxbox$sendKeysToElement(list(key="enter"))
      Sys.sleep(3)
      wxbox <- remDr$findElement(using = 'id', "frm_2012158061206151238")
      wxbox$clearElement()
      Sys.sleep(3)
      wxbox$sendKeysToElement(list(departureDate, "\uE007"))
      wxbox <- remDr$findElement(using = 'id', "frm_2012158061206151239")
      wxbox$clearElement()
      Sys.sleep(3)
      wxbox$sendKeysToElement(list(arrivalDate, "\uE007"))
      Sys.sleep(3)
      webElem<-remDr$findElement(using = 'css selector', ".btnSearchFlight")
      webElem$clickElement()
      Sys.sleep(10)
      remDr$screenshot(display = TRUE)
      Etihad <- read_html(remDr$getPageSource()[[1]])
      #########################################################################################################
      
      #Extracting departure and arrival locations.
      #"departure_code" includes codes of outbound and inbound trips, added.
      #And same for "return_code" as well.
      departure_code <- html_nodes(Etihad,".airportCode") %>% html_text()
      return_code <- html_nodes(Etihad,".ItinHeaderTo div:nth-child(1)") %>% html_text()
      
      #Extracting departure time.
      departure_time <- html_nodes(Etihad,".ItinHeaderFrom") %>% html_text()
      dep_time <- grep("[0-9]+[:]+[0-9]+", x = departure_time, value = T)
      dep_time <- gsub("[A-Z]+",replacement = "",x = dep_time)
      
      #Extracting arrival time.
      arrival_time <- html_nodes(Etihad,".ItinHeaderTo div") %>% html_text()
      arr_time <- grep("[0-9]+[:]+[0-9]+", x = arrival_time, value = T)
      
      #Extracting outbound prices. Sometimes both, outbound and inbound trips gives either
      #3 or 4 columns for prices. So following code checks it and calculates min prices 
      #accordingly.
      outboundPrices <- html_nodes(Etihad,"#dtcontainer-outbounds .price-cell") %>% html_text()
      outboundPrices <- trimws(gsub("\n","",outboundPrices))
      outboundPrices[outboundPrices == "Sold out"] <- NA
      outboundPrices <- as.numeric(str_extract(outboundPrices,"[0-9]{3,4}+[.]+[0-9]{2}"))
      lp_ob <- length(outboundPrices)
      
      outboundCodes <- html_nodes(Etihad,"#dtcontainer-outbounds .alignAfterMoreThanOneDayIndicator") %>%
        html_text() %>%
        length()
      
      if (lp_ob/outboundCodes == 3){
        min_price_ob <- unname(tapply(X = outboundPrices, cut(1:lp_ob, lp_ob/3), FUN = min,na.rm = T ))  
      }else{
        min_price_ob <- unname(tapply(X = outboundPrices, cut(1:lp_ob, lp_ob/4), FUN = min,na.rm = T ))  
      }
      min_price_ob[min_price_ob == "Inf"] <- NA
      
      inboundPrices <- html_nodes(Etihad,"#dtcontainer-inbounds .price-cell") %>% html_text()
      inboundPrices <- trimws(gsub("\n","",inboundPrices))
      inboundPrices[inboundPrices == "Sold out"] <- NA
      inboundPrices <- as.numeric(str_extract(inboundPrices,"[0-9]{3,4}+[.]+[0-9]{2}"))
      lp_ib <- length(inboundPrices)
      
      inboundCodes <- html_nodes(Etihad,"#dtcontainer-inbounds td.ItinHeaderFrom") %>%
        html_text() %>%
        length()
      
      if (lp_ib/inboundCodes == 4){
        min_price_ib <- unname(tapply(X = inboundPrices, cut(1:lp_ib, lp_ib/4), FUN = min,na.rm = T ))  
      }else{
        min_price_ib <- unname(tapply(X = inboundPrices, cut(1:lp_ib, lp_ib/3), FUN = min,na.rm = T ))  
      }
      
      min_price_ib[min_price_ib == "Inf"] <- NA
      Min_prices <- c(min_price_ob,min_price_ib)
      
      #Extracting dates. Following code does not show blanks for same day arrival. So returns less elements than actual.
      dates <- html_nodes(Etihad,".ItinHeaderTo") %>% html_text()
      dates <- grep("[0-9]+[:]+[0-9]+", x = dates, value = T)
      
      #Etihad website does not show departure dates and arrival dates. So to make a final data frame we take
      #dates from the date variables of the code itself.
      #As it does not show the dates, from the website itself we cannot say that on which date the flight reaches 
      #the destination. However, the website shows that after how many days, it'll reach. So we add that numeric
      #value of days, called "lateness", to departure date to find the arrival date of the outbound flight to its destination.
      lateness <- str_extract(dates,"[Arrival,]{8}+|[Arrival]{7}")
      lateness[lateness == "Arrival"] <- 1
      lateness[lateness == "Arrival,"] <- 2
      lateness[is.na(lateness)] <- 0
      lateness <-  as.numeric(lateness)
      lateness_ob <- lateness[1:outboundCodes]
      lateness_ib <- lateness[1:inboundCodes]
      
      DepDate_fB <- rep(departureDate, outboundCodes)
      ArrDate_tD <- as.Date(DepDate_fB, "%d/%m/%Y") + lateness_ob
      
      DepDate_fD <- rep(arrivalDate, inboundCodes)
      ArrDate_tB <- as.Date(DepDate_fD, "%d/%m/%Y") + lateness_ib
      
      #Following are the Departure and Arrival dates for all the flights.
      #That means "DepDate = Dates from BOSTON to Destination + from Destination to BOSTON".
      #That means "ArrDate = Dates from BOSTON to Destination + from Destination to BOSTON".
      DepDate <- as.character(c(DepDate_fB,DepDate_fD))
      ArrDate <- as.character(c(ArrDate_tD,ArrDate_tB))
      AllDates <- cbind(as.data.frame(DepDate),as.data.frame(ArrDate))
      
      
      #Final data frame.
      EtihadData <- as.data.frame(cbind(Airline=rep("Etihad",length(departure_code)), dep_time, arr_time, 
                                        departure_code, return_code, DepDate, ArrDate, Min_prices))
      
      colnames(EtihadData) <- c("Airline","DepTime", "ArrTime", "From", "To", "DepDate", "ArrDate", "Min_Price")
      
      
      #Extracting minimum prices for outbound and inbound flights. Addition of minimum values
      #of each will give total minimum price for round trip.
      f <- EtihadData[which(EtihadData$From == "BOS"),]
      t <- EtihadData[which(EtihadData$From != "BOS"),]
      colnames(f)[8] <- "Min_Price_1"
      colnames(t)[8] <- "Min_Price_2"
      f <- arrange(f,Min_Price_1)
      t <- arrange(t,Min_Price_2)
      EtihadData <- cbind(f[1,],t[1,])
      Min_Price <- as.numeric(as.character(EtihadData$Min_Price_1))+as.numeric(as.character(EtihadData$Min_Price_2))
      EtihadData <- cbind(EtihadData,Min_Price)
      EtihadData <- EtihadData[,-c(8,16)]
      ScrapeTime <- format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
      EtihadData <- cbind(EtihadData, ScrapeTime)
      
      #colnames(EtihadData) <- c("Airline", "DepTime", "ArrTime", "From", "To", "DepDate", 
      #                          "ArrDate", "ReturnAirline", "DepTime", "ArrTime", "From", 
      #                         "To", "DepDate", "ArrDate", "Min_Price", "ScrapeTime")
      return(EtihadData)
      remDr$closeall()
      remDr$close()
    }
}

AirlinePrices<-function(d1,d2){
  Airports<-c("IXZ","VTZ","GAU","GAY","DEL","GOI","AMD","SXR","BLR","IXE","COK","CCJ","TRV","BHO","BOM","NAG","IMF","BBI","ATQ","JAI","MAA","CJB","IXM","TRZ","HYD","LKO","VNS","CCU")
  AirlineDatabase<-data.frame()
  for(i in seq(1:length(Airports))){
    r<-EtihadInput(Airports[i],d1,d2)
    if(sum(!is.na(match("no flights",r)))!=1){
      AirlineDatabase<-rbind(AirlineDatabase,r)
      Sys.sleep(2)
      
    }
  }
  if(file.exists("/Users/lkm/Desktop/data course/project/1.csv")){
    loadDb<-read.csv("/Users/lkm/Desktop/data course/project/1.csv",header=TRUE,stringsAsFactors=F)
    loadDb<-loadDb[,-1]
    AirlineDatabase<-rbind(loadDb,AirlineDatabase)
    write.csv(AirlineDatabase,"/Users/lkm/Desktop/data course/project/1.csv")
  }else{
    write.csv(AirlineDatabase,"/Users/lkm/Desktop/data course/project/1.csv")
  }
  return(AirlineDatabase)
}
  
AirlinePrices("12/14/2016","01/13/2017")

