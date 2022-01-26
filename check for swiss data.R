library(RSelenium)
library(RCurl)
library(bitops)
library(XML)
library(stringr)
library(lubridate)
library(rvest)
require(Hmisc)

SwissAirlinesInput<- function(destination,departureDate,arrivalDate){
  departureDate<-as.Date(departureDate,"%m/%d/%Y")
  departureDate<-format(departureDate,"%Y-%m-%d")
  arrivalDate<-as.Date(arrivalDate,"%m/%d/%Y")
  arrivalDate<-format(arrivalDate,"%Y-%m-%d")
  url<- c("https://www.swiss.com/gb/en/Book/Outbound/",
          "BOS","-",destination,
          "/from-",departureDate,"/to-",arrivalDate,
          "/adults-1/children-0/infants-0/class-economy/al-LX/")
  url<-paste(url,collapse="")
  remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = list(phantomjs.binary.path = "/Users/lkm/phantomjs"))
  remDr$open()
  remDr$navigate(url)
  Sys.sleep(10)
  h<- read_html(remDr$getPageSource()[[1]])
  a<-html_nodes(h,".js-notification-close")%>%html_text()
  if(length(a)>2){Swissdata<-"no flights"
  }else{
    loc<-unlist(str_extract_all(html_nodes(h,".is-active .step-title")%>%html_text(),"[A-Z][A-Z][A-Z]"))
    From<-loc[1]
    To<-loc[2]
    inf<-html_nodes(h,".book-bundle-flightentry--time div")%>%html_text()
    inf<-gsub("\n","",inf)
    day<-sum(as.numeric(unlist(str_extract_all(inf,"[+][1-9]"))))
    depinf<-inf[which(str_detect(inf,loc[1])==TRUE)]
    arrinf<-inf[which(str_detect(inf,loc[2])==TRUE)]
    DepTime<-unlist(str_extract_all(depinf,"[0-9][0-9][:][0-5][0-9]"))
    ArrTime<-unlist(str_extract_all(arrinf,"[0-9][0-9][:][0-5][0-9]"))
    date<-html_nodes(h,".is-active .book-matrix--tab-link--date")%>%html_text()
    DepDate<-as.Date(str_split_fixed(date," ",2)[,2],"%d/%m/%Y")
    ArrDate<-DepDate+day
    NameOfAirline<-rep("SWISS",length(DepTime))
    price<-html_nodes(h,".js-book-bundling--button")%>%html_text()
    price<-str_split_fixed(str_split_fixed(unlist(gsub("\n","",price)),"USD",2)[,2],"S",2)[,1]
    price<-gsub(",","",gsub(" ","",price))
    price[which(price=="")]<-Inf
    PriceInUsd<-vector(length = length(DepTime))
    for (i in 1:length(DepTime)) {
      PriceInUsd[i]<-min(price[i:6*i])
    }
    Swissdata<-data.frame(Airline=NameOfAirline,DepTime=DepTime,ArrTime=ArrTime,From=From,To=To,DepDate=DepDate,ArrDate=ArrDate,Min_Price=PriceInUsd,stringsAsFactors = FALSE)
    
    
    wxbox<-remDr$findElements(using = 'class', 'book-bundle-button--price')
    buttonind<-vector("character")
    for(i in seq(1:length(wxbox))){
      if(gsub("[^0123456789]","",wxbox[[i]]$getElementText())==PriceInUsd)
      {
        wxbox[[i]]$clickElement()
      }
    }
    Sys.sleep(5)
    remDr$findElement(using = 'css selector', '.btn-submit')$clickElement()
    h<- read_html(remDr$getPageSource()[[1]])
    Sys.sleep(10)
    a<-html_nodes(h,".js-notification-close")%>%html_text()
    if(length(a)>2){Swissdata<-"no flights"
    }else{
      loc<-unlist(str_extract_all(html_nodes(h,".is-active .step-title")%>%html_text(),"[A-Z][A-Z][A-Z]"))
      From<-loc[1]
      To<-loc[2]
      inf<-html_nodes(h,".book-bundle-flightentry--time div")%>%html_text()
      inf<-gsub("\n","",inf)
      day<-sum(as.numeric(unlist(str_extract_all(inf,"[+][1-9]"))))
      depinf<-inf[which(str_detect(inf,loc[1])==TRUE)]
      arrinf<-inf[which(str_detect(inf,loc[2])==TRUE)]
      DepTime<-unlist(str_extract_all(depinf,"[0-9][0-9][:][0-5][0-9]"))
      ArrTime<-unlist(str_extract_all(arrinf,"[0-9][0-9][:][0-5][0-9]"))
      date<-html_nodes(h,".is-active .book-matrix--tab-link--date")%>%html_text()
      DepDate<-as.Date(str_split_fixed(date," ",2)[,2],"%d/%m/%Y")
      ArrDate<-as.character(DepDate+day)
      DepDate<-as.character(DepDate)
      NameOfAirline<-rep("SWISS",length(DepTime))
      price<-html_nodes(h,".js-book-bundling--button")%>%html_text()
      price<-str_split_fixed(str_split_fixed(unlist(gsub("\n","",price)),"USD",2)[,2],"S",2)[,1]
      price<-gsub(",","",gsub(" ","",price))
      price[which(price=="")]<-Inf
      PriceInUsd<-vector(length = length(DepTime))
      for (i in 1:length(DepTime)) {
        PriceInUsd[i]<-min(price[i:6*i])
      }
      ScrapeTime<-format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
      Swissdata1<-data.frame(Airline=NameOfAirline,DepTime=DepTime,ArrTime=ArrTime,From=From,To=To,DepDate=DepDate,ArrDate=ArrDate,Min_Price=PriceInUsd,stringsAsFactors = FALSE)
      Swissdata<-rbind(Swissdata,Swissdata1)
      dedata<-Swissdata[which(Swissdata$From==Swissdata$From[1]),]
      redata<-Swissdata[which(Swissdata$From==Swissdata$From[nrow(Swissdata)]),]
      debest<-dedata[which(dedata$Min_Price==min(dedata$Min_Price)),]
      names(debest)[names(debest)=="Airline"]="DepartureAirline"
      rebest<-redata[which(redata$Min_Price==min(redata$Min_Price)),]
      names(rebest)[names(rebest)=="Airline"]="ReturnAirline"
      Min_price<-sum(as.numeric(debest$Min_Price),as.numeric(rebest$Min_Price))
      Swissdata<-cbind(debest[,-8],rebest[,-8])
      Swissdata<-cbind(Swissdata,Min_price)
      Swissdata<-cbind(Swissdata,ScrapeTime)
    }
  }
  return(Swissdata)
  remDr$closeall()
  remDr$close()
}




AirlinePrices<-function(d1,d2){
  Airports<-c("VTZ","GAU","DEL","GOI","AMD","SXR","BLR","COK","TRV","BOM","BBI","JAI","MAA","CJB","IXM","HYD","LKO","VNS","CCU")
  AirlineDatabase<-data.frame()
  for(i in seq(1:length(Airports))){
    r<-SwissAirlinesInput(Airports[i],d1,d2)
    if(sum(!is.na(match("no flights",r)))!=1){
      AirlineDatabase<-rbind(AirlineDatabase,r)
      Sys.sleep(2)
    }
  }
  if(file.exists("/Users/lkm/Desktop/data course/project/t1.csv")){
    loadDb<-read.csv("/Users/lkm/Desktop/data course/project/t1.csv",stringsAsFactors=F)
    loadDb<-loadDb[,-1]
    AirlineDatabase<-rbind(loadDb,AirlineDatabase)
    write.csv(AirlineDatabase,"/Users/lkm/Desktop/data course/project/t1.csv")
  }else{
    write.csv(AirlineDatabase,"/Users/lkm/Desktop/data course/project/t1.csv")
  }
  return(AirlineDatabase)
}
AirlinePrices("12/14/2016","01/13/2017")






