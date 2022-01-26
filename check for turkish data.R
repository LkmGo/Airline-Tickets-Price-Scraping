library(RCurl)
library(bitops)
library(stringr)
library(lubridate)
library(XML)
library(rvest)
library(base)
library(RSelenium)
checkForServer()
startServer()
TurkishAirlinesInput<- function(destination,departureDate,arrivalDate){
  d1<-paste(c("(",destination,")",","),collapse="")
  departureDate<-as.Date(departureDate,"%m/%d/%Y")
  arrivalDate<-as.Date(arrivalDate,"%m/%d/%Y")
  url<-"https://p.turkishairlines.com/"
  remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = list(phantomjs.binary.path = "/Users/lkm/phantomjs"))
  remDr$open()
  remDr$navigate(url)
  Sys.sleep(10)
  wxbox<-remDr$findElements(using = 'css selector', '.pull-left')
  for(i in seq(1:length(wxbox))){
    if(length(wxbox[[i]]$findChildElements(using = 'css selector', '.icon-plane'))==1){
      wxbox[[i]]$clickElement()
      tryCatch({wxbox[[i]]$sendKeysToElement(list("BOS"))
        print(1)
      },error=function(e){
        print("ignore this error")
      }
      )
    }
  }
  wxbox<-remDr$findElements(using = 'class', 'label-full')
  for(i in seq(1:length(wxbox))){
    for(j in seq(1:length(str_split(wxbox[[i]]$getElementText()," ")[[1]]))){
      if(str_split(wxbox[[i]]$getElementText()," ")[[1]][j]=="(BOS),")
      {
        wxbox[[i]]$clickElement()
        break
      }
    }
  }
  wxbox<-remDr$findElements(using = 'css selector', '.pull-left')
  for(i in seq(1:length(wxbox))){
    if(length(wxbox[[i]]$findChildElements(using = 'css selector', '.form-control'))==1){
      wxbox[[i]]$clickElement()
      tryCatch(
        {
          wxbox[[i]]$sendKeysToElement(list(destination))
        },error=function(e){
          print("ignore this error")
        }
      )
    }
  }
  wxbox<-remDr$findElements(using = 'class', 'label-full')
  for(i in seq(1:length(wxbox))){
    for(j in seq(1:length(str_split(wxbox[[i]]$getElementText()," ")[[1]]))){
      if(str_split(wxbox[[i]]$getElementText()," ")[[1]][j]==d1){
        wxbox[[i]]$clickElement()
        break
      }
    }
  }
  remDr$findElement(using = 'id', 'calendarPlaceholder')$clickElement()
  repeat{
    if(remDr$findElement(using = 'class', 'ui-datepicker-year')$getElementText()[[1]]==year(departureDate) & remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementText()[[1]] ==toupper(format(departureDate,"%B"))){
      break
    }else{
      remDr$findElement(using = 'css selector', '.ui-datepicker-next')$clickElement()
    }
  }
  wxbox<-remDr$findElements(using = 'css selector', '.ui-state-default')
  wxbox[[as.numeric(day(departureDate))]]$clickElement()
  repeat{
    if(remDr$findElement(using = 'class', 'ui-datepicker-year')$getElementText()[[1]]==year(arrivalDate) & remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementText()[[1]] ==  toupper(format(arrivalDate,"%B"))){
      break
    }else{
      remDr$findElement(using = 'css selector', '.ui-datepicker-next')$clickElement()
    }
  }
  wxbox<-remDr$findElements(using = 'css selector', '.ui-state-default')
  wxbox[[as.numeric(day(arrivalDate))]]$clickElement()
  remDr$maxWindowSize()
  Sys.sleep(3)
  remDr$findElement(using = 'id', 'executeSingleCitySubmit')$clickElement()
  Sys.sleep(20)
  remDr$screenshot(display = TRUE)
  t<- read_html(remDr$getPageSource()[[1]])
  ##Turkish Airlines
  #get the price
  price<-html_nodes(t,".fs-20 span")%>%html_text()
  if(length(price)==0){
    Turkishdata<-"no flights"
  }else{
    price<-price[seq(1,length(price),by=2)]
    depinf<-html_nodes(t,".showcanvasoverlay:nth-child(1) td:nth-child(2) .mb-5")%>%html_text()
    deploc<-html_nodes(t,".showcanvasoverlay:nth-child(1) td:nth-child(2) div:nth-child(1)")%>%html_text()
    arr<-html_nodes(t,".mt-5 div:nth-child(1) , .mb-5")%>%html_text()
    arr<-arr[-seq(1,length(arr),by=7)]
    arr<-matrix(arr,ncol=3,byrow = TRUE)
    arr<-arr[which(arr[,3]%in%deploc==TRUE),]
    arr<-arr[-seq(1,length(arr),by=2),]
    deploc<-str_extract(deploc,"[A-Z][A-Z][A-Z]")
    depdate<-as.character(as.Date(str_split_fixed(depinf[seq(2,length(depinf),2)],",",2)[,2],format = " %B %d"))
    arrdate<-as.character(as.Date(str_split_fixed(arr[,2],",",2)[,2],format = " %B %d"))
    ScrapeTime<-format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
    Turkishdata<-data.frame(Airline=rep("Turkish",length(price)),DepTime=depinf[seq(1,length(depinf),2)],ArrTime=arr[,1],From=deploc,To=str_extract(arr[,3],"[A-Z][A-Z][A-Z]"),DepDate=depdate,ArrDate=arrdate,Min_Price=price,stringsAsFactors = FALSE)
    dedata<-Turkishdata[which(Turkishdata$From==Turkishdata$From[1]),]
    redata<-Turkishdata[which(Turkishdata$From==Turkishdata$From[nrow(Turkishdata)]),]
    debest<-dedata[which(dedata$Min_Price==min(dedata$Min_Price)),]
    names(debest)[names(debest)=="Airline"]="DepartureAirline"
    rebest<-redata[which(redata$Min_Price==min(redata$Min_Price)),]
    names(rebest)[names(rebest)=="Airline"]="ReturnAirline"
    Min_price<-sum(as.numeric(debest$Min_Price),as.numeric(rebest$Min_Price))
    Turkishdata<-cbind(debest[,-8],rebest[,-8])
    Turkishdata<-cbind(Turkishdata,Min_price)
    Turkishdata<-cbind(Turkishdata,ScrapeTime)
  }
  return(Turkishdata)
  remDr$closeall()
  remDr$close()
}

AirlinePrices<-function(d1,d2){
  # Airports<-c("DEL","AMD","BLR","BOM","ATQ","MAA","HYD","CCU")
  AirlineDatabase<-data.frame()
  for(i in seq(1:length(Airports))){
    r<-TurkishAirlinesInput(Airports[i],d1,d2)
    if(sum(!is.na(match("no flights",r)))!=1){
      AirlineDatabase<-rbind(AirlineDatabase,r)
      Sys.sleep(2)
      
    }
  }
  if(file.exists("/Users/lkm/Desktop/data course/project/t2.csv")){
    loadDb<-read.csv("/Users/lkm/Desktop/data course/project/t2.csv",header=TRUE,stringsAsFactors=F)
    loadDb<-loadDb[,-1]
    AirlineDatabase<-rbind(loadDb,AirlineDatabase)
    write.csv(AirlineDatabase,"/Users/lkm/Desktop/data course/project/t2.csv")
  }else{
    write.csv(AirlineDatabase,"/Users/lkm/Desktop/data course/project/t2.csv")
  }
  return(AirlineDatabase)
}

AirlinePrices("12/17/2016","01/13/2017")



