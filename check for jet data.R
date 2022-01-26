library(RSelenium)
library(RCurl)
library(bitops)
library(XML)
library(stringr)
library(lubridate)
library(rvest)
require(Hmisc)

JetAirwaysInput<- function(destination,departureDate,arrivalDate){
  url<-"http://www.jetairways.com/EN/IN/planyourtravel/book-online.aspx"
  remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = list(phantomjs.binary.path = "/Users/lkm/phantomjs"))
  remDr$open()
  remDr$navigate(url)
  wxbox <- remDr$findElement(using = 'id', "ctl00_MainBody_ctl00_autoOrigin_txtCountry")
  wxbox$sendKeysToElement(list("BOS"))
  Sys.sleep(5)
  wxbox<-remDr$findElements(using = 'css selector', '.autocomplete-suggestion')
  for(i in seq(1:length(wxbox))){
    for(j in seq(1:length(str_split(wxbox[[i]]$getElementText()," ")[[1]]))){
      if(str_split(wxbox[[i]]$getElementText()," ")[[1]][j]=="(BOS)")
      {
        wxbox[[i]]$clickElement()
      }
    }
  }
  
  wxbox <- remDr$findElement(using = 'id', "ctl00_MainBody_ctl00_autoDestination_txtCountry")
  wxbox$sendKeysToElement(list(destination))
  destination<-paste(c("(",destination,")"),collapse="")
  Sys.sleep(2)
  wxbox<-remDr$findElements(using = 'css selector', '.autocomplete-suggestion')
  for(i in seq(1:length(wxbox))){
    for(j in seq(1:length(str_split(wxbox[[i]]$getElementText()," ")[[1]]))){
      if(str_split(wxbox[[i]]$getElementText()," ")[[1]][j]==destination)
      {
        wxbox[[i]]$clickElement()
      }
    }
  }
  wxbox <- remDr$findElement(using = 'css selector', ".icon-calendar")
  wxbox$clickElement()
  departureDate<-as.Date(departureDate,"%m/%d/%Y")
  repeat{
    if(remDr$findElement(using = 'class', 'ui-datepicker-year')$getElementAttribute('value')==year(departureDate)){
      break
    }else{
      wxbox<-remDr$findElement(using = 'class', 'ui-datepicker-year')
      wxbox$clickElement()
      wxbox$sendKeysToElement(list(key="down_arrow"))
    }
  }
  if(remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value')<(month(departureDate)-1)){
    repeat{
      if(remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value')==(month(departureDate)-1)){
        break
      }else{
        wxbox<-remDr$findElement(using = 'class', 'ui-datepicker-month')
        wxbox$clickElement()
        wxbox$sendKeysToElement(list(key="down_arrow"))
      }
    }
  }else{
    repeat{
      if(remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value')==(month(departureDate)-1)){
        break
      }else{
        wxbox<-remDr$findElement(using = 'class', 'ui-datepicker-month')
        wxbox$clickElement()
        wxbox$sendKeysToElement(list(key="up_arrow"))
      }
    }
  }
  wxbox<-remDr$findElements(using = 'css selector', '.ui-state-default')
  dateind<-vector("character")
  k<-1
  for(i in seq(1:length(wxbox))){
    if(wxbox[[i]]$getElementText()==day(departureDate))
    {
      dateind[k]<-i
      k<-k+1
    }
  }
  if(length(dateind)>1){
    j<-as.numeric(dateind[2])
    wxbox[[j]]$clickElement()
  }else{
    j<-as.numeric(dateind[1])
    wxbox[[j]]$clickElement()
  }
  
  arrivalDate<-as.Date(arrivalDate,"%m/%d/%Y")
  wxbox<-remDr$findElement(using = 'id', 'txtEndDate')
  wxbox$clickElement()
  repeat{
    if(remDr$findElement(using = 'class', 'ui-datepicker-year')$getElementAttribute('value')==year(arrivalDate)){
      break
    }else{
      wxbox<-remDr$findElement(using = 'class', 'ui-datepicker-year')
      wxbox$clickElement()
      wxbox$sendKeysToElement(list(key="down_arrow"))
    }
  }
  
  if(remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value')<(month(arrivalDate)-1)){
    repeat{
      if(remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value')==(month(arrivalDate)-1)){
        break
      }else{
        wxbox<-remDr$findElement(using = 'class', 'ui-datepicker-month')
        wxbox$clickElement()
        wxbox$sendKeysToElement(list(key="down_arrow"))
      }
    }
  }else{
    repeat{
      if(remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value')==(month(arrivalDate)-1)){
        break
      }else{
        wxbox<-remDr$findElement(using = 'class', 'ui-datepicker-month')
        wxbox$clickElement()
        wxbox$sendKeysToElement(list(key="up_arrow"))
      }
    }
  }
  wxbox<-remDr$findElements(using = 'css selector', '.ui-state-default')
  dateind<-vector("character")
  k<-1
  for(i in seq(1:length(wxbox))){
    if(wxbox[[i]]$getElementText()==day(arrivalDate))
    {
      dateind[k]<-i
      k<-k+1
    }
  }
  if(length(dateind)>1){
    j<-as.numeric(dateind[2])
    wxbox[[j]]$clickElement()
  }else{
    j<-as.numeric(dateind[1])
    wxbox[[j]]$clickElement()
  }
  wxbox <- remDr$findElement(using = 'id', "MainBody_ctl00_btnBookOnline")
  wxbox$clickElement()
  Sys.sleep(60)
  remDr$screenshot(display = TRUE)
  t<- read_html(remDr$getPageSource()[[1]])
  time<-html_nodes(t,".journey__time")%>%html_text()
  if(length(time)==0){
    jet<-"no flights"
  }else{
    loc<-html_nodes(t,"abbr")%>%html_text()
    date<-as.Date(str_split_fixed(html_nodes(t,".travel-info--date")%>%html_text(),",",2)[,2],"%d %b %Y")
    price<-gsub("\n*|\\t*|\\s*","",html_nodes(t,".items")%>%html_text())
    price<-as.numeric(gsub("[^.0123456789]","",price))
    #a<-which(str_detect(price,"USD")==TRUE)
    #price[a]<-gsub(",","",price[a])
    #price[a]<-gsub(" ","",price[a])
    #price[a]<-str_split_fixed(price[a],"USD",2)[,2]
    #price[a]<-as.numeric(gsub(" ","",as.character(price[a])))
    pricegroup<-array("0",c(4,1,length(price)/4))
    k<-1
    for(i in seq(1,length(price),4)){
      pricegroup[,,k]<-as.matrix(price[i:(i+3)])
      k<-k+1
    }
    pricemin<-vector("numeric")
    k<-1
    for(i in seq(1,length(price)/4,2)){
      pricemin[k]<-min(as.numeric(pricegroup[,,i]),na.rm=T)
      k<-k+1
    }
    
    data<-cbind(time,as.character(date))
    data<-cbind(data,loc)
    place<-unique(str_extract(html_nodes(t,".airport")%>%html_text(),"[A-Z][A-Z][A-Z]"))
    newdata<-data[which(data[,3]==place[1]|data[,3]==place[2]),]
    departure<-newdata[seq(1,nrow(newdata),by=2),]
    arrival<-newdata[seq(2,nrow(newdata),by=2),]
    jet<-data.frame(departure,arrival)
    ScrapeTime<-format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
    jet<-cbind(jet,pricemin,stringsAsFactors = FALSE)
    name<-rep("Jet Airways",nrow(jet))
    jet<-cbind(name,jet)
    colnames(jet)<-c("NameOfAirline","DepTime","DepDate","From","ArrTime","ArrDate","To","PriceInUsd")
    jet<-data.frame(Airline=jet[,1],DepTime=jet[,2],ArrTime=jet[,5],From=jet[,4],To=jet[,7],DepDate=jet[,3],ArrDate=jet[,6],Min_Price=jet[,8],stringsAsFactors = FALSE)
    jet$DepDate<-as.character(jet$DepDate)
    jet$ArrDate<-as.character(jet$ArrDate)
    dedata<-jet[which(jet$From==jet$From[1]),]
    redata<-jet[which(jet$From==jet$From[nrow(jet)]),]
    debest<-dedata[which(dedata$Min_Price==min(dedata$Min_Price)),]
    names(debest)[names(debest)=="Airline"]="DepartureAirline"
    rebest<-redata[which(redata$Min_Price==min(redata$Min_Price)),]
    names(rebest)[names(rebest)=="Airline"]="ReturnAirline"
    Min_price<-sum(as.numeric(debest$Min_Price),as.numeric(rebest$Min_Price))
    jet<-cbind(debest[,-8],rebest[,-8])
    jet<-cbind(jet,Min_price)
    jet<-cbind(jet,ScrapeTime)
    }
  return(jet)
  remDr$closeall()
  remDr$close()
}



AirlinePrices<-function(d1,d2){
  Airports<-c("IXZ","VTZ","GAU","DEL","GOI","AMD"
              # "SXR","BLR","IXE","COK","CCJ","TRV"
              # "BHO","BOM","NAG","IMF","ATQ","JAI"
              # "MAA","CJB","IXM","TRZ","HYD","LKO","VNS","CCU"
              )
  AirlineDatabase<-data.frame()
  for(i in seq(1:length(Airports))){
    r<-JetAirwaysInput(Airports[i],d1,d2)
    if(sum(!is.na(match("no flights",r)))!=1){
      AirlineDatabase<-rbind(AirlineDatabase,r)
      Sys.sleep(2)
      return(AirlineDatabase)
      
    }
  }
  if(file.exists("/Users/lkm/Desktop/data course/project/t1.csv")){
    loadDb<-read.csv("/Users/lkm/Desktop/data course/project/t1.csv",header=TRUE,stringsAsFactors=F)
    loadDb<-loadDb[,-1]
    AirlineDatabase<-rbind(loadDb,AirlineDatabase)
    write.csv(AirlineDatabase,"/Users/lkm/Desktop/data course/project/t1.csv")
  }else{
    write.csv(AirlineDatabase,"/Users/lkm/Desktop/data course/project/t1.csv")
  }
  return(AirlineDatabase)
}

AirlinePrices("12/17/2016","01/13/2017")
