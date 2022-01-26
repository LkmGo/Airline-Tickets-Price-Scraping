library(RSelenium)
library(RCurl)
library(bitops)
library(XML)
library(stringr)
library(lubridate)
library(rvest)
require(Hmisc)


QatarInput<- function(destination,departureDate,arrivalDate){
  url<-"http://www.qatarairways.com/us/en/homepage.page"
  remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = list(phantomjs.binary.path = "/Users/lkm/phantomjs"))
  remDr$open()
  remDr$navigate(url)
  wxbox <- remDr$findElement(using = 'id', "FromTemp")
  wxbox$sendKeysToElement(list("BOS"))
  Sys.sleep(2)
  wxbox$sendKeysToElement(list(key="enter"))
  wxbox <- remDr$findElement(using = 'id', "ToTemp")
  Sys.sleep(2)
  wxbox$sendKeysToElement(list(destination))
  Sys.sleep(2)
  wxbox$sendKeysToElement(list(key="enter"))
  wxbox <- remDr$findElement(using = 'id', "departing")
  wxbox$clearElement()
  departureDate<-as.Date(departureDate,"%m/%d/%Y")
  departureDate<-format(departureDate,"%d-%b-%Y")
  wxbox$sendKeysToElement(list(departureDate))
  wxbox$sendKeysToElement(list(key="enter"))
  wxbox <- remDr$findElement(using = 'id', "returning")
  wxbox$clearElement()
  arrivalDate<-as.Date(arrivalDate,"%m/%d/%Y")
  arrivalDate<-format(arrivalDate,"%d-%b-%Y")
  wxbox$sendKeysToElement(list(arrivalDate))
  wxbox$sendKeysToElement(list(key="enter"))
  wxbox <- remDr$findElement(using = 'id', "bookFlight")
  wxbox$clickElement()
  #wxbox$sendKeysToElement(list(key="enter"))
  Sys.sleep(10)
  #remDr$screenshot(display = TRUE)
  h<- read_html(remDr$getPageSource()[[1]])
  #get the time and location
  # test1<-html_nodes(h,".orange-box")%>%html_text()
  # test2<-html_nodes(h,".error-box")%>%html_text()
  # if(nchar(test1)>300||nchar(test2)!=128){
  #   Qatardata<-"no flights"
  # }else{
    place<-html_nodes(h,"span.time-head")%>%html_text()
    place<-place[which(nchar(place)>50)]
    place<-trimws(place)
    place<-place[which(place!="")]
    information<-str_split_fixed(place,"\n",2)
    odd<-seq(1,nrow(information),by=2)
    depinf<-information[odd,]
    arrinf<-information[-odd,]
    for (i in 1:nrow(arrinf)) {
      if(arrinf[i]==arrinf[i+1]){depnum<-i+1}
      else{break}
    }
    data<-cbind(depinf,arrinf)
    for (i in 1:ncol(data)) {
      data[,i]<-gsub("\t","",data[,i])
      data[,i]<-gsub("\n","",data[,i])
    }
    price<-html_nodes(h,"span.price")%>%html_text()
    k<-length(html_nodes(h,".f1top")%>%html_text())
    m<-length(which(data[,2]==data[1,2]))
    n<-nrow(data)-m
    lf<-matrix(c(1,1,m,n),nrow = 2,byrow = TRUE)
    rf<-matrix(c(k,length(price)),nrow = 2)
    if(lf[1,1]==lf[1,2]&lf[2,1]==lf[2,2]){
      result<-c(k/2,k/2)
    }else{
      result<-solve(lf,rf)
    }
    price<-gsub(",","",price)
    price[which(nchar(price)<15)]<-"10000000"
    price<-as.numeric(price)
    priinf<-vector(length = nrow(arrinf))
    for (i in 1:length(priinf)) {
      if(i<=m){priinf[i]<-min(price[(result[1]*i-result[1]+1):(result[1]*i)])
      }else{
        priinf[i]<-min(price[(result[2]*i-m*result[1]-result[2]+1):(result[2]*i-m*result[1])])
      }
      date<-html_nodes(h,"span.date")%>%html_text()
      date<-str_split_fixed(date,",",2)
      d<-data.frame(date[c(1,2),2])
      d[,1]<-gsub("\t","",d[,1])
      d[,1]<-gsub("\n"," ",d[,1])
      a<-data[,2]==unique(data[,2])[1]
      newd1<-rep(d[1,1],nrow(data[which(a==TRUE),]))
      newd2<-rep(d[2,1],nrow(data[which(a==FALSE),]))
      newd<-c(newd1,newd2)
      
      #reformat the data
      a<-str_extract(data[,4],"[0-9]")
      a[which(is.na(a)==TRUE)]<-0
      b<-str_extract(data[,4],"[A-Z][A-Z][A-Z]")
      newd<-as.Date(newd,format = "%d %b %Y")
      e<-newd+as.numeric(a)
      Qatardata<-data.frame(Airline=rep("Qatar",nrow(data)),DepTime=data[,1],ArrTime=data[,3],From=data[,2],To=b,DepDate=newd,ArrDate=e,Min_Price=priinf,stringsAsFactors = FALSE)
      dedata<-Qatardata[which(Qatardata$From==Qatardata$From[1]),]
      redata<-Qatardata[which(Qatardata$From==Qatardata$From[nrow(Qatardata)]),]
      debest<-dedata[which(dedata$Min_Price==min(dedata$Min_Price,na.rm = TRUE)),]
      names(debest)[names(debest)=="Airline"]="DepartureAirline"
      rebest<-redata[which(redata$Min_Price==min(redata$Min_Price,na.rm = TRUE)),]
      names(rebest)[names(rebest)=="Airline"]="ReturnAirline"
      Min_price<-sum(debest$Min_Price,rebest$Min_Price)
      Qatardata<-cbind(debest[1,-8],rebest[1,-8])
      Qatardata<-cbind(Qatardata,Min_price)
    #}
  }
  return(Qatardata)
  remDr$closeall()
  remDr$close()
}




AirlinePrices<-function(d1,d2){
  Airports<-c(
               # "DEL",
               # "GOI",
                "AMD",
               "BLR"
              # "COK",
              # "CCJ",
              # "TRV",
              # "BOM",
              # "NAG",
              # "ATQ",
              # "MAA",
              # "HYD",
              # "CCU"
              )
  AirlineDatabase<-data.frame()
  for(i in seq(1:length(Airports))){
    r<-QatarInput(Airports[i],d1,d2)
    if(sum(!is.na(match("no flights",r)))!=1){
      AirlineDatabase<-rbind(AirlineDatabase,r)
      Sys.sleep(2)
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






