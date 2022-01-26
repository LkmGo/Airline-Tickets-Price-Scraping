# Loads required packages.
library(RSelenium, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(RCurl, quietly = TRUE)
library(bitops, quietly = TRUE)
library(XML, quietly = TRUE)D
library(stringr, quietly = TRUE)
library(rvest, quietly = TRUE)
library(Hmisc, quietly = TRUE)
library(dplyr, quietly = TRUE)

################################################################
## FUNCTIONS
################################################################

#A Function, when called, returns a table with minimum price for flight operated by Etihad Airways for given destination and dates from Boston.

# EtihadInput <-
#   function(remDr,
#            i,
#            Airports,
#            destination,
#            departureDate,
#            arrivalDate) {
#     print(paste(
#       c(
#         "Scraping",
#         " Etihad Airways for ",
#         destination,
#         "; ",
#         length(Airports) - i,
#         " remaining"
#       ),
#       collapse = ""
#     ))
#     if (destination %in% c(
#       "JAI",
#       "IXZ",
#       "VTZ",
#       "GAU",
#       "GAY",
#       "GOI",
#       "SXR",
#       "IXE",
#       "NAG",
#       "BHO",
#       "IMF",
#       "BBI",
#       "CJB",
#       "IXM",
#       "ATQ",
#       "TRZ",
#       "LKO",
#       "VNS"
#     )) {
#       print(paste(c(
#         destination, " not serviced by Etihad Airways"
#       ), collapse = ""))  #Checks for aiports not serviced ny airline
#       return("no flights")
#     } else{
#       url <- "http://www.etihad.com/en-us/"
#       remDr$navigate(url) #Navigates the phantomjs browser to above mentioned url
#       departureDate <- as.Date(departureDate, "%m/%d/%Y")
#       departureDate <- format(departureDate, "%d/%m/%Y")
#       arrivalDate <- as.Date(arrivalDate, "%m/%d/%Y")
#       arrivalDate <- format(arrivalDate, "%d/%m/%Y")
#       wxbox <-
#         remDr$findElement(using = 'id', "frm_2012158061206151234") #finds departure airport box
#       wxbox$sendKeysToElement(list(key = "down_arrow"))
#       Sys.sleep(6)
#       wxbox$sendKeysToElement(list("BOS")) #Inserts Boston as departure airport
#       Sys.sleep(6)
#       wxbox$sendKeysToElement(list(key = "enter"))
#       Sys.sleep(6)
#       wxbox <-
#         remDr$findElement(using = 'id', "frm_20121580612061235") #finds arrival airport box
#       wxbox$sendKeysToElement(list(key = "down_arrow"))
#       Sys.sleep(6)
#       wxbox$sendKeysToElement(list(destination)) #Inserts destination argument as arrival airport
#       Sys.sleep(6)
#       wxbox$sendKeysToElement(list(key = "enter"))
#       Sys.sleep(6)
#       wxbox <-
#         remDr$findElement(using = 'id', "frm_2012158061206151238")
#       wxbox$clearElement()
#       Sys.sleep(6)
#       wxbox$sendKeysToElement(list(departureDate, "\uE007")) #Inserts departure date
#       wxbox <-
#         remDr$findElement(using = 'id', "frm_2012158061206151239")
#       wxbox$clearElement()
#       Sys.sleep(6)
#       wxbox$sendKeysToElement(list(arrivalDate, "\uE007"))
#       Sys.sleep(6) #Inserts arrival date
#       webElem <-
#         remDr$findElement(using = 'css selector', ".btnSearchFlight")
#       webElem$clickElement()
#       Sys.sleep(10)
#       Etihad <-
#         read_html(remDr$getPageSource()[[1]]) #Extracts current page's HTML code
#       ################################################################
# 
#       Etihad <- read_html("Etihad10.html")
#       Etihad <- read_html("Etihad11.html")
# 
#       #Checking whether flights are available.If not, then break the loop.
#       errorMsg <- html_nodes(Etihad, ".priorH , h3") %>% html_text()
#       if (length(errorMsg) <= 3)
#       {
#         EtihadData <- "No flights."
#         return(EtihadData)
#       } else{
#         #Extracting departure and arrival locations.
#         #"departure_code" includes codes of outbound and inbound trips, both.
#         #And same for "return_code" as well.
#         #Outbound  trip is a trip from Boston  to  Destination.
#         #Inbound trip is a trip from Destination to Boston.
#         departure_code <-
#           html_nodes(Etihad, ".airportCode") %>% html_text()
#         return_code <-
#           html_nodes(Etihad, ".ItinHeaderTo div:nth-child(1)") %>% html_text()
# 
#         #Extracting departure time.
#         departure_time <-
#           html_nodes(Etihad, ".ItinHeaderFrom") %>% html_text()
#         dep_time <-
#           grep("[0-9]+[:]+[0-9]+", x = departure_time, value = T)
#         dep_time <- gsub("[A-Z]+", replacement = "", x = dep_time)
# 
#         #Extracting arrival time.
#         arrival_time <-
#           html_nodes(Etihad, ".ItinHeaderTo div") %>% html_text()
#         arr_time <-
#           grep("[0-9]+[:]+[0-9]+", x = arrival_time, value = T)
# 
#         #Extracting outbound trips' prices.
#         outboundPrices <-
#           html_nodes(Etihad, "#dtcontainer-outbounds .price-cell") %>% html_text()
#         outboundPrices <- trimws(gsub("\n", "", outboundPrices))
#         outboundPrices[outboundPrices == "Sold out"] <- NA
#         outboundPrices <-
#           as.numeric(str_extract(outboundPrices, "[0-9]{3,4}+[.]+[0-9]{2}"))
#         lp_ob <- length(outboundPrices)
# 
#         #Sometimes both, outbound and inbound trips give either 3 or 4 columns for prices.
#         #So following code checks how many columns are there and calculates min prices
#         #accordingly.
#         outboundCodes <-
#           html_nodes(Etihad,
#                      "#dtcontainer-outbounds .alignAfterMoreThanOneDayIndicator") %>%
#           html_text() %>%
#           length()
# 
#         if (lp_ob / outboundCodes == 3) {
#           min_price_ob <-
#             unname(tapply(
#               X = outboundPrices,
#               cut(1:lp_ob, lp_ob / 3),
#               FUN = min,
#               na.rm = T
#             ))
#         } else{
#           min_price_ob <-
#             unname(tapply(
#               X = outboundPrices,
#               cut(1:lp_ob, lp_ob / 4),
#               FUN = min,
#               na.rm = T
#             ))
#         }
#         min_price_ob[min_price_ob == "Inf"] <- NA
# 
#         inboundPrices <-
#           html_nodes(Etihad, "#dtcontainer-inbounds .price-cell") %>% html_text()
#         inboundPrices <- trimws(gsub("\n", "", inboundPrices))
#         inboundPrices[inboundPrices == "Sold out"] <- NA
#         inboundPrices <-
#           as.numeric(str_extract(inboundPrices, "[0-9]{3,4}+[.]+[0-9]{2}"))
#         lp_ib <- length(inboundPrices)
# 
#         inboundCodes <-
#           html_nodes(Etihad, "#dtcontainer-inbounds td.ItinHeaderFrom") %>%
#           html_text() %>%
#           length()
# 
#         if (lp_ib / inboundCodes == 4) {
#           min_price_ib <-
#             unname(tapply(
#               X = inboundPrices,
#               cut(1:lp_ib, lp_ib / 4),
#               FUN = min,
#               na.rm = T
#             ))
#         } else{
#           min_price_ib <-
#             unname(tapply(
#               X = inboundPrices,
#               cut(1:lp_ib, lp_ib / 3),
#               FUN = min,
#               na.rm = T
#             ))
#         }
# 
#         #Sometimes Etihad Airways also show flights even if all the tickets are sold.
#         #In this case, all of the values on the web site will be "Sold out" or "Not available".
#         #The above loop converts those values to "Inf".
#         #So we are forcefully imputing NAs in such scenario for those "Inf"s.
#         #It shows some warnings but the code works that way.
# 
#         min_price_ib[min_price_ib == "Inf"] <- NA
#         Min_prices <- c(min_price_ob, min_price_ib)
# 
#         #Extracting dates.
#         #Following code does not show blanks for same day arrival. So returns less elements than actual.
#         dates <- html_nodes(Etihad, ".ItinHeaderTo") %>% html_text()
#         dates <- grep("[0-9]+[:]+[0-9]+", x = dates, value = T)
# 
#         #Etihad website does not show departure dates and arrival dates. So to make a final data frame we take
#         #dates from the departureDate parameter of the function itself.
#         #As it does not show the dates, from the website itself we cannot say that on which date the flight reaches
#         #the destination. However, the website shows that after how many days, it'll reach. So we add that numeric
#         #value of days, called "lateness", to departure date to find the arrival date of the outbound flight to its destination.
#         lateness <- str_extract(dates, "[Arrival,]{8}+|[Arrival]{7}")
#         lateness[lateness == "Arrival"] <- 1
#         lateness[lateness == "Arrival,"] <- 2
#         lateness[is.na(lateness)] <- 0
#         lateness <-  as.numeric(lateness)
#         lateness_ob <- lateness[1:outboundCodes]
#         lateness_ib <- lateness[1:inboundCodes]
# 
#         #Taking departureDate from function parameter and converting it into appropriate format.
#         #To find arrival dates, we add lateness to departure date here.
#         DepDate_fB <- rep("departureDate", outboundCodes)
#         ArrDate_tD <- as.Date(DepDate_fB, "%m/%d/%Y") + lateness_ob
#         ArrDate_tD <- format(ArrDate_tD, "%m/%d/%Y")
# 
#         DepDate_fD <- rep("arrivalDate", inboundCodes)
#         ArrDate_tB <- as.Date(DepDate_fD, "%m/%d/%Y") + lateness_ib
#         ArrDate_tB <- format(ArrDate_tB, "%m/%d/%Y")
# 
#         #Following are the Departure and Arrival dates for all the flights.
#         #That means "DepDate = Dates from BOSTON to Destination + from Destination to BOSTON".
#         #That means "ArrDate = Dates from BOSTON to Destination + from Destination to BOSTON".
#         DepDate <- as.data.frame(c(DepDate_fB, DepDate_fD))
#         ArrDate <- as.data.frame(c(ArrDate_tD, ArrDate_tB))
#         AllDates <-
#           cbind(as.data.frame(DepDate), as.data.frame(ArrDate))
# 
#         #Combining all the details in a data frame.
#         EtihadData <-
#           as.data.frame(
#             cbind(
#               Airline = rep("Etihad", length(departure_code)),
#               dep_time,
#               arr_time,
#               departure_code,
#               return_code,
#               DepDate,
#               ArrDate,
#               Min_prices
#             )
#           )
# 
#         colnames(EtihadData) <-
#           c(
#             "Airline",
#             "DepTime",
#             "ArrTime",
#             "From",
#             "To",
#             "DepDate",
#             "ArrDate",
#             "Min_Price"
#           )
# 
# 
#         #Extracting minimum prices for outbound and inbound flights. Addition of minimum values
#         #of each will give total minimum price for round trip. Followin code also creates a final
#         #data frame with minimum price of round trip as well.
#         f <- EtihadData[which(EtihadData$From == "BOS"), ]
#         t <- EtihadData[which(EtihadData$From != "BOS"), ]
#         colnames(f)[8] <- "Min_Price_1"
#         colnames(t)[8] <- "Min_Price_2"
#         f <- arrange(f, Min_Price_1)
#         t <- arrange(t, Min_Price_2)
#         EtihadData <- cbind(f[1, ], t[1, ])
#         Min_Price <- EtihadData$Min_Price_1 + EtihadData$Min_Price_2
#         EtihadData <- cbind(EtihadData, Min_Price)
#         EtihadData <- EtihadData[, -c(8, 16)]
#         ScrapeTime <- format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
#         EtihadData <- cbind(EtihadData, ScrapeTime)
# 
#         colnames(EtihadData) <-
#           c(
#             "Airline",
#             "DepTime",
#             "ArrTime",
#             "From",
#             "To",
#             "DepDate",
#             "ArrDate",
#             "ReturnAirline",
#             "DepTime",
#             "ArrTime",
#             "From",
#             "To",
#             "DepDate",
#             "ArrDate",
#             "Min_Price",
#             "ScrapeTime"
#           )
#       }
#       return(EtihadData)
#     }
#   }

# ################################################################
# ################################################################
# 
# # A Function, when called, returns a table with minimum price for flight operated by British Airways for given destination and dates from Boston.
# 
# BritishAirwaysInput <-
#   function(remDr,
#            i,
#            Airports,
#            destination,
#            departureDate,
#            arrivalDate) {
#     print(paste(
#       c(
#         "Scraping",
#         " British Airways for ",
#         destination,
#         "; ",
#         length(Airports) - i,
#         " remaining"
#       ),
#       collapse = ""
#     ))
#     if (destination %in% c("GAY", "BHO", "IMF", "BBI", "CJB", "TRZ")) {
#       print(paste(c(
#         destination, " not serviced by British Airways"
#       ), collapse = "")) #Checks for aiports not serviced by airline
#       return("no flights")
#     }
#     else{
#       departureDate <- as.Date(departureDate, "%m/%d/%Y")
#       departureDate <- format(departureDate, "%m-%d-%y")
#       arrivalDate <- as.Date(arrivalDate, "%m/%d/%Y")
#       arrivalDate <- format(arrivalDate, "%m-%d-%y")
#       url <- "http://www.britishairways.com/travel/fx/public/en_us"
#       remDr$navigate(url)
#       Sys.sleep(10)
#       #The following code searches for US as departure airport's country
#       if (remDr$findElement(using = 'id', 'depCountry')$getElementAttribute('value')[[1]] !=
#           "US") {
#         wxbox <- remDr$findElement(using = 'id', "depCountry")
#         wxbox$sendKeysToElement(list("U"))
#         Sys.sleep(3)
#         repeat {
#           if (remDr$findElement(using = 'id', 'depCountry')$getElementAttribute('value')[[1]] ==
#               "US") {
#             break
#           } else{
#             wxbox <- remDr$findElement(using = 'id', "depCountry")
#             wxbox$sendKeysToElement(list(key = "down_arrow"))
#             Sys.sleep(2)
#           }
#         }
#       }
#       #The following code searches for Boston as departure airport
#       if (remDr$findElement(using = 'id', 'from')$getElementAttribute('value')[[1]] !=
#           "BOS") {
#         wxbox <- remDr$findElement(using = 'id', "from")
#         wxbox$sendKeysToElement(list("B"))
#         repeat {
#           if (remDr$findElement(using = 'id', 'from')$getElementAttribute('value')[[1]] ==
#               "BOS") {
#             break
#           } else{
#             wxbox <- remDr$findElement(using = 'id', "from")
#             wxbox$sendKeysToElement(list(key = "down_arrow"))
#           }
#         }
#       }
#       wxbox <- remDr$findElement(using = 'id', "to")
#       wxbox$clearElement()
#       wxbox$sendKeysToElement(list(destination)) #Inserts destination argument as arrival airport
#       Sys.sleep(2)
#       wxbox$sendKeysToElement(list(key = "enter"))
#       wxbox <- remDr$findElement(using = 'id', "depDate")
#       wxbox$clearElement()
#       wxbox$sendKeysToElement(list(departureDate)) #Inserts departure date
#       wxbox <- remDr$findElement(using = 'id', "retDate")
#       wxbox$clearElement()
#       wxbox$sendKeysToElement(list(arrivalDate)) #Inserts arrival date
#       wxbox <- remDr$findElement(using = 'id', "theButton")
#       wxbox$clickElement()
#       Sys.sleep(20)
#       British <-
#         read_html(remDr$getPageSource()[[1]]) #Extracts current page's HTML code
#       
#       #Checking whether flights are available. If not, then code returns "No flights' and
#       #breaks out of the loop.
#       errorMsg1 <- html_nodes(British, "h3") %>% html_text()
#       errorMsg2 <- html_nodes(British, ".errorTitle") %>% html_text()
#       if (length(errorMsg1) == 3)
#       {
#         BritishData <- "No flights."
#         return(BritishData)
#       } else if (length(errorMsg2) == 3) {
#         BritishData <- "No flights."
#         return(BritishData)
#       } else{
#         #Extracting all the relevant details of departure, i.e. dates, time, locations, and prices.
#         #And  also reshaping the data in an appropriate format.
#         TimeDatePlacePrice_Dep <-
#           html_nodes(
#             British,
#             "#outboundDates .priceSelectorConnect , #outboundDates .arrivaltrue , #outboundDates .departuretrue"
#           ) %>%
#           html_text()
#         
#         rem1 <- grep("[()]", TimeDatePlacePrice_Dep)
#         TimeDatePlacePrice_Dep <- TimeDatePlacePrice_Dep[-rem1]
#         l_TDPP_Dep <- length(TimeDatePlacePrice_Dep)
#         s <-
#           sort(c(
#             seq(1, l_TDPP_Dep, 10),
#             seq(1, l_TDPP_Dep , 10) + 1,
#             seq(1, l_TDPP_Dep, 10) + 2,
#             seq(1, l_TDPP_Dep, 10) + 3,
#             seq(1, l_TDPP_Dep, 10) + 4
#           ))
#         TimeDatePlacePrice_Dep <- TimeDatePlacePrice_Dep[s]
#         
#         #Extracting departure and arrival times for outbound trips only.
#         dep_time_ob <-
#           grep(
#             "[Boston]+$",
#             TimeDatePlacePrice_Dep,
#             ignore.case = F,
#             value = T
#           )
#         dep_time_ob <- str_extract(dep_time_ob, "[0-9]+[:]+[0-9]+")
#         
#         arr_time_ob <-
#           grep(
#             "[Boston]+$",
#             TimeDatePlacePrice_Dep,
#             ignore.case = F,
#             value = T,
#             invert = T
#           )
#         arr_time_ob <- str_extract(arr_time_ob, "[0-9]+[:]+[0-9]+")
#         arr_time_ob <- arr_time_ob[!is.na(arr_time_ob)]
#         
#         #Departure and arrival locations for outbound trips only.
#         dep_loc_ob <- rep("BOS", length(dep_time_ob))
#         #arr_loc_ob <- rep(destination,length(dep_time_ob))
#         arr_loc_ob <- rep("destination", length(dep_time_ob))
#         
#         #Extracting departure dates.
#         dep_date_ob <-
#           grep("[Boston]+$", TimeDatePlacePrice_Dep, value = T)
#         dep_date_ob <-
#           str_extract(dep_date_ob, "[ ]+[0-9]{2}+[ ]+[A-Za-z]{3}") %>% trimws()
#         
#         arr_date_ob <-
#           grep(
#             "[Boston]+$|[0-9]$",
#             TimeDatePlacePrice_Dep,
#             invert = T,
#             value = T
#           )
#         arr_date_ob <-
#           str_extract(arr_date_ob, "[ ]+[0-9]{2}+[ ]+[A-Za-z]{3}") %>% trimws()
#         arr_date_ob[arr_date_ob == "Not available"] <- NA
#         arr_date_ob <- arr_date_ob[!is.na(arr_date_ob)]
#         
#         #Extracting prices and finding the minimum of all the available prices for both, outbound
#         #and inbound trips.
#         Prices <-
#           grep(
#             "[0-9]{3}$|[0-9]{4}$|[0-9]{5}$|[Not available]+$",
#             TimeDatePlacePrice_Dep,
#             value = T
#           )
#         Prices <-
#           str_extract(Prices,
#                       "[0-9]{3}$|[0-9]{4}$|[0-9]{5}$|[Not available]+$")
#         
#         #Imputing NAs in place of "Not available" and in tapply NAs will be removed.
#         #When we extract prices, we get prices of first class and business class as well
#         #along with economy prices. So we take the minimum of those three values only.
#         Prices[Prices  == "Not available"] <- NA
#         Prices <- as.numeric(Prices)
#         len_p <- length(Prices)
#         min_price <-
#           unname(tapply(
#             X = Prices,
#             cut(1:len_p, len_p / 3),
#             FUN = min,
#             na.rm = T
#           ))
#         
#         #Final data frame containing the all the details about outbound flights.
#         DepData <-
#           as.data.frame(
#             cbind(
#               Airline = rep("British", length(dep_time_ob)),
#               DepTime = dep_time_ob,
#               ArrTime = arr_time_ob,
#               DepDate = dep_date_ob,
#               ArrDate = arr_date_ob,
#               From = dep_loc_ob,
#               To = arr_loc_ob,
#               Min_Price_ob = min_price
#             )
#           )
#         
#         #Arranging the data frame in the ascending order of the prices.
#         DepData <- arrange(DepData, as.numeric(DepData$Min_Price_ob))
#         
#         #Extracting all the relevant details for inbound trips.
#         TimeDatePlacePrice_Arr <-
#           html_nodes(
#             British,
#             "#inboundDates .priceselecter , #inboundDates .arrivaltrue , #inboundDates .departuretrue"
#           ) %>%
#           html_text()
#         
#         rem2 <- grep("[()]", TimeDatePlacePrice_Arr)
#         TimeDatePlacePrice_Arr <- TimeDatePlacePrice_Arr[-rem2]
#         TimeDatePlacePrice_Arr[TimeDatePlacePrice_Arr == "Not available"] <-
#           "NoT AVAILABLE"
#         TimeDatePlacePrice_Arr <-
#           grep(
#             "[A-Z]{3}$|[0-9]{3}$|[0-9]{4}$|[0-9]{5}$|[NOT AVAILABLE]+$",
#             TimeDatePlacePrice_Arr,
#             value = T
#           )
#         
#         l_TDPP_Arr <- length(TimeDatePlacePrice_Arr)
#         s <-
#           sort(c(
#             seq(1, l_TDPP_Arr , 8),
#             seq(1, l_TDPP_Arr, 8) + 1,
#             seq(1, l_TDPP_Arr, 8) + 2,
#             seq(1, l_TDPP_Arr, 8) + 3,
#             seq(1, l_TDPP_Arr, 8) + 4
#           ))
#         TimeDatePlacePrice_Arr <- TimeDatePlacePrice_Arr[s]
#         
#         #Extracting departure and arrival times for outbound trips only.
#         TimeDatePlacePrice_Arr[TimeDatePlacePrice_Arr == "NoT AVAILABLE"] <-
#           "Not available"
#         dep_time_ib <-
#           grep("[A-Z]{3}$", TimeDatePlacePrice_Arr, value = T)
#         dep_time_ib <-
#           grep("[BOS]$",
#                dep_time_ib,
#                value = T,
#                invert = T)
#         dep_time_ib <- str_extract(dep_time_ib, "[0-9]+[:]+[0-9]+")
#         
#         #Extracting departure and arrival times for inbound trips only.
#         arr_time_ib <-
#           grep(
#             "[A-Z]{3}$",
#             TimeDatePlacePrice_Arr,
#             ignore.case = F,
#             value = T
#           )
#         arr_time_ib <- grep("[BOS]$", arr_time_ib, value = T)
#         arr_time_ib <- str_extract(arr_time_ib, "[0-9]+[:]+[0-9]+")
#         
#         
#         #Getting arrival and departure locations for inbound trips.
#         #For departure location, we are repeating the function parameter "destination" itself, as not doing so
#         #involves a lot of work to extract arrival location because of the  structure of the website.
#         arr_loc_ib <- rep("BOS", length(dep_time_ib))
#         dep_loc_ib <- rep("destination", length(dep_time_ib))
#         
#         #Extracting departure dates for inbound trips only.
#         dep_date_ib <-
#           grep("[A-Z]{3}$", TimeDatePlacePrice_Arr, value = T)
#         dep_date_ib <-
#           grep("[BOS]$",
#                dep_date_ib,
#                value = T,
#                invert = T)
#         dep_date_ib <-
#           str_extract(dep_date_ib, "[ ]+[0-9]{2}+[ ]+[A-Za-z]{3}") %>% trimws()
#         
#         #Extracting arrival dates for inbound trips only.
#         arr_date_ib <-
#           grep("[BOS]$", TimeDatePlacePrice_Arr, value = T)
#         arr_date_ib <-
#           str_extract(arr_date_ib, "[ ]+[0-9]{2}+[ ]+[A-Za-z]{3}") %>% trimws()
#         
#         #Extracting prices for inbound trips only.
#         Prices_ib <-
#           grep(
#             "[0-9]{3}$|[0-9]{4}$|[0-9]{5}$|[Not available]+$",
#             TimeDatePlacePrice_Arr,
#             value = T
#           )
#         Prices_ib <-
#           str_extract(Prices_ib,
#                       "[0-9]{3}$|[0-9]{4}$|[0-9]{5}$|[Not available]+$")
#         
#         #Imputing NAs in place of "Not available" and in tapply NAs will be removed.
#         Prices_ib[Prices_ib  == "Not available"] <- NA
#         Prices_ib <- as.numeric(Prices_ib)
#         ln_p <- length(Prices_ib)
#         min_price_ib <-
#           unname(tapply(
#             X = Prices_ib,
#             cut(1:ln_p, ln_p / 3),
#             FUN = min,
#             na.rm = T
#           ))
#         
#         #Final data frame containing all the relevant information about the inbound trips.
#         ArrData <-
#           as.data.frame(
#             cbind(
#               ReturnAirline = rep("British", length(dep_time_ib)),
#               DepTime = dep_time_ib,
#               ArrTime = arr_time_ib,
#               DepDate = dep_date_ib,
#               ArrDate = arr_date_ib,
#               From = dep_loc_ib,
#               To = arr_loc_ib,
#               Min_Price_ib = min_price_ib
#             ),
#             stringsAsFactors = F
#           )
#         
#         #Arranging the data frame in the ascending order of the prices.
#         ArrData <- arrange(ArrData, as.numeric(ArrData$Min_Price_ib))
#         
#         #Extracting flights with minimun outbound and inbound trips, producing the final data frame
#         #with the roundtrip having minimum price.
#         final <-
#           as.data.frame(cbind(DepData[1, ], ArrData[1, ]), stringAsFactors = F)
#         Min_Price <-
#           as.numeric(levels(final$Min_Price_ob)[final$Min_Price_ob]) + as.numeric(final$Min_Price_ib)
#         final <- cbind(final, Min_Price)
#         final <- final[, -c(8, 16)]
#         ScrapeTime <- format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
#         BritishData <- cbind(final, ScrapeTime)
#         colnames(BritishData) <-
#           c(
#             "Airline",
#             "DepTime",
#             "ArrTime",
#             "From",
#             "To",
#             "DepDate",
#             "ArrDate",
#             "ReturnAirline",
#             "DepTime",
#             "ArrTime",
#             "From",
#             "To",
#             "DepDate",
#             "ArrDate",
#             "Min_Price",
#             "ScrapeTime"
#           )
#         
#       }
#       return(BritishData)
#     }
#   }
# 
# ################################################################
# ################################################################
# 
# # A Function, when called, returns a table with minimum price for flight operated by Emirates for given destination and dates from Boston.

EmiratesInput <-
  function(remDr,
           i,
           Airports,
           destination,
           departureDate,
           arrivalDate) {
    print(paste(
      c(
        "Scraping",
        " Emirates for ",
        destination,
        "; ",
        length(Airports) - i,
        " remaining"
      ),
      collapse = ""
    ))
    if (destination %in% c("IXZ",
                           "GAU",
                           "GAY",
                           "GOI",
                           "CCJ",
                           "BHO",
                           "NAG",
                           "IMF",
                           "BBI",
                           "SXR",
                           "ATQ")) {
      print(paste(c(
        destination, " not serviced by Emirates"
      ), collapse = ""))#Checks for aiports not serviced by airline
      return("no flights")
    } else{
      departureDate <- as.Date(departureDate, "%m/%d/%Y")
      departureDate <-
        paste(c(
          "day-",
          day(departureDate),
          "-",
          month(departureDate) - 1,
          "-",
          year(departureDate)
        ), collapse = "")
      arrivalDate <- as.Date(arrivalDate, "%m/%d/%Y")
      arrivalDate <-
        paste(c(
          "day-",
          day(arrivalDate),
          "-",
          month(arrivalDate) - 1,
          "-",
          year(arrivalDate)
        ), collapse = "")
      url <-
        "http://fly4.emirates.com/CAB/IBE/SearchAvailability.aspx"
      remDr$navigate(url)
      Sys.sleep(10)
      wxbox <-
        remDr$findElement(using = 'id', "ctl00_c_CtWNW_ddlFrom-suggest")
      wxbox$clearElement()
      wxbox$sendKeysToElement(list("BOS"))
      Sys.sleep(2)
      wxbox$sendKeysToElement(list(key = "down_arrow"))
      wxbox$sendKeysToElement(list(key = "enter"))
      wxbox <- remDr$findElement(using = 'id', "txtDepartDate")
      wxbox$clickElement()
      Sys.sleep(2)
      # The following code searches for departure date by navigating through months of the website's calender
      repeat {
        tryCatch({
          wxbox <- remDr$findElement(using = 'id', departureDate)
          wxbox$clickElement()
          break
        }, error = function(e) {
          wxbox <- remDr$findElement(using = 'id', 'nextMonth')
          wxbox$clickElement()
        })
      }
      Sys.sleep(2)
      wxbox <-
        remDr$findElement(using = 'id', "ctl00_c_CtWNW_ddlTo-suggest")
      wxbox$clearElement()
      wxbox$sendKeysToElement(list(destination)) #Inserts destination argument as arrival airport
      Sys.sleep(2)
      wxbox$sendKeysToElement(list(key = "down_arrow"))
      wxbox$sendKeysToElement(list(key = "enter"))
      wxbox <- remDr$findElement(using = 'id', "txtarrivaldate")
      wxbox$clickElement()
      Sys.sleep(2)
      # The following code searches for arrival date by navigating through months of the website's calender
      repeat {
        tryCatch({
          wxbox <- remDr$findElement(using = 'id', arrivalDate)
          wxbox$clickElement()
          break
        }, error = function(e) {
          wxbox <- remDr$findElement(using = 'id', 'nextMonth')
          wxbox$clickElement()
        })
      }
      Sys.sleep(2)
      webElem <-
        remDr$findElement(using = 'css selector', "#ctl00_c_IBE_PB_FF")
      webElem$clickElement()
      Sys.sleep(10)
      ################################################################
      htm <-
        htmlParse(remDr$getPageSource()[[1]]) #Extracts current page's HTML code
      htm <- readHTMLTable(htm, stringsAsFactors = FALSE)
      wxbox <- remDr$findElements(using = 'class', "flights-num")

      # The following code checks for connecting flight and then scrapes the webpage
      if (unlist(htm[[3]])[!is.na(unlist(htm[[3]]))][1] == "Leg") {
        departure1 <- vector("character")
        arrival1 <- vector("character")
        deptime1 <- vector("character")
        arrivaltime1 <- vector("character")
        depdate1 <- vector("character")
        arrivaldate1 <- vector("character")
        departure2 <- vector("character")
        arrival2 <- vector("character")
        deptime2 <- vector("character")
        arrivaltime2 <- vector("character")
        depdate2 <- vector("character")
        arrivaldate2 <- vector("character")
        minprice <- vector("character")
        x <- htm[[2]][!is.na(htm[[2]])]
        x <- x[which(x != "")]
        minprice <-
          as.numeric(substring(gsub(",", "", unlist(
            strsplit(unlist(strsplit(x[2], "\n"))[2], " ")
          )[2]), 1, nchar(gsub(
            ",", "", unlist(strsplit(unlist(
              strsplit(x[2], "\n")
            )[2], " "))[2]
          )) / 2)) #Extracts minimum flight price for selected itinerary
        x <- htm[[3]][!is.na(htm[[3]])]
        x <- x[which(x != "")]
        departure1 <- c(departure1, substring(x[2], 1, 3))
        arrival1 <- c(arrival1, x[length(x) - 8])
        #Extracts departure flight itenrary
        if (substring(x[5], 1, 1) %in% c(0:9)) {
          deptime1 <- c(deptime1, substring(x[5], 1, 5))
        } else{
          deptime1 <- c(deptime1, substring(x[6], 1, 5))
        }
        arrivaltime1 <- c(arrivaltime1, substring(x[length(x) - 9], 1, 5))
        if (substring(x[5], 1, 1) %in% c(0:9)) {
          depdate1 <-
            c(depdate1, gsub(",", "", paste(
              c(unlist(strsplit(x[5], " "))[2:4], substring(unlist(
                strsplit(x[5], " ")
              )[5], 1, 4)), collapse = " "
            )))
          depdate1 <-
            format(as.Date(depdate1, "%A %d %B %Y"), "%m/%d/%Y")
        } else{
          depdate1 <-
            c(depdate1, gsub(",", "", paste(
              c(unlist(strsplit(x[6], " "))[2:4], substring(unlist(
                strsplit(x[6], " ")
              )[5], 1, 4)), collapse = " "
            )))
          depdate1 <- format(as.Date(depdate1, "%A %d %B"), "%m/%d/%y")
        }
        arrivaldate1 <-
          c(arrivaldate1, gsub(",", "", paste(c(
            unlist(strsplit(x[length(x) - 9], " "))[2:4], substring(unlist(strsplit(x[length(x) -
                                                                                        9], " "))[5], 1, 4)
          ), collapse = " ")))
        arrivaldate1 <-
          format(as.Date(arrivaldate1, "%A %d %B %Y"), "%m/%d/%Y")
        len <- vector("numeric")
        for (pq in seq(1:length(htm))) {
          len[pq] <- length(unlist(htm[[pq]]))
        }
        pq <- which(len >= (len[3] - 15))[2]
        x <- htm[[pq]][!is.na(htm[[pq]])]
        x <- x[which(x != "")]
        departure2 <- c(departure2, substring(x[2], 1, 3))
        gqm <- vector("numeric")
        km <- 1
        for (gq in seq(1:length(x))) {
          if (sum(match(":", strsplit(x[gq], "")[[1]]), na.rm = T) != 0) {
            gqm[km] <- gq
            km <- km + 1
          }
        }
        gqm[length(gqm)]
        arrival2 <- c(arrival2, x[gqm[length(gqm)] + 1])
        if (substring(x[5], 1, 1) %in% c(0:9)) {
          deptime2 <- c(deptime2, substring(x[5], 1, 5))
        } else{
          deptime2 <- c(deptime2, substring(x[6], 1, 5))
        }
        arrivaltime2 <-
          c(arrivaltime2, substring(x[gqm[length(gqm)]], 1, 5))

        #Extracts arrival itinerary
        if (substring(x[5], 1, 1) %in% c(0:9)) {
          depdate2 <-
            c(depdate2, gsub(",", "", paste(
              c(unlist(strsplit(x[5], " "))[2:4], substring(unlist(
                strsplit(x[5], " ")
              )[5], 1, 4)), collapse = " "
            )))
          depdate2 <-
            format(as.Date(depdate2, "%A %d %B %Y"), "%m/%d/%Y")
        } else{
          depdate2 <-
            c(depdate2, gsub(",", "", paste(
              c(unlist(strsplit(x[6], " "))[2:4], substring(unlist(
                strsplit(x[6], " ")
              )[5], 1, 4)), collapse = " "
            )))
          depdate2 <-
            format(as.Date(depdate2, "%A %d %B %Y"), "%m/%d/%Y")
        }
        arrivaldate2 <-
          c(arrivaldate2, gsub(",", "", paste(c(
            unlist(strsplit(x[gqm[length(gqm)]], " "))[2:4], substring(unlist(strsplit(x[gqm[length(gqm)]], " "))[5], 1, 4)
          ), collapse = " ")))
        arrivaldate2 <-
          format(as.Date(arrivaldate2, "%A %d %B %Y"), "%m/%d/%Y")
        DepartureAirline <- "Emirates"
        From <- departure1
        DepDate <- depdate1
        DepTime <- deptime1
        To <- arrival1
        ArrDate <- arrivaldate1
        ArrTime <- arrivaltime1
        emiratesd <-
          data.frame(
            DepartureAirline,
            DepTime,
            ArrTime,
            From,
            To,
            DepDate,
            ArrDate,
            stringsAsFactors = FALSE
          )
        ReturnAirline <- "Emirates"
        From <- departure2
        DepDate <- depdate2
        DepTime <- deptime2
        To <- arrival2
        ArrDate <- arrivaldate2
        ArrTime <- arrivaltime2
        emiratesr <-
          data.frame(ReturnAirline,
                     DepTime,
                     ArrTime,
                     From,
                     To,
                     DepDate,
                     ArrDate,
                     stringsAsFactors = FALSE)
        Min_price <- minprice
        ScrapeTime <- format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
        emiratestable <-
          cbind(emiratesd, emiratesr, Min_price, ScrapeTime)
        return(emiratestable)
      } else if (length(wxbox) < 2) {
        #Checks for flight availability
        emiratestable <- "no flights"
        return(emiratestable)
      } else{
        o <- 1
        for (p in seq(1:length(wxbox))) {
          if (wxbox[[p]]$getElementText() != "") {
            if (o == 1) {
              outboundflights <- as.numeric(wxbox[[p]]$getElementText())
              o <- o + 1
            } else{
              inboundflights <- as.numeric(wxbox[[p]]$getElementText())
            }
          }
        }
        departure1 <- vector("character")
        arrival1 <- vector("character")
        deptime1 <- vector("character")
        arrivaltime1 <- vector("character")
        depdate1 <- vector("character")
        arrivaldate1 <- vector("character")
        minprice1 <- vector("numeric")
        for (i in seq(10, 10 + (outboundflights - 1) * 11, 11)) {
          x <- data.frame(unlist(htm[[i]]), stringsAsFactors = FALSE)
          x <- x[!is.na(x[, 1]), ]
          x <- gsub("\n", " ", x)
          departure <- gsub(" ", "", gsub("Depart.*", "", x[1]))
          departure1 <- c(departure1, departure)
          arrival <-
            gsub(" ", "", gsub(".*Airport", "", gsub("Arrive.*", "", x[1])))
          arrival1 <- c(arrival1, arrival)
          deptime1 <- c(deptime1, strsplit(x[2], " ")[[1]][4])
          depdate <-
            as.Date(paste(
              strsplit(x[2], " ")[[1]][16],
              strsplit(x[2], " ")[[1]][17],
              substring(
                departureDate,
                nchar(departureDate) - 3,
                nchar(departureDate)
              ),
              collapse = " "
            ),
            "%d %B %Y")
          depdate <- format(depdate, "%m/%d/%Y")
          depdate1 <- c(depdate1, depdate)
          arrivaltime1 <- c(arrivaltime1, strsplit(x[6], " ")[[1]][4])
          arrivaldate <-
            as.Date(paste(
              strsplit(x[6], " ")[[1]][16],
              strsplit(x[6], " ")[[1]][17],
              substring(
                departureDate,
                nchar(departureDate) - 3,
                nchar(departureDate)
              ),
              collapse = " "
            ),
            "%d %B %Y")
          arrivaldate <- format(arrivaldate, "%m/%d/%Y")
          arrivaldate1 <- c(arrivaldate1, arrivaldate)
          priceid <- vector("numeric")
          prices <- vector("numeric")
          k <- 1
          for (j in seq(1:length(x))) {
            if (!is.na(match("USD", strsplit(x, " ")[[j]]))) {
              priceid[k] <- j
              prices[k] <-
                strsplit(x, " ")[[j]][match("USD", strsplit(x, " ")[[j]]) + 1]
              k <- k + 1
            }
          }
          prices <- as.numeric(gsub(",", "", prices))
          minprice1 <- c(minprice1, min(prices))
        }
        departure2 <- vector("character")
        arrival2 <- vector("character")
        deptime2 <- vector("character")
        arrivaltime2 <- vector("character")
        depdate2 <- vector("character")
        arrivaldate2 <- vector("character")
        minprice2 <- vector("numeric")
        if (length(htm[[10 + (outboundflights - 1) * 11 + 11]]) > 1) {
          n <- 10 + (outboundflights - 1) * 11 + 11
        } else{
          n <- 10 + (outboundflights - 1) * 11 + 12
        }
        for (i in seq(n, n + (inboundflights - 1) * 11, 11)) {
          x <- data.frame(unlist(htm[[i]]), stringsAsFactors = FALSE)
          x <- x[!is.na(x[, 1]), ]
          x <- gsub("\n", " ", x)
          departure <- gsub(" ", "", gsub("Depart.*", "", x[1]))
          departure2 <- c(departure2, departure)
          arrival <-
            gsub(" ", "", gsub(".*Airport", "", gsub("Arrive.*", "", x[1])))
          arrival2 <- c(arrival2, arrival)
          deptime2 <- c(deptime2, strsplit(x[2], " ")[[1]][4])
          depdate <-
            as.Date(paste(
              strsplit(x[2], " ")[[1]][16],
              strsplit(x[2], " ")[[1]][17],
              substring(
                arrivalDate,
                nchar(arrivalDate) - 3,
                nchar(arrivalDate)
              ),
              collapse = " "
            ),
            "%d %B %Y")
          depdate <- format(depdate, "%m/%d/%Y")
          depdate2 <- c(depdate2, depdate)
          arrivaltime2 <- c(arrivaltime2, strsplit(x[6], " ")[[1]][4])
          arrivaldate <-
            as.Date(paste(
              strsplit(x[6], " ")[[1]][16],
              strsplit(x[6], " ")[[1]][17],
              substring(
                arrivalDate,
                nchar(arrivalDate) - 3,
                nchar(arrivalDate)
              ),
              collapse = " "
            ),
            "%d %B %Y")
          arrivaldate <- format(arrivaldate, "%m/%d/%Y")
          arrivaldate2 <- c(arrivaldate2, arrivaldate)
          priceid <- vector("numeric")
          prices <- vector("numeric")
          k <- 1
          for (j in seq(1:length(x))) {
            if (!is.na(match("USD", strsplit(x, " ")[[j]]))) {
              priceid[k] <- j
              prices[k] <-
                strsplit(x, " ")[[j]][match("USD", strsplit(x, " ")[[j]]) + 1]
              k <- k + 1
            }
          }
          prices <- as.numeric(gsub(",", "", prices))
          minprice2 <- c(minprice2, min(prices))
        }
        c1 <- which(minprice1 == min(minprice1))
        c2 <- which(minprice2 == min(minprice2))
        DepartureAirline <- "Emirates"
        From <- departure1[c1[1]]
        DepDate <- depdate1[c1[1]]
        DepTime <- deptime1[c1[1]]
        To <- arrival1[c1[1]]
        ArrDate <- arrivaldate1[c1[1]]
        ArrTime <- arrivaltime1[c1[1]]
        emiratesd <-
          data.frame(
            DepartureAirline,
            DepTime,
            ArrTime,
            From,
            To,
            DepDate,
            ArrDate,
            stringsAsFactors = FALSE
          )
        ReturnAirline <- "Emirates"
        From <- departure2[c2[1]]
        DepDate <- depdate2[c2[1]]
        DepTime <- deptime2[c2[1]]
        To <- arrival2[c2[1]]
        ArrDate <- arrivaldate2[c2[1]]
        ArrTime <- arrivaltime2[c2[1]]
        emiratesr <-
          data.frame(ReturnAirline,
                     DepTime,
                     ArrTime,
                     From,
                     To,
                     DepDate,
                     ArrDate,
                     stringsAsFactors = FALSE)
        Min_price <- minprice1[c1[1]] + minprice2[c2[1]]
        ScrapeTime <- format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
        emiratestable <-
          cbind(emiratesd[1, ], emiratesr[1, ], Min_price, ScrapeTime)
        return(emiratestable)
      }
    }
  }

################################################################################################################################

# A Function, when called, returns a table with minimum price for flight operated by Jet Airways for given destination and dates from Boston.

JetAirwaysInput <-
  function(remDr,
           i,
           Airports,
           destination,
           departureDate,
           arrivalDate) {
    print(paste(
      c(
        "Scraping",
        " Jet Airways for ",
        destination,
        "; ",
        length(Airports) - i,
        " remaining"
      ),
      collapse = ""
    ))
    if (destination %in% c("GAY", "BBI")) {
      #Checks for aiports not serviced by airline
      print(paste(c(
        destination, " not serviced by Jet Airways"
      ), collapse = ""))
      return("no flights")
    } else{
      url <-
        "http://www.jetairways.com/EN/IN/planyourtravel/book-online.aspx"
      remDr$navigate(url)
      Sys.sleep(10)
      wxbox <-
        remDr$findElement(using = 'id', "ctl00_MainBody_ctl00_autoOrigin_txtCountry")
      wxbox$sendKeysToElement(list("BOS"))
      Sys.sleep(5)
      wxbox <-
        remDr$findElements(using = 'css selector', '.autocomplete-suggestion')
      #The following code searches for Boston in the auto complete suggestions.
      for (i in seq(1:length(wxbox))) {
        for (j in seq(1:length(str_split(wxbox[[i]]$getElementText(), " ")[[1]]))) {
          if (str_split(wxbox[[i]]$getElementText(), " ")[[1]][j] == "(BOS)")
          {
            wxbox[[i]]$clickElement()
          }
        }
      }

      wxbox <-
        remDr$findElement(using = 'id',
                          "ctl00_MainBody_ctl00_autoDestination_txtCountry")
      wxbox$sendKeysToElement(list(destination))
      destination <- paste(c("(", destination, ")"), collapse = "")
      Sys.sleep(2)
      wxbox <-
        remDr$findElements(using = 'css selector', '.autocomplete-suggestion')
      #The following code searches for destination in the auto complete suggestions.
      for (i in seq(1:length(wxbox))) {
        for (j in seq(1:length(str_split(wxbox[[i]]$getElementText(), " ")[[1]]))) {
          if (str_split(wxbox[[i]]$getElementText(), " ")[[1]][j] == destination)
          {
            wxbox[[i]]$clickElement()
          }
        }
      }
      wxbox <-
        remDr$findElement(using = 'css selector', ".icon-calendar")
      wxbox$clickElement()
      departureDate <- as.Date(departureDate, "%m/%d/%Y")
      #The following code searches for departure year in website's calender
      repeat {
        if (remDr$findElement(using = 'class', 'ui-datepicker-year')$getElementAttribute('value') ==
            year(departureDate)) {
          break
        } else{
          wxbox <- remDr$findElement(using = 'class', 'ui-datepicker-year')
          wxbox$clickElement()
          wxbox$sendKeysToElement(list(key = "down_arrow"))
        }
      }
      #The following code searches for departure month in website's calender.
      if (remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value') <
          (month(departureDate) - 1)) {
        repeat {
          if (remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value') ==
              (month(departureDate) - 1)) {
            break
          } else{
            wxbox <- remDr$findElement(using = 'class', 'ui-datepicker-month')
            wxbox$clickElement()
            wxbox$sendKeysToElement(list(key = "down_arrow"))
          }
        }
      } else{
        repeat {
          if (remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value') ==
              (month(departureDate) - 1)) {
            break
          } else{
            wxbox <- remDr$findElement(using = 'class', 'ui-datepicker-month')
            wxbox$clickElement()
            wxbox$sendKeysToElement(list(key = "up_arrow"))
          }
        }
      }
      #The following code searches for departure date in website's calender
      wxbox <-
        remDr$findElements(using = 'css selector', '.ui-state-default')
      dateind <- vector("character")
      k <- 1
      for (i in seq(1:length(wxbox))) {
        if (wxbox[[i]]$getElementText() == day(departureDate))
        {
          dateind[k] <- i
          k <- k + 1
        }
      }
      if (length(dateind) > 1) {
        j <- as.numeric(dateind[2])
        wxbox[[j]]$clickElement()
      } else{
        j <- as.numeric(dateind[1])
        wxbox[[j]]$clickElement()
      }

      arrivalDate <- as.Date(arrivalDate, "%m/%d/%Y")
      wxbox <- remDr$findElement(using = 'id', 'txtEndDate')
      wxbox$clickElement()
      #The following code searches for arrival year in website's calender
      repeat {
        if (remDr$findElement(using = 'class', 'ui-datepicker-year')$getElementAttribute('value') ==
            year(arrivalDate)) {
          break
        } else{
          wxbox <- remDr$findElement(using = 'class', 'ui-datepicker-year')
          wxbox$clickElement()
          wxbox$sendKeysToElement(list(key = "down_arrow"))
        }
      }

      #The following code searches for arrival month in website's calender
      if (remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value') <
          (month(arrivalDate) - 1)) {
        repeat {
          if (remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value') ==
              (month(arrivalDate) - 1)) {
            break
          } else{
            wxbox <- remDr$findElement(using = 'class', 'ui-datepicker-month')
            wxbox$clickElement()
            wxbox$sendKeysToElement(list(key = "down_arrow"))
          }
        }
      } else{
        repeat {
          if (remDr$findElement(using = 'class', 'ui-datepicker-month')$getElementAttribute('value') ==
              (month(arrivalDate) - 1)) {
            break
          } else{
            wxbox <- remDr$findElement(using = 'class', 'ui-datepicker-month')
            wxbox$clickElement()
            wxbox$sendKeysToElement(list(key = "up_arrow"))
          }
        }
      }
      #The following code searches for arrival day in website's calender
      wxbox <-
        remDr$findElements(using = 'css selector', '.ui-state-default')
      dateind <- vector("character")
      k <- 1
      for (i in seq(1:length(wxbox))) {
        if (wxbox[[i]]$getElementText() == day(arrivalDate))
        {
          dateind[k] <- i
          k <- k + 1
        }
      }
      if (length(dateind) > 1) {
        j <- as.numeric(dateind[2])
        wxbox[[j]]$clickElement()
      } else{
        j <- as.numeric(dateind[1])
        wxbox[[j]]$clickElement()
      }
      wxbox <-
        remDr$findElement(using = 'id', "MainBody_ctl00_btnBookOnline")
      wxbox$clickElement()
      Sys.sleep(60)
      ################################################################
      t <-
        read_html(remDr$getPageSource()[[1]]) #Extracts current page's HTML code
      #scraping the time iformation from the website
      time <- html_nodes(t, ".journey__time") %>% html_text()
      #check weather the flight exists by check the basic conetent length
      if (length(time) == 0) {
        jet <- "no flights"
      } else{
        loc <- html_nodes(t, "abbr") %>% html_text()
        date <-
          as.Date(str_split_fixed(
            html_nodes(t, ".travel-info--date") %>% html_text(),
            ",",
            2
          )[, 2],
          "%d %b %Y")
        #format the price information into standard numeric data
        price <-
          gsub("\n*|\\t*|\\s*",
               "",
               html_nodes(t, ".items") %>% html_text())
        price <- as.numeric(gsub("[^.0123456789]", "", price))
        #as the number of columns of the price information maybe different as the flight change, some may
        #do not the whole class, so we build a formula to calculate the number of columns
        pricegroup <- array("0", c(4, 1, length(price) / 4))
        k <- 1
        for (i in seq(1, length(price), 4)) {
          pricegroup[, , k] <- as.matrix(price[i:(i + 3)])
          k <- k + 1
        }
        #after get the correct group price for each flight, we get the minimum price as the price for that flight
        pricemin <- vector("numeric")
        k <- 1
        for (i in seq(1, length(price) / 4, 2)) {
          pricemin[k] <- min(as.numeric(pricegroup[, , i]), na.rm = T)
          k <- k + 1
        }
        data <- cbind(time, as.character(date))
        data <- cbind(data, loc)
        #scraping the location as 3 capital-character symbol
        place <-
          unique(str_extract(
            html_nodes(t, ".airport") %>% html_text(),
            "[A-Z][A-Z][A-Z]"
          ))
        newdata <- data[which(data[, 3] == place[1] |
                                data[, 3] == place[2]), ]
        departure <- newdata[seq(1, nrow(newdata), by = 2), ]
        arrival <- newdata[seq(2, nrow(newdata), by = 2), ]
        jet <- data.frame(departure, arrival)
        ScrapeTime <- format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
        jet <- cbind(jet, pricemin, stringsAsFactors = FALSE)
        name <- rep("Jet Airways", nrow(jet))
        jet <- cbind(name, jet)
        #get the data frame for jet airways
        colnames(jet) <-
          c(
            "NameOfAirline",
            "DepTime",
            "DepDate",
            "From",
            "ArrTime",
            "ArrDate",
            "To",
            "PriceInUsd"
          )
        jet <-
          data.frame(
            Airline = jet[, 1],
            DepTime = jet[, 2],
            ArrTime = jet[, 5],
            From = jet[, 4],
            To = jet[, 7],
            DepDate = jet[, 3],
            ArrDate = jet[, 6],
            Min_Price = jet[, 8],
            stringsAsFactors = FALSE
          )
        #let the date as the character format in case of getting error when combining the data frame with other frame
        jet$DepDate <- as.character(jet$DepDate)
        jet$ArrDate <- as.character(jet$ArrDate)
        #choose the departure flight with the minimum price and the returnflight with minimum price and combine them as one row and the sum of these two price as the final price for round trip.
        dedata <- jet[which(jet$From == jet$From[1]), ]
        redata <- jet[which(jet$From == jet$From[nrow(jet)]), ]
        debest <-
          dedata[which(dedata$Min_Price == min(dedata$Min_Price)), ][1, ]
        names(debest)[names(debest) == "Airline"] = "DepartureAirline"
        rebest <-
          redata[which(redata$Min_Price == min(redata$Min_Price)), ][1, ]
        names(rebest)[names(rebest) == "Airline"] = "ReturnAirline"
        Min_price <-
          sum(as.numeric(debest$Min_Price),
              as.numeric(rebest$Min_Price))
        jet <- cbind(debest[, -8], rebest[, -8])
        jet <- cbind(jet, Min_price)
        jet <- cbind(jet, ScrapeTime)
      }
      return(jet)
    }
  }

################################################################################################################################
# A Function, when called, returns a table with minimum price for flight operated by Qatar Airways for given destination and dates from Boston.

QatarInput <-
  function(remDr,
           i,
           Airports,
           destination,
           departureDate,
           arrivalDate) {
    print(paste(
      c(
        "Scraping",
        " Qatar Airways for ",
        destination,
        "; ",
        length(Airports) - i,
        " remaining"
      ),
      collapse = ""
    ))
    if (destination %in% c(
      "IXZ",
      "VTZ",
      "GAU",
      "GAY",
      "SXR",
      "IXE",
      "BHO",
      "IMF",
      "BBI",
      "JAI",
      "CJB",
      "IXM",
      "TRZ",
      "LKO",
      "VNS"
    )) {
      print(paste(c(
        destination, " not serviced by Qatar Airways"
      ), collapse = ""))
      return("no flights")
    } else{
      url <- "http://www.qatarairways.com/us/en/homepage.page"
      remDr$navigate(url)
      wxbox <- remDr$findElement(using = 'id', "FromTemp")
      wxbox$sendKeysToElement(list("BOS"))
      Sys.sleep(2)
      wxbox$sendKeysToElement(list(key = "enter"))
      wxbox <- remDr$findElement(using = 'id', "ToTemp")
      Sys.sleep(2)
      wxbox$sendKeysToElement(list(destination))
      Sys.sleep(2)
      wxbox$sendKeysToElement(list(key = "enter"))
      wxbox <- remDr$findElement(using = 'id', "departing")
      wxbox$clearElement()
      departureDate <- as.Date(departureDate, "%m/%d/%Y")
      departureDate <- format(departureDate, "%d-%b-%Y")
      wxbox$sendKeysToElement(list(departureDate))
      wxbox$sendKeysToElement(list(key = "enter"))
      wxbox <- remDr$findElement(using = 'id', "returning")
      wxbox$clearElement()
      arrivalDate <- as.Date(arrivalDate, "%m/%d/%Y")
      arrivalDate <- format(arrivalDate, "%d-%b-%Y")
      wxbox$sendKeysToElement(list(arrivalDate))
      wxbox$sendKeysToElement(list(key = "enter"))
      wxbox <- remDr$findElement(using = 'id', "bookFlight")
      wxbox$clickElement()
      Sys.sleep(10)
      h <- read_html(remDr$getPageSource()[[1]])
      #get the time and location
      test<-html_nodes(h,".orange-box")%>%html_text()
      place <- html_nodes(h, "span.time-head") %>% html_text()
      if (nchar(test) >200) {
        Qatardata <- "no flights"
      } else{
        place <- place[which(nchar(place) > 50)]
        place <- trimws(place)
        place <- place[which(place != "")]
        information <- str_split_fixed(place, "\n", 2)
        odd <- seq(1, nrow(information), by = 2)
        depinf <- information[odd, ]
        arrinf <- information[-odd, ]
        for (i in 1:nrow(arrinf)) {
          if (arrinf[i] == arrinf[i + 1]) {
            depnum <- i + 1
          }
          else{
            break
          }
        }
        data <- cbind(depinf, arrinf)
        for (i in 1:ncol(data)) {
          data[, i] <- gsub("\t", "", data[, i])
          data[, i] <- gsub("\n", "", data[, i])
        }

        #get the price
        #The following code first searches for different class of tickets offered by Qatar airways and what are the itenrary options. It further extracts the minimum price based on these data.

        price <-
          as.numeric(gsub(
            "[^.0123456789]",
            "",
            html_nodes(h, "span.price") %>% html_text()
          ))
        kt <-
          c(
            " Promo  Economy",
            " Saver  Economy",
            " Value  Economy",
            " Flexi  Economy",
            " Promo  Business",
            " Saver  Business"
          )
        k1 <- html_nodes(h, ".f1top") %>% html_text()
        k2 <- html_nodes(h, ".f-class") %>% html_text()
        k <- paste(k1, k2)
        ko <- match(k, kt)
        koh <- vector("numeric")
        for (i in c(2:length(ko))) {
          if (ko[i] <= ko[i - 1]) {
            koh[1] <- i - 1
          }
          koh[2] <- length(ko) - koh[1]
        }
        place1 <- html_nodes(h, "span.time-head") %>% html_text()
        place1 <- gsub("\n*|\t*|\\s*", "", place1)
        place1 <- gsub("[^[:alpha:]]", "", place1)
        place1 <-
          place1[which(!is.na(match(
            place1, c(
              destination,
              "BOS",
              "BOSday",
              paste(destination, "day", sep = "")
            )
          )))]
        k <- vector("numeric")
        for (i in c(2:length(place1))) {
          if (sum(!is.na(match(place1[i:(i - 1)], c(
            destination, paste(destination, "day", sep = "")
          )))) == 2) {
            k[1] <- (i - 1) / 4
            k[2] <- (length(place1) - (i - 1)) / 4
          }
        }
        kohk <- koh * k

        pricegroup1 <- array("0", c(koh[1], 1, k[1]))
        j <- 1
        for (i in seq(1, kohk[1], koh[1])) {
          pricegroup1[, , j] <- as.matrix(price[i:(i + koh[1] - 1)])
          j <- j + 1
        }
        pricemin1 <- vector("numeric")
        j <- 1
        for (i in seq(1:k[1])) {
          pricemin1[j] <- min(as.numeric(pricegroup1[, , i]), na.rm = T)
          j <- j + 1
        }
        pricegroup2 <- array("0", c(koh[2], 1, k[2]))
        j <- 1
        for (i in seq(kohk[1] + 1, length(price), koh[2])) {
          pricegroup2[, , j] <- as.matrix(price[i:(i + koh[2] - 1)])
          j <- j + 1
        }
        pricemin2 <- vector("numeric")
        j <- 1
        for (i in seq(1:k[2])) {
          pricemin2[j] <- min(as.numeric(pricegroup2[, , i]), na.rm = T)
          j <- j + 1
        }
        pricemin <- c(pricemin1, pricemin2)


        #get the date
        date <- html_nodes(h, "span.date") %>% html_text()
        date <- str_split_fixed(date, ",", 2)
        d <- data.frame(date[c(1, 2), 2])
        d[, 1] <- gsub("\t", "", d[, 1])
        d[, 1] <- gsub("\n", " ", d[, 1])


        #reformat the data
        a <- str_extract(data[, 4], "[0-9]")
        a[which(is.na(a) == TRUE)] <- 0
        b <- str_extract(data[, 4], "[A-Z][A-Z][A-Z]")
        d <- as.Date(d[, 1], format = "%d %b %Y")
        d <-
          rep(d, times = c(length(which(data[, 2] == " BOS")), length(which(
            data[, 2] == paste(c(" ", destination), collapse = "")
          ))))
        e <- as.character(d + as.numeric(a))
        d <- as.character(d)
        ScrapeTime <- format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
        #get the information
        Qatardata <-
          data.frame(
            Airline = rep("Qatar", nrow(data)),
            DepTime = data[, 1],
            ArrTime = data[, 3],
            From = data[, 2],
            To = b,
            DepDate = d,
            ArrDate = e,
            Min_Price = pricemin,
            stringsAsFactors = FALSE
          )
        #choose the departure flight with the minimum price and the returnflight with minimum price and combine them as one row and the sum of these two price as the final price for round trip.
        dedata <- Qatardata[which(Qatardata$From == Qatardata$From[1]), ]
        redata <-
          Qatardata[which(Qatardata$From == Qatardata$From[nrow(Qatardata)]), ]
        debest <-
          dedata[which(dedata$Min_Price == min(dedata$Min_Price,na.rm = TRUE)), ]
        names(debest)[names(debest) == "Airline"] = "DepartureAirline"
        rebest <-
          redata[which(redata$Min_Price == min(redata$Min_Price,na.rm = TRUE)), ]
        names(rebest)[names(rebest) == "Airline"] = "ReturnAirline"
        Min_price <-
          sum(as.numeric(debest$Min_Price),
              as.numeric(rebest$Min_Price))
        Qatardata <- cbind(debest[1, -8], rebest[1, -8])
        Qatardata <- cbind(Qatardata, Min_price)
        Qatardata <- cbind(Qatardata, ScrapeTime)
      }
        return(Qatardata)
    }
  }

################################################################################################################################
# A Function, when called, returns a table with minimum price for flight operated by Swiss Airlines for given destination and dates from Boston.

SwissAirlinesInput <-
  function(remDr,
           i,
           Airports,
           destination,
           departureDate,
           arrivalDate) {
    print(paste(
      c(
        "Scraping",
        " Swiss Airlines for ",
        destination,
        "; ",
        length(Airports) - i,
        " remaining"
      ),
      collapse = ""
    ))
    if (destination %in% c(
      "IXZ",
      "VTZ",
      "GAU",
      "GAY",
      "GOI",
      "AMD",
      "SXR",
      "BLR",
      "IXE",
      "COK",
      "CCJ",
      "TRV",
      "BHO",
      "NAG",
      "IMF",
      "BBI",
      "ATQ",
      "JAI",
      "MAA",
      "CJB",
      "IXM",
      "TRZ",
      "HYD",
      "LKO",
      "VNS",
      "CCU"
    )) {
      #Checks for aiports not serviced by airline
      print(paste(c(
        destination, " not serviced by Swiss Airlines"
      ), collapse = ""))
      return("no flights")
    } else{
      departureDate <- as.Date(departureDate, "%m/%d/%Y")
      departureDate <- format(departureDate, "%Y-%m-%d")
      arrivalDate <- as.Date(arrivalDate, "%m/%d/%Y")
      arrivalDate <- format(arrivalDate, "%Y-%m-%d")
      #The following code creates a custom url based on itenrary parameters.
      url <- c(
        "https://www.swiss.com/gb/en/Book/Outbound/",
        "BOS",
        "-",
        destination,
        "/from-",
        departureDate,
        "/to-",
        arrivalDate,
        "/adults-1/children-0/infants-0/class-economy/al-LX/"
      )
      url <- paste(url, collapse = "")
      remDr$navigate(url)
      Sys.sleep(10)
      ################################################################
      h <- read_html(remDr$getPageSource()[[1]])
      a <- html_nodes(h, ".js-notification-close") %>% html_text()
      if (length(a) > 2) {
        Swissdata <- "no flights"
      } else{
        loc <-
          unlist(str_extract_all(
            html_nodes(h, ".is-active .step-title") %>% html_text(),
            "[A-Z][A-Z][A-Z]"
          ))
        From <- loc[1]
        To <- loc[2]
        inf <-
          html_nodes(h, ".book-bundle-flightentry--time div") %>% html_text()
        inf <- gsub("\n", "", inf)
        day <-
          sum(as.numeric(unlist(str_extract_all(
            inf, "[+][1-9]"
          ))))
        depinf <- inf[which(str_detect(inf, loc[1]) == TRUE)]
        arrinf <- inf[which(str_detect(inf, loc[2]) == TRUE)]
        DepTime <-
          unlist(str_extract_all(depinf, "[0-9][0-9][:][0-5][0-9]"))
        ArrTime <-
          unlist(str_extract_all(arrinf, "[0-9][0-9][:][0-5][0-9]"))
        date <-
          html_nodes(h, ".is-active .book-matrix--tab-link--date") %>% html_text()
        DepDate <- as.Date(str_split_fixed(date, " ", 2)[, 2], "%d/%m/%Y")
        ArrDate <- DepDate + day
        ArrDate <- format(as.Date(ArrDate, "%Y-%m-%d"), "%m/%d/%Y")
        DepDate <- format(as.Date(DepDate, "%Y-%m-%d"), "%m/%d/%Y")
        NameOfAirline <- rep("SWISS", length(DepTime))
        price <-
          html_nodes(h, ".js-book-bundling--button") %>% html_text()
        price <-
          str_split_fixed(str_split_fixed(unlist(gsub(
            "\n", "", price
          )), "USD", 2)[, 2], "S", 2)[, 1]
        price <- gsub(",", "", gsub(" ", "", price))
        price[which(price == "")] <- Inf
        PriceInUsd <- vector(length = length(DepTime))
        for (i in 1:length(DepTime)) {
          PriceInUsd[i] <- min(price[i:6 * i])
        }
        Swissdata <-
          data.frame(
            Airline = NameOfAirline,
            DepTime = DepTime,
            ArrTime = ArrTime,
            From = From,
            To = To,
            DepDate = DepDate,
            ArrDate = ArrDate,
            Min_Price = PriceInUsd,
            stringsAsFactors = FALSE
          )
        wxbox <-
          remDr$findElements(using = 'class', 'book-bundle-button--price')
        buttonind <- vector("character")
        for (i in seq(1:length(wxbox))) {
          if (gsub("[^0123456789]", "", wxbox[[i]]$getElementText()) == PriceInUsd) {
            wxbox[[i]]$clickElement()
          }
        }
        Sys.sleep(5)
        remDr$findElement(using = 'css selector', '.btn-submit')$clickElement()
        h <- read_html(remDr$getPageSource()[[1]])
        Sys.sleep(10)
        a <- html_nodes(h, ".js-notification-close") %>% html_text()
        #check the length of basic information content to make sure weather there is no flight
        if (length(a) > 2) {
          Swissdata <- "no flights"
        } else{
          #get the location, and the odd is the departure and even is the arrival place
          loc <-
            unlist(str_extract_all(
              html_nodes(h, ".is-active .step-title") %>% html_text(),
              "[A-Z][A-Z][A-Z]"
            ))
          From <- loc[1]
          To <- loc[2]
          inf <-
            html_nodes(h, ".book-bundle-flightentry--time div") %>% html_text()
          inf <- gsub("\n", "", inf)
          #get the arrival date by add the duration day to the departure date
          day <-
            sum(as.numeric(unlist(
              str_extract_all(inf, "[+][1-9]")
            )))
          depinf <- inf[which(str_detect(inf, loc[1]) == TRUE)]
          arrinf <- inf[which(str_detect(inf, loc[2]) == TRUE)]
          #extract the time from all the time information
          DepTime <-
            unlist(str_extract_all(depinf, "[0-9][0-9][:][0-5][0-9]"))
          ArrTime <-
            unlist(str_extract_all(arrinf, "[0-9][0-9][:][0-5][0-9]"))
          date <-
            html_nodes(h, ".is-active .book-matrix--tab-link--date") %>% html_text()
          DepDate <-
            as.Date(str_split_fixed(date, " ", 2)[, 2], "%d/%m/%Y")
          ArrDate <- DepDate + day
          ArrDate <- format(as.Date(ArrDate, "%Y-%m-%d"), "%m/%d/%Y")
          DepDate <- format(as.Date(DepDate, "%Y-%m-%d"), "%m/%d/%Y")
          NameOfAirline <- rep("SWISS", length(DepTime))
          price <-
            html_nodes(h, ".js-book-bundling--button") %>% html_text()
          price <-
            str_split_fixed(str_split_fixed(unlist(gsub(
              "\n", "", price
            )), "USD", 2)[, 2], "S", 2)[, 1]
          price <- gsub(",", "", gsub(" ", "", price))
          #let the price where is not available be the Inf, so when we choose the minimum,it won't be taken into consideration
          price[which(price == "")] <- Inf
          PriceInUsd <- vector(length = length(DepTime))
          for (i in 1:length(DepTime)) {
            PriceInUsd[i] <- min(price[i:6 * i])
          }
          ScrapeTime <- format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
          Swissdata1 <-
            data.frame(
              Airline = NameOfAirline,
              DepTime = DepTime,
              ArrTime = ArrTime,
              From = From,
              To = To,
              DepDate = DepDate,
              ArrDate = ArrDate,
              Min_Price = PriceInUsd,
              stringsAsFactors = FALSE
            )
          Swissdata <- rbind(Swissdata, Swissdata1)
          #choose the departure flight with the minimum price and the returnflight with minimum price and combine them as one row and the sum of these two price as the final price for round trip.
          dedata <-
            Swissdata[which(Swissdata$From == Swissdata$From[1]), ]
          redata <-
            Swissdata[which(Swissdata$From == Swissdata$From[nrow(Swissdata)]), ]
          debest <-
            dedata[which(dedata$Min_Price == min(dedata$Min_Price)), ]
          names(debest)[names(debest) == "Airline"] = "DepartureAirline"
          rebest <-
            redata[which(redata$Min_Price == min(redata$Min_Price)), ]
          names(rebest)[names(rebest) == "Airline"] = "ReturnAirline"
          Min_price <-
            sum(as.numeric(debest$Min_Price),
                as.numeric(rebest$Min_Price))
          Swissdata <- cbind(debest[, -8], rebest[, -8])
          Swissdata <- cbind(Swissdata, Min_price)
          Swissdata <- cbind(Swissdata, ScrapeTime)
        }
      }
      return(Swissdata)
    }
  }

################################################################################################################################
# # A Function, when called, returns a table with minimum price for flight operated by Turkish Airlines for given destination and dates from Boston.
# 
TurkishAirlinesInput <-
  function(
    remDr,
    i,
    Airports,
    destination,
    departureDate,
    arrivalDate) {
    print(paste(
      c(
        "Scraping",
        " Turkish Airlines for ",
        destination,
        "; ",
        length(Airports) - i,
        " remaining"
      ),
      collapse = ""
    ))
    if (destination %in% c(
      "IXZ",
      "VTZ",
      "GAU",
      "GAY",
      "GOI",
      "SXR",
      "IXE",
      "COK",
      "CCJ",
      "TRV",
      "BHO",
      "NAG",
      "IMF",
      "BBI",
      "JAI",
      "CJB",
      "IXM",
      "TRZ",
      "LKO",
      "VNS",
      "CCU"
    )) {
      #Checks for aiports not serviced by airline
      print(paste(
        c(destination, " not serviced by Turkish Airlines"),
        collapse = ""
      ))
      return("no flights")
    } else{
      d1 <- paste(c("(", destination, ")", ","), collapse = "")
      departureDate <- as.Date(departureDate, "%m/%d/%Y")
      arrivalDate <- as.Date(arrivalDate, "%m/%d/%Y")
      url <- "https://p.turkishairlines.com/"
      # remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = list(phantomjs.binary.path = "/Users/lkm/phantomjs"))
      # remDr$open()
      remDr$navigate(url)
      remDr$maxWindowSize()
      Sys.sleep(15)
      remDr$sendKeysToActiveElement(list(destination))
      Sys.sleep(3)
      #The following code searches for arrival airport
      wxbox <- remDr$findElements(using = 'class', 'label-full')
      for (i in seq(1:length(wxbox))) {
        for (j in seq(1:length(str_split(wxbox[[i]]$getElementText(), " ")[[1]]))) {
          if (str_split(wxbox[[i]]$getElementText(), " ")[[1]][j] == d1) {
            wxbox[[i]]$clickElement()
            break
          }
        }
      }
      Sys.sleep(3)
      #The following code checks for departure year and month and selects the date
      repeat {
        mon <- 0
        if (remDr$findElement(using = 'class', 'ui-datepicker-year')$getElementText()[[1]] ==
            year(departureDate)) {
          mon <- mon + 1
        }
        for (i in seq(1:3)) {
          if (remDr$findElements(using = 'class', 'ui-datepicker-month')[[i]]$getElementText()[[1]] ==
              toupper(format(departureDate, "%B"))) {
            mon <- mon + 1
          }
        }
        if (mon == 2) {
          break
        } else{
          remDr$findElement(using = 'css selector', '.ui-icon-circle-triangle-e')$clickElement()
        }
      }
      Sys.sleep(3)
      #The following code checks for arrival year and month and selects the date
      wxbox <-
        remDr$findElements(using = 'css selector', '.ui-state-default')
      wxbox[[as.numeric(day(departureDate))]]$clickElement()
      Sys.sleep(3)
      repeat {
        mon <- 0
        if (remDr$findElement(using = 'class', 'ui-datepicker-year')$getElementText()[[1]] ==
            year(arrivalDate)) {
          mon <- mon + 1
        }
        for (i in seq(1:3)) {
          if (remDr$findElements(using = 'class', 'ui-datepicker-month')[[i]]$getElementText()[[1]] ==
              toupper(format(arrivalDate, "%B"))) {
            mon <- mon + 1
          }
        }
        if (mon == 2) {
          break
        } else{
          remDr$findElement(using = 'css selector', '.ui-icon-circle-triangle-e')$clickElement()
        }
      }
      Sys.sleep(3)
      wxbox <-
        remDr$findElements(using = 'css selector', '.ui-state-default')
      wxbox[[as.numeric(day(arrivalDate))]]$clickElement()
      Sys.sleep(3)
      remDr$findElement(using = 'id', 'executeSingleCitySubmit')$clickElement()
      Sys.sleep(20)
      ################################################################
      t <- read_html(remDr$getPageSource()[[1]])
      ##Turkish Airlines
      #get the price
      price <- html_nodes(t, ".fs-20 span") %>% html_text()
      if (length(price) == 0) {
        Turkishdata <- "no flights"
        return(Turkishdata)
      } else{
        price <- price[seq(1, length(price), by = 2)]
        depinf <-
          html_nodes(t,
                     ".showcanvasoverlay:nth-child(1) td:nth-child(2) .mb-5") %>% html_text()
        deploc <-
          html_nodes(t,
                     ".showcanvasoverlay:nth-child(1) td:nth-child(2) div:nth-child(1)") %>% html_text()
        arr <-
          html_nodes(t, ".mt-5 div:nth-child(1) , .mb-5") %>% html_text()
        arr <- arr[-seq(1, length(arr), by = 7)]
        arr <- matrix(arr, ncol = 3, byrow = TRUE)
        arr <- arr[which(arr[, 3] %in% deploc == TRUE), ]
        arr <- arr[-seq(1, length(arr), by = 2), ]
        #choose the three capital character symbol as the departure and arrival place
        deploc <- str_extract(deploc, "[A-Z][A-Z][A-Z]")
        #let the date be the character format in case of error when combinin the data with other data frame
        depdate <-
          as.character(as.Date(str_split_fixed(depinf[seq(2, length(depinf), 2)], ",", 2)[, 2], format = " %B %d"))
        arrdate <-
          as.character(as.Date(str_split_fixed(arr[, 2], ",", 2)[, 2], format = " %B %d"))
        ScrapeTime <- format(Sys.time(), "%m/%d/%Y %H:%M:%S %Z")
        Turkishdata <-
          data.frame(
            Airline = rep("Turkish", length(price)),
            DepTime = depinf[seq(1, length(depinf), 2)],
            ArrTime = arr[, 1],
            From = deploc,
            To = str_extract(arr[, 3], "[A-Z][A-Z][A-Z]"),
            DepDate = depdate,
            ArrDate = arrdate,
            Min_Price = price,
            stringsAsFactors = FALSE
          )
        #choose the departure flight with the minimum price and the returnflight with minimum price and combine them as one row and the sum of these two price as the final price for round trip.
        dedata <-
          Turkishdata[which(Turkishdata$From == Turkishdata$From[1]), ]
        redata <-
          Turkishdata[which(Turkishdata$From == Turkishdata$From[nrow(Turkishdata)]), ]
        debest <-
          dedata[which(dedata$Min_Price == min(dedata$Min_Price)), ]
        names(debest)[names(debest) == "Airline"] = "DepartureAirline"
        rebest <-
          redata[which(redata$Min_Price == min(redata$Min_Price)), ]
        names(rebest)[names(rebest) == "Airline"] = "ReturnAirline"
        Min_price <-
          sum(as.numeric(debest$Min_Price),
              as.numeric(rebest$Min_Price))
        Turkishdata <- cbind(debest[, -8], rebest[, -8])
        Turkishdata <- cbind(Turkishdata, Min_price)
        Turkishdata <- cbind(Turkishdata, ScrapeTime)
      }
      return(Turkishdata)
      # remDr$closeall()
      # remDr$close()
    }
  }

################################################################################################################################
# A Function, when called, returns a table with minimum price for flight operated by all above mentioned airlines for given dates from Boston to all possible international airpoets in India.

AirlinePrices <- function(d1, d2) {
  Airports <-
    c(
      "IXZ",
      "VTZ",
      "GAU",
      "GAY",
      "DEL"
      # "GOI",
      # "AMD",
      # "SXR",
      # "BLR",
      # "IXE",
      # "COK",
      # "CCJ",
      # "TRV",
      # "BHO",
      # "BOM",
      # "NAG",
      # "IMF",
      # "BBI",
      # "ATQ",
      # "JAI",
      # "MAA",
      # "CJB",
      # "IXM",
      # "TRZ",
      # "HYD",
      # "LKO",
      # "VNS",
      # "CCU"
    ) #An array of all international airport codes in India.
 # remDr <- remoteDriver(browserName = 'phantomjs')
#  pJS <- phantom()
  remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = list(phantomjs.binary.path = "/Users/lkm/phantomjs"))
  remDr$open()
  capture.output(remDr$open(), file = 'NUL') #Opens the phantomjs browser
  AirlineDatabase <- data.frame()
  for (i in seq(1:length(Airports))) {
    # r <- JetAirwaysInput(remDr, i, Airports, Airports[i], d1, d2)
    # if (sum(!is.na(match("no flights", r))) != 1) {
    #   AirlineDatabase <- rbind(AirlineDatabase, r)
    #   print(AirlineDatabase)
    # }
    # r <- EmiratesInput(remDr, i, Airports, Airports[i], d1, d2)
    # if (sum(!is.na(match("no flights", r))) != 1) {
    #   AirlineDatabase <- rbind(AirlineDatabase, r)
    #   print(AirlineDatabase)
    # }
    # r <- SwissAirlinesInput(remDr, i, Airports, Airports[i], d1, d2)
    # if (sum(!is.na(match("no flights", r))) != 1) {
    #   AirlineDatabase <- rbind(AirlineDatabase, r)
    #   print(AirlineDatabase)
    # }
    r <- QatarInput(remDr, i, Airports, Airports[i], d1, d2)
    if (sum(!is.na(match("no flights", r))) != 1) {
      AirlineDatabase <- rbind(AirlineDatabase, r)
      print(AirlineDatabase)
    }
    # r <- BritishAirwaysInput(remDr, i, Airports, Airports[i], d1, d2)
    # if (sum(!is.na(match("no flights", r))) != 1) {
    #   AirlineDatabase <- rbind(AirlineDatabase, r)
    #   print(AirlineDatabase)
    # }
    # r <- EtihadInput(remDr, i, Airports, Airports[i], d1, d2)
    # if (sum(!is.na(match("no flights", r))) != 1) {
    #   AirlineDatabase <- rbind(AirlineDatabase, r)
    #   print(AirlineDatabase)
    # }
    # r <- TurkishAirlinesInput(remDr, i, Airports, Airports[i], d1, d2)
    # if (sum(!is.na(match("no flights", r))) != 1) {
    #   AirlineDatabase <- rbind(AirlineDatabase, r)
    #   print(AirlineDatabase)
    # }
  }
  
  #The following code checks for AirlinePricesDb database and if exists, appends the same database with newly scraped data and if dosen't, creates a new one.
  
  if (file.exists("/Users/lkm/Desktop/data course/project/AirlinePricesDb.csv")) {
    loadDb <- read.csv("/Users/lkm/Desktop/data course/project/AirlinePricesDb.csv", stringsAsFactors = F)
    names(loadDb)[c(9:14)] <-
    paste(gsub(".1", "", colnames(loadDb[, c(9:14)])))
    #loadDb<-loadDb[,-1]
    AirlineDatabase <- rbind(loadDb, AirlineDatabase)
    write.csv(AirlineDatabase, "/Users/lkm/Desktop/data course/project/AirlinePricesDb.csv", row.names = FALSE)
    print("updating database")
  } else{
    write.csv(AirlineDatabase, "/Users/lkm/Desktop/data course/project/AirlinePricesDb.csv", row.names = FALSE)
  }
  return(AirlineDatabase)
  print("Writing new database")
  #pJS$stop()
  remDr$closeall()
  remDr$close()
}

AirlinePrices("12/17/2016", "01/13/2017")
