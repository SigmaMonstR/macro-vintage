##########################################
#DOWNLOAD VINTAGE SERIES FROM PHILLIE FED#
##########################################

setwd("/Documents/Github/macro-vintage/data")
require(rvest)

out <- read_html("https://www.philadelphiafed.org/research-and-data/real-time-center/real-time-data/data-files/first-second-third")

links <- html_nodes(out, "a") %>% html_attr("href")
links <- grep("-/media/research-and-data/real-time-center/real-time-data/data-files/files/xlsx/", links, value = T)

master <- list()
for(j in links){

  #Download data
  download.file(j, "temp.xlsx", mode = "wb", quiet = T)
  temp <- readxl::read_excel("temp.xlsx", sheet = 2, skip = 3)
  colnames(temp) <- tolower(colnames(temp))
  temp <- as.data.frame(temp)
  
  #Clean data set label
  index_name <- gsub("https://www.philadelphiafed.org:443/-/media/research-and-data/real-time-center/real-time-data/data-files/files/xlsx/", "",
                     j)
  index_name <- gsub("_first_second_third.xlsx\\?la=en", "",
                     index_name)
  
  #Run through clean fields
  for(k in 2:ncol(temp)){
    temp[,k] <- as.numeric(temp[,k])
  }
  colnames(temp)[-1] <- paste0(index_name,"_", c(1:3,"r"))
  
  #Add identifier for type
  if(length(grep("Q", temp$date))>0){
    index_name <- paste0(index_name,".Q")
  } else if(length(grep("(:\\d{1,2})", temp$date))>0){
    index_name <- paste0(index_name,".M")
  }
  master[[index_name]] <- temp
  print(index_name)
}

save(master, file = "macro_vintages.Rda")


#Pull together
  qtr <- data.frame()
  mo <- data.frame()
  
  for(i in names(master)){
    temp <- master[[i]]
    temp <- temp[!duplicated(temp$date),]
    if(any(grep("\\.Q", i))){
      
      if(nrow(qtr) == 0){
        qtr <- temp
      } else {
        qtr <- merge(qtr, temp, by = "date", all.x = T, all.y = T)
      }
    } else{
      if(nrow(mo) == 0){
        mo <- temp
      } else {
        mo <- merge(mo, temp, by = "date", all.x = T, all.y = T)
      }
    }
  }
  
  #Convert month to qtr
    mo_index <- as.numeric(substr(mo$date, 6,7))
    mo_index <- ceiling(mo_index/3)
    yrs <- paste0(substr(mo$date,1,4), ":Q",mo_index)
    mo2 <- mo
    mo2 <- cbind(yrs, mo2)
  
    tots <- data.frame()
    for(j in unique(yrs)){
      row <- as.data.frame(t(apply(mo2[mo2$yrs == j, -c(1:2)],2,FUN = function(x){mean(x, na.rm = T)})))
      tots <- rbind(tots, cbind(date = j, row))
    }
    
    for(j in 2:ncol(tots)){
      tots[is.na(tots[,j]),j] <- NA
    }
    
    qtr <- merge(qtr, tots, by = "date", all.x = T, all.y = T)
    
  save(qtr, mo, file = "macro_vintages.Rda")
