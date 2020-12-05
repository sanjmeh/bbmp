library(tidyverse)
library(data.table)
library(lubridate)
library(magrittr)
library(httr)
library(jsonlite)
library(urltools)
load("urlrcpts.RData")
# basic scraper
jscrape <- function(x) read_lines(x) %>% fromJSON %>% as.data.table 

jscrape_wr <- function(x){
        y <- x %>%  modify_url(query = list(pReceiptMainID=i)) %>% {try(jscrape(.))}
        while(unlist(y)[1] %>% grepl("^Error",.)) 
        {
                message("...retrying id ",param_get(x,"pReceiptMainID")," in next 5 seconds.",appendLF = F)
                Sys.sleep(5)
                x <- url1 %>%  modify_url(query = list(pReceiptMainID=i)) %>% {try(jscrape(.))}
                
        }
        
}
        

# pass a vector of ids scraped earlier and all 3 popups of the receipts on url1,2,3 will be dumped.
# this function is now checking already processed files and skipping them.
scrape_bbm_receipts <- function(inputfile="id1.csv",oprfx="SM",datapath="~/Dropbox/bbmp",urlval=urlrcpt2){
        
        stopifnot(dir.exists(datapath), inputfile %>% file.path(datapath,.) %>% file.exists )
        basefile <- inputfile %>% str_split("\\.",simplify = T,n=2) %>% as.character() %>% first()
        inputpath <-  inputfile %>% file.path(datapath,.)
        existing_matches <- list.files(path = datapath,pattern = paste0(basefile,".RDS") )
        outputpath <- paste(oprfx,basefile,sep = "_") %>% paste0(".RDS") %>% file.path(datapath,.)
        if(length(existing_matches)>0) 
        {
                message("Oops! this file has already been processed:",existing_matches)
                return(NA)
        }
        url1 <- urlval %>% modify_url(query = list(pFromWhere=1))
        url2 <- urlval %>% modify_url(query = list(pFromWhere=2))
        url3 <- urlval %>% modify_url(query = list(pFromWhere=3))
        dt1 <-  data.table()
        dt2 <-  data.table()
        dt3 <-  data.table()
        id_dt <- fread(inputpath)
        stopifnot(nrow(id_dt)>0)
        id_vect <- id_dt[[1]] %>% as.numeric
        cat("\nWeb scraping started for file", inputfile, ": Donot disturb the internet connection.\nurl -1 in progress. ")
        for(i in id_vect){
                x <- url1 %>%  modify_url(query = list(pReceiptMainID=i)) %>% {try(jscrape(.))}
                while(unlist(x)[1] %>% grepl("^Error",.)) 
                {
                        message("...retrying id ",i," in next 5 seconds.",appendLF = F) 
                        Sys.sleep(5)
                        x <- url1 %>%  modify_url(query = list(pReceiptMainID=i)) %>% {try(jscrape(.))}
                        
                }
                dt1 <- rbind(dt1,x)
                cat(". ")
        }
        cat("ended\n")
        
        cat("url -2 started. ")
        l2 <- list()
        for(i in id_vect){
                x <- url2 %>%  modify_url(query = list(pReceiptMainID=i)) %>% {try(read_lines(.) %>% fromJSON())}
                while(unlist(x)[1] %>% grepl("^Error",.)) 
                {
                        message("...retrying id ",i," in next 5 seconds.",appendLF = F) 
                        Sys.sleep(5)
                        x <- url2 %>%  modify_url(query = list(pReceiptMainID=i)) %>% {try(read_lines(.) %>% fromJSON())}
                        
                }
                l2 <- c(l2,list(x=x))
                cat(". ")
        }
        cat("ended\n")
        
        dt2 <- data.table(form2=l2)
        
        cat("url -3 started. ")
        l3 <- list()        
        for(i in id_vect){
                x <- url3 %>%  modify_url(query = list(pReceiptMainID=i))  %>% {try(read_lines(.) %>% fromJSON())}
                while(unlist(x)[1] %>% grepl("^Error",.)) 
                {
                        message("...retrying id ",i," in next 5 seconds.",appendLF = F) 
                        Sys.sleep(5)
                        x <- url2 %>%  modify_url(query = list(pReceiptMainID=i)) %>% {try(read_lines(.) %>% fromJSON())}
                        
                }
                l3 <- c(l3,list(x=x))
                cat(". ")
        }
        cat("ended\n")
        
        dt3 <- data.table(form3=l3)
        dt <- cbind(id_dt,dt1,dt2,dt3)
        if(nrow(dt)>0)  message(sprintf("New table created with %d receipts totalling to Rs %d ",dt[,.N],dt$amount %>% as.numeric %>% sum))
        cat("Saving output to ",outputpath,"\n")
        saveRDS(dt,outputpath)
}

scrape_continuous <- function(pat="id[2-9][0-9].txt",prf="SM"){
        list.files("~/Dropbox/bbmp",pattern = pat) %>% 
                 walk(scrape_bbm_receipts,oprfx=prf)
}
