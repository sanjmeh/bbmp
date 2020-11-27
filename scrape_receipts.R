library(tidyverse)
library(data.table)
library(magrittr)
library(httr)
library(jsonlite)
load("urlrcpts.RData")
# basic scraper
jscrape <- function(x) read_lines(x) %>% fromJSON %>% as.data.table 

# pass a vector of ids scraped earlier and all 3 popups of the receipts on url1,2,3 will be dumped.
scrape_bbm_receipts <- function(id_vect,prfx="SM-001",urlval=urlrcpt2){
        url1 <- urlval %>% modify_url(query = list(pFromWhere=1))
        url2 <- urlval %>% modify_url(query = list(pFromWhere=2))
        url3 <- urlval %>% modify_url(query = list(pFromWhere=3))
        dt1 <-  data.table()
        dt2 <-  data.table()
        dt3 <-  data.table()
        cat("url -1 started..")
        for(i in id_vect){
                x <- url1 %>%  modify_url(query = list(pReceiptMainID=i)) %>% jscrape
                dt1 <- rbind(dt1,x)
        }
        cat("ended\n")
        
        cat("url -2 started..")
        l2 <- list()
        for(i in id_vect){
                x <- url2 %>%  modify_url(query = list(pReceiptMainID=i)) %>% read_lines %>% fromJSON()
                l2 <- c(l2,list(x=x))
        }
        cat("ended\n")
        
        dt2 <- data.table(form2=l2)
        
        cat("url -3 started..")
        l3 <- list()        
        for(i in id_vect){
                x <- url3 %>%  modify_url(query = list(pReceiptMainID=i))  %>% read_lines %>% fromJSON()
                l3 <- c(l3,list(x=x))
        }
        cat("ended\n")
        
        dt3 <- data.table(form3=l3)
        dt <- cbind(dt1,dt2,dt3)
        if(nrow(dt)>0) message("..SUCCESS")
        outputfile=paste(prfx,"receipt2.RDS",sep = "_")
        cat("..saving output to ",outputfile)
        saveRDS(dt,outputfile)
}