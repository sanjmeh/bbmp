# scraping other sites
library(rvest)
library(xml2)
library(XML)
library(tidyverse)
library(wrapr)
library(data.table)
library(stringr)
library(googlesheets4)
library(jsonlite)
library(httr)
library(urltools)

url1 <- "https://api.blcares.in/boothsquery?chunk=0&limit=25&ward=-1&search="

url_corp_details <- "http://bbmp.gov.in/en/wardwisecouncliesdetails;jsessionid=EE17715975CF9DD34CA92A60907EA040?p_p_id=councillors_WAR_councillorsportlet&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_count=2&_councillors_WAR_councillorsportlet_keywords=&_councillors_WAR_councillorsportlet_advancedSearch=false&_councillors_WAR_councillorsportlet_andOperator=true&_councillors_WAR_councillorsportlet_resetCur=false&_councillors_WAR_councillorsportlet_delta=198"


scrape_corp_site <- function(url= url_corp_details){
        h1 <- read_html(url)
        html_nodes(h1,css=".aui-column-content-last") %>% html_text()
}

proc_corp_details <- function(str){
        
        
}

unpack_corp_string <- function(x){
        ward <- x[2] %>% str_extract("\\d+") %>% as.integer()
        sqkm <- x[4] %>% str_extract("[\\d.]+") %>% as.numeric
        ac <-  str_remove(x[5],"Name") %>% str_trim
        name <- str_remove(x[6],"Address") %>% str_trim
        addr <- str_remove(x[7],"Phone Number") %>% str_trim
        phn <-  str_trim(x[8])
        data.table(ward=ward,sqkm=sqkm,ac=ac,name=name,addr=addr,phn=phn)
}