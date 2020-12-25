#pdf table extraction for Vidya's son Yash
library(data.table)
library(janitor)
library(tidyverse)
library(tabulizer)

colnames <- c("RoundNo", "InstituteName", "BranchName", "AllottedQuota", 
              "Category", "SeatPool", "OpenningRank", "ClosingRank")

# load the data (a list of matrix of 296 length that was already extracted using tabulizer::extract_tables()
viddt <- readRDS("FinalRoundJoshaaList.RDS")

proc_page <- function(pdt){
        setDT(pdt)
        if(pdt[1,1]=="Round\rNo") normal <- TRUE else normal <- FALSE
        if(normal==T) return(row_to_names(pdt,1)) else
        {
                pdt[,V6:=NULL]
                return(pdt[-(1:2),c(1:8)] %>% setnames(colnames))
        }
        
}

# uncomment the lines below to test all 296 pages
vidmaster <-  viddt %>% map(~as.data.frame(.x) %>% proc_page) %>% rbindlist(fill = T)
