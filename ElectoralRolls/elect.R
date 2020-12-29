# electoral role
library(rvest)
library(httr)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(htmlTable)
library(wrapr)
library(data.table)
library(magrittr)
library(stringr)
library(stringdist)
library(pdftools)
library(splitstackshape)
library(tabulizer)
library(formattable)
library(readxl)
library(janitor)
library(textclean)
library(ggmap)
library(tesseract)
library(magick)
load("urls.RData")
source("~/R/bnp/globalfun.R")


longnames <- function(x){
        c(ward = "Ward", areacode  = "Area Code", Part = "Booth No.", 
            poll_center = "Booth Name", room = "Room",  booth_address= "Booth Address", 
          communities = "Communities", voters = "Voters", males = "Males", females = "Females", 
          tot_votes = "Votes Polled", BJP = "BJP", INC = "INC", JDS = "JDS", 
           winner = "Winner",  winner_margin = "Margin", median_age = "Median Age", 
           age_18_25 = "18-25", age_26_40 = "26-40",  age_41_60 = "41-60", 
          age_gt_60 = "60+",
          booths = "Booths Aggregated Under Area" )[x]
}

shortnames <- function(x){
        c(Ward = "ward", `Area Code` = "areacode", `Booths Aggregated Under Area` = "booths", 
          `Booth Name` = "poll_center", Communities = "communities", Voters = "voters", 
          Males = "males", Females = "females", `Votes Polled` = "tot_votes", 
          BJP = "BJP", INC = "INC", JDS = "JDS", Winner = "winner", Margin = "winner_margin", 
          `Median Age` = "median_age", `18-25` = "age_18_25", `26-40` = "age_26_40", 
          `41-60` = "age_41_60", `60+` = "age_gt_60",
          `Booth No.` = "Part")[x]
}


colord1 <- c("Ward", "Area Code", "Booth No.", "Booth Name", "Room", "Booth Address", 
             "Communities", "Voters", "Males", "Females", "Votes Polled", 
             "BJP", "INC", "JDS", "Winner", "Margin", "Median Age", "18-25", 
             "26-40", "41-60", "60+")

colord2 <- c("Ward", "Area Code", "Booths Aggregated Under Area", "Booth Name", 
             "Communities", "Voters", "Males", "Females", "Votes Polled", 
             "BJP", "INC", "JDS", "Winner", "Margin", "Median Age", "18-25", 
             "26-40", "41-60", "60+")

subdir <- "voterREC"
dir <- "ElectoralRolls"

# ==== Load the files into variables if a gap of 1 day ========

if(difftime(Sys.time(),fread("LastRun.txt")$time,units = 'days') > 1){
        suppressWarnings(booths_txt <- fread("booths.txt") %>% clean_names())
        booths2 <- booths_txt # reading even ward 999
        #booths2 <- booths_txt[ward!=999]
        x1 <- fread("kannada_english.txt") %>% setnames(c("part","booth_address"))
        booths2 <- x1[booths2,on="part"][,booth_address:=ifelse(is.na(booth_address),i.booth_address,booth_address)][,i.booth_address:=NULL]
        booths2[,part:=as.character(part)]
        datafiles <- list.files(subdir,full.names = T)
        sections <- read_excel("Sections.xlsx",col_types = c("text","numeric","numeric","text")) %>% clean_names() %>% setDT
        parties <- read_excel("LA2018_F20_3Parties.xlsx") %>% setDT
        Sys.time() %>% data.table(time=.) %>% fwrite(file = "LastRun.txt") # write the time of reading
        message("Read booths, sections and parties files again...")
}




#====Main functions =====

# strip the leading number from booth_address
striplnumb <- function(addr){
        addr %>% 
                str_remove("^\\d+(?!(\\d+)?\\s*(th|rd))[\\-\\s]*")
}

booth_ovlp <- function(part1,part2,thr=0.3){
        common_words <- qc(cross,main,road,bangalore,bengaluru,opp, behind)
        dt1 <- 
                sections[part ==part1,sec_address] %>% 
                str_action("punct") %>% 
                str_action("suppr") %>% 
                str_action("doub") %>% 
                str_action("vow") %>% 
                expand_grid(
                        sections[part == part2,sec_address]  %>% 
                                str_action("punct") %>% 
                                str_action("suppr") %>% 
                                str_action("doub") %>% 
                                str_action("vow")
                ) %>% 
                setDT %>% setnames(c("a","b"))
        
        dt1[,dist:=stringdist(a,b)]
        dt1[,len:=min(nchar(a),nchar(b)),by=list(rownames(dt1))]
        dt1[,match:=(len-dist)/len]
        formattable::percent(dt1[match>thr,.N]/nrow(dt1))
}

booth_pairs <- function(booths){
        bpairs <- booths %>% combn(m = 2) %>% t %>% as.data.table()
        bpairs[,overlap:=booth_ovlp(V1,V2),by=list(rownames(bpairs))]
}

roomfun <- function(addr){
        room_str <-
                addr %>% # https://regex101.com/r/UQ20CW/2
                str_extract(regex("(ro*m[[:punct:]\\s]*.{0,3}(no[[:punct:]\\s]*)?\\d{1,2},?|r[[:punct:]\\s]?\\s*No[[:punct:]\\s]*\\s*\\d,?)",ig=T))
        
        schooladdr <- 
                addr %>% 
                str_remove(fixed(room_str)) %>% str_squish() %>%  # twin vector in action
                str_remove("[,;-]$|^[:punct:]") %>% # remove leading and trailing commas
                str_remove_all("(?<=,)\\s?,") %>% # remove  even consecutive commas with space or no space
                str_remove_all("\\s(?=,)") %>% # finally close the gap of a comma with its preceding text
                str_squish
        
        polladdr <- ifelse(is.na(schooladdr),addr,schooladdr)
        
        
        room_no <- str_extract(room_str,"\\d+") %>% as.integer()
        return(data.table(poll_center=polladdr,room=room_no))
}

# merge the two DTs - booths and sections and collapse on all section address assuming they are communities we want
# also create poll_centers that are unique booth addresses after shaving off the room numbers.
expand_booth <- function(bdata=booths,sdata=sections){
        sdata[,sec_address:=ifelse(is.na(sec_address),"",sec_address)]
        
        exp <- sdata[bdata,on="part",nomatch=NA
        ][,.(communities=paste(sec_address,collapse = " + ")),by=.(ward=i.ward,part,booth_address)][
                ,communities:=str_trim(communities)  %>%  str_remove("\\+$") %>% str_trim]
        
        # could do this in one regex but below is more readable
        exp <- cbind(exp,roomfun(exp$booth_address))
        exp[, poll_center := striplnumb(poll_center)]
        exp[,part:=as.character(part)]
        booth_clust <- unique(exp,by=c("ward","poll_center"))
        booth_clust2 <- booth_clust$ward %>% unique %>% map(~clust_addr(ht=6,dt=booth_clust[ward==.x],var = "poll_center")) %>% rbindlist
        return(booth_clust2)
        exp <- booth_nvow[exp,on="addr_nvow"][order(poll_center,room)][,addr_nvow:=NULL][,i.poll_center:=NULL]
        parties[exp,on=.(Part=part)]
} 
# Note: 1510185 and 1600046 part nos. did not have corresponding sections.



# assuming the second component of file name is part code of booth
# filecomp <- datafiles %>% str_extract("(?<=\\/).+(?=\\.)") %>% str_split("_",sim=T)
# names(datafiles) <- filecomp[,2] 

# pass a named file vector
read_people_files <- function(filevector){
        pdata <- filevector %>% imap(~read_excel(.x)) %>% rbindlist(idcol = "filen")
        pdata[,filen:=as.numeric(filen)]
}

# input 2 DTs (booth level and people level)
aggr_boothlev <- function(data1=booths,data2=voterdata){
        data3 <- data1[data2,on=.(Part),nomatch=NA
        ][,.(voters=.N, 
             median_age=median(Age),
             males=sum(Sex=="M"),
             females=sum(Sex=="F"),
             age_18_25=sum(Age>=18 & Age<=25),
             age_26_40 = sum(Age>25 & Age<=40),
             age_41_60 = sum(Age>40 & Age<=60),
             age_gt_60 = sum(Age>60)
        ),
        by=.(ward,booth_address,Part,communities,poll_center,room,BJP,INC,JDS)][order(ward,booth_address)]
        
        data3[,.(ward,poll_center)] %>% unique -> areacodes
        areacodes[,areacode:=paste0(sprintf("%03d",ward),seq_along(poll_center) %>% sprintf("%02d",.)),by=ward]
        data4 <- areacodes[data3,on=.(poll_center,ward)][order(poll_center,room)]
        data4[,cumcount:=cumsum(voters),by=.(poll_center,ward)
        ][,areacount:=trunc(cumcount/5000),by=poll_center
        ][,areacode:=paste0(areacode,areacount %>% sprintf("%02d",.))
         ][,cumcount:=NULL
         ][,areacount:=NULL]
        # 
        data4[,winner:=ifelse(BJP>INC & BJP > JDS,"BJP",
                              ifelse(INC> BJP & INC > JDS, "INC", "JDS"))
        ][,winner_margin:=sort(c(BJP,INC,JDS))[3] - sort(c(BJP,INC,JDS))[2],
          by=rownames(data4)
        ][,tot_votes:=BJP+INC+JDS,by=rownames(data4)
        ][order(ward,Part)]
        return(data4)        
}


aggr_arealev <- function(vmaster=voter_master2){
        x1 <- vmaster[,.(communities= unique(communities) %>% paste(collapse = " ++ "), 
                   booths=paste(Part,collapse = "+"),
                   voters=sum(voters),
                   Age=mean(median_age) %>% round(0),
                   males=sum(males),females=sum(females),
                   age_18_25=sum(age_18_25),
                   age_26_40=sum(age_26_40),
                   age_41_60=sum(age_41_60),
                   age_gt_60=sum(age_gt_60),
                   BJP = sum(BJP,na.rm = T),
                   INC = sum(INC,na.rm = T),
                   JDS = sum(JDS,na.rm = T)
                   ),by=.(ward,areacode,poll_center)
                ]
        x1[,winner:=ifelse(BJP>INC & BJP > JDS,"BJP",
                           ifelse(INC> BJP & INC > JDS, "INC", "JDS"))
        ][,winner_margin:=sort(c(BJP,INC,JDS))[3] - sort(c(BJP,INC,JDS))[2],
          by=rownames(x1)
        ][,tot_votes:=BJP+INC+JDS,by=rownames(x1)
          ][order(ward,areacode)]
}

# use the aggregated tables - one of the above - as input,  but with shorter names, to keep life easy
aggr_wardlev <- function(vmaster=voter_master2){
        x1 <- vmaster[,.( 
                   voters=sum(voters),
                   median_age=mean(median_age) %>% round(0),
                   males=sum(males),female=sum(females),
                   age_18_25=sum(age_18_25),
                   age_26_40=sum(age_26_40),
                   age_41_60=sum(age_41_60),
                   age_gt_60=sum(age_gt_60),
                   BJP = sum(BJP,na.rm = T),
                   INC = sum(INC,na.rm = T),
                   JDS = sum(JDS,na.rm = T)
                   ),by=.(ward)]
        x1[,winner:=ifelse(BJP>INC & BJP > JDS,"BJP",
                           ifelse(INC> BJP & INC > JDS, "INC", "JDS"))
        ][,winner_margin:=sort(c(BJP,INC,JDS))[3] - sort(c(BJP,INC,JDS))[2],
          by=rownames(x1)
        ][,tot_votes:=BJP+INC+JDS,by=rownames(x1)
        ][,winnervotes:=max(BJP,INC,JDS),by=rownames(x1)
        ][,winnerperc:= formattable::percent(winnervotes/tot_votes)
        ][,marginperc:= formattable::percent(winner_margin/tot_votes)
        ][order(ward)]
        
}

read_ward_details <- function(dt=NULL,fwards="../BBMPR_ward_master_BBMP Restructuring 03-08-2015.pdf"){
        if(is.null(dt)) x1 <- tabulizer::extract_tables(fwards) else x1 <- dt
                x1 %>% 
                        map(~.x %>% as.data.table) %>% 
                        rbindlist(fill = T) %>% row_to_names(1) %>% 
                        clean_names() %>% 
                        .[ward_no!=""] %>% 
                        .[-nrow(.)] %>% 
                        map_at(.at = c(1,6:9),as.numeric) %>% 
                        as.data.table()
}

read_ward_statistics <- function(fstatistics="Wards_Statistics.xlsx"){
        x1 <- read_excel(fstatistics) %>% setDT %>% clean_names() %>% .[-199]
        x1[,ward := as.integer(ward)]
        x1
}



plot_dendo <- function(wno,dt=lev_area,textsize=0.75){
        dt[ward %in% wno,poll_center] %>% unique -> w1
       clust= stringdistmatrix(w1,w1) %>% 
                           as.dist %>% 
                        hclust() %>% 
                           plot(labels=w1,cex=textsize)
}

wardtbl <- function(html=T,dt1 = ward_statistics,dt2=lev_ward,wards=1:198){
        dt1[dt2,on="ward"
        ][ward %in% wards,.(ward,ward_name,
             BBMPvoters=voters,LAvoters=i.voters,
             BBMPvote_perc=percent(voting_percent) %>% as.character(), LAvote_perc = percent(tot_votes/i.voters) %>% as.character,
             BBMPwinner=party_18, LAwinner=i.winner,
             BBMPwinner_votes=percent(votes_19/total_votes) %>% as.character, LAwinner_votes=as.character(winnerperc),
             BBMPvotes=total_votes,LAvotes=tot_votes
        )] %>% {if(html==T)
                htmlTable(.,css.cell="padding-right:0.5em",
                          cgroup = c("Ward","Voters","Percent Voting","Winner","Winner Votes","Total Votes"),
                          n.cgroup = c(2,2,2,2,2,2),
                          header = c("Number","Name",rep(c("BBMP","LA"),5)),
                          rnames=F
                ) else . }
}

slow_upload <- function(){
        for(w in 183:199){
                if(w%%8 ==0) Sys.sleep(110)
                fname<- paste(w,ward_names$ward_name[w],sep = "_")
                gs4_create(name = fname ,sheets = list("Booth Level" = list1[[w]],"Area Level" = list2[[w]]))
                
        }

}

get_voterlist <- function(w=112,v=voters_enr){
        v[ward==w,.(ward,ward_name,part,EPIC,Status,Serial,House,Name,Sex,Age,Relative,Reln,booth_address)]
}

# add ward details, booth details to every voter
enrich_voters <- function(v=voters,b=booths_txt){
        v[,Part:=as.numeric(Part)]
         wstat <- read_ward_statistics()
        b[wstat,on="ward"][v,on=.(part=Part)]
}

clean_blo_names <- function(x){
        nslno=x[[1]] %>% str_which("Sl\\s*no") %>% first %>% as.integer()
        if(length(nslno)>0)
           row_to_names(x,row_number = nslno) %>% clean_names()
        else
        row_to_names(x,row_number = 1) %>% clean_names()
}

revise_blo_names <- function(x){
        cmpr <- names(x) %>% tolower %>% str_squish %>% str_remove_all("[_\\s*-]")
        new_names <- case_when(
                grepl('slno',cmpr) ~ "slno",
                grepl('dis',cmpr) ~ "district",
                grepl('acn[uo]',cmpr) ~ "ac_number",
                grepl('acnam',cmpr) ~ "ac_name",
                grepl('pollingsta.*no|psno',cmpr) ~ "psno",
                grepl('psloc.*no',cmpr) ~ "ps_location",
                grepl('psname|polling.*name',cmpr) ~ "ps_name",
                grepl('psloc.*name',cmpr) ~ "ps_location_name",
                grepl('nameoftheblo|bloname',cmpr) ~ "blo_name",
                grepl('catego',cmpr) ~ "category",
                grepl('contactadd.*blo|add.*blo',cmpr) ~ "address_blo",
                grepl('contactadd.*sup',cmpr) ~ "address_suprv",
                grepl('mobile',cmpr) ~ "mobile_blo",
                grepl('name.*superv',cmpr) ~ "supervisor_name",
                grepl('mobile.*superv',cmpr) ~ "supervisor_contact",
                grepl('nodal',cmpr) ~ "nodal_off",
                grepl('desi?g',cmpr) ~ "design",
                grepl('bank.*name',cmpr) ~ "bank",
                grepl('bank.*acc',cmpr) ~ "bank_account",
                grepl('ifsc',cmpr) ~ "ifsc",
                T ~ "Undetected"
        )
        names(x) <- new_names
        clean_names(x)
        x
}

# Scraping Election Booths, Wards from Constituencies from bbmpknowyourbooth.in website =======

# Scrape booth names for a specific ward; note: it also needs AC number.
scrape_booths <- function(baseurl=u3_bbmpknowyourbooth.in,ward=1,ac=150){
        baseurl %>% 
                httr::modify_url(query = list(ward_no=ward,assembly_constituency_no=ac)) %>% 
                read_html %>% 
                html_table %>% 
                as.data.table()
}

#observer.pdf <- tabulizer::extract_tables("https://www.ceokarnataka.kar.nic.in/Pdfs_NewWebsite/Poll_Related_Officers/Observer.pdf")
# pass the tabulizer extracted list `observer.pdf` (uncomment the line at the top and dowwnload the pdf again if needed )
extr_assembly_const <- function(lpdf=observer.pdf){
        lpdf %>% map(as.data.table) %>% 
                rbindlist %>% 
                 select(1,2) %>% 
                setnames(qc(distt,ac)) %>% 
                filter(grepl("BANG.+URBAN|B.B.M.P",distt)) %>% 
                mutate(acno=str_extract(ac,"\\d{3}")) %>% 
                filter(!is.na(acno)) %>% 
                mutate(acname=str_remove(ac,acno) %>% str_trim %>% str_remove("-"))
}

scrape_wards_of_ac <- function(baseurl=u2_bbmpknowyourbooth.in,ac=150){
        
        baseurl %>% 
                httr::modify_url(query = list(assembly_constituency_no=ac)) %>% 
                read_html %>% 
                html_table %>% 
                as.data.table()
}

scrape_acs_of_distts <- function(baseurl=u1_bbmpknowyourbooth.in,district=1){
        
        baseurl %>% 
                httr::modify_url(query = list(district_no=district)) %>% 
                read_html %>% 
                html_table %>% 
                as.data.table()
}
