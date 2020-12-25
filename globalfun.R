# ====== HELPER FUNCTIONS ==========
library(data.table)
library(stringr)
library(stringdist)

# input an address vector to get a address vector with + signs in between
prep_addr_loc <- function(addstr,locality="Peenya"){
        str2 <- addstr %>% str_replace_all("[#,&]"," ") %>% 
                str_squish %>% str_replace_all("\\s+","+") %>% 
                {ifelse(grepl("locality", .,ig=T), .,paste(.,locality,sep="+"))} %>% 
                {ifelse(grepl("Bangalore|Bengaluru", .,ig=T), paste(.,"Karnataka",sep="+"),paste(.,"Bangalore+Karnataka",sep="+"))}
        str2 %>% strsplit("\\s+") %>% map(paste,collapse="+") %>% unlist
}

# input an address vector to get a address vector with + signs in between
prep_addr <- function(addstr){
        str2 <- addstr %>% str_replace_all("[#,&]"," ") %>% 
                str_trim %>% str_replace_all("\\s+","+") %>% 
                {ifelse(grepl("Bangalore|Bengaluru", .,ig=T), paste(.,"Karnataka",sep="+"),paste(.,"Bangalore+Karnataka",sep="+"))}
        str2 %>% strsplit("\\s+") %>% map(paste,collapse="+") %>% unlist
}

# most frequent string actions coded in one function
str_action <- function(x,what="punct"){
        case_when(
                grepl("punct",what,ig=T) ~ str_remove_all(x,"[\\s[:punct:]]") %>% tolower(),
                grepl("dou|dbl",what,ig=T) ~ str_replace_all(x, regex("([a-z])\\1{1,}",ig=T),regex("\\1",ig=T)),
                grepl("vow",what,ig=T) ~ str_remove_all(x,regex("[aeiou]",ig=T)),
                grepl("near|next",what,ig=T) ~ str_replace(x,regex("(near|opp[.]*|behind|opp to|next to) [a-z]{1,20}",ig=T),"BLACKLIST"),
                grepl("suppr",what,ig=T) ~ str_remove_all(x,regex("cross|main|road|street|opp|next|right|left",ig=T)),
                grepl("dig",what,ig=T) ~ str_remove_all(x,"[\\d[:punct:]]"),
                TRUE ~ x
        )
}

# add a cluster code column in a DT. Clustering will be on a `var` column that can be passed and he height of the dendograph `ht` can be specified to cluster them
clust_addr <- function(dt,var="Description",ht=2){
        w1 <- dt[,get(var)]  %>% unique %>% na.omit %>% as.character()
        w2 <- w1 %>% 
                str_action("punct") %>% 
                str_action("doub") %>% 
                str_action("vow")
        
        clustdt <- data.table(string=w1,clust= stringdistmatrix(w2,w2) %>% 
                                      as.dist %>% hclust %>% 
                                      cutree(h = ht))
        clustdt[dt,on=c(string=var)]
}
