# bbmp reports


mean2 <- function(x) mean(x,na.rm = T) %>% as.numeric %>% round_half_up
median2 <- function(x) median(x,na.rm = T) %>% as.numeric %>% round_half_up()


# Citizen Portal ward summary generation
genr_summ_citzport <- function(){
        jobs_citzport[,.(ProjAppr=.N,
                         ProjImpl=sum(ifelse(is.na(WorkOrderDate),0,1)),
                         ApprAmt=sum(BudgetAmount)/1e7,
                         ImplAmt=sum(ifelse(is.na(TotalExpense),0,TotalExpense))/1e7
        ),by=.(FY_Budget,Ward)]
}


# Histograms of ward counts with focus on one ward
plot_hist <- function(citzport_data = citzport_test, wardno=150,keypatt="Foot",yr=16:20,var="bud_pcap",clip=0.99){
        to_crore <- function(amt) (amt/1e7) %>% round_to_fraction(4)
        
        dt_axis <- fread("axis_titles.csv")
        Keywords <- citzport_data$keywords %>% str_split(",") %>% unlist %>% str_trim %>% unique %>% str_subset(keypatt) %>% paste(collapse =",")
        Years <- yr %>% paste(collapse =",")
        
        dt <- ward_ranks(dt = citzport_data, keypat = keypatt,yr = yr,wmaster = ward_master)
        dt <- dt[get(var)<=clip*max(get(var))]
        wname <- dt[ward==wardno,ward_name]
        box_text <-  c(wname,Keywords,dt_axis[colname==var,axisname]) %>% paste(collapse ="\n")
        ylim <- hist(dt[[var]],plot=F,breaks = 30)$counts %>% max # cool way to find the scales of a histogram for label positioning
        xlim <- hist(dt[[var]],plot=F,breaks = 30)$mids %>% max
        meanval <-  mean(dt[[var]])
        wardval <- dt[ward==wardno,get(var)]
        ratio <- wardval/meanval
        statement <- paste("Ward",wname,"is",percent(ratio),"of the mean across all wards")
        
        gplot <- dt %>% 
                ggplot(aes(get(var))) + 
                geom_histogram() + 
                geom_vline(xintercept = meanval,lty=2) + 
                geom_vline(xintercept = wardval,color="hotpink",lty=4,size=1.5) +
                geom_label(x=wardval,y=ylim*0.75,
                           label=wname %>% str_split("\\s+",simplify = T) %>% unlist %>% paste(collapse = "\n")) + 
                geom_label(x=mean(dt[[var]]),y=ylim*0.33,label="Mean \nacross \nall wards") + 
                ggtitle(paste0("Historgram of ward counts for year",(ifelse(length(yr)>1,"s","")),":",Years),
                        subtitle = paste("How",wname,"compares with other wards on budget heads:",Keywords)) + 
                xlab(dt_axis[colname==var,axisname])
        rect <- annotate("rect", xmin = 0.5*xlim,ymin=0.48*ylim, xmax=0.95*xlim,ymax=0.6*ylim,fill="dodgerblue") 
        numb <- annotate("text", label=percent(ratio),x=0.51*xlim,y=0.55*ylim,col="yellow",size=14,hjust ="left")
        sentence <- annotate("text", label="of the mean\nof all wards.",x=0.73*xlim,y=0.55*ylim,col="white",size=6,hjust =0)
        box2 <-  annotate("label",x=0.5*xlim,y=0.9*ylim,label=wname,hjust=0.5,size=13,fill="yellow",color="red")
        caption <- paste("This histogram is plotted across all 198 wards")
        gplot + rect + numb + sentence + box2
}

# Histograms of ward counts with high variance boxed out
# pass the vartype = "perc" for budget utilisation. 
hist_sigma <- function(citzport_data = citzport_test,keypatt="Foot",yr=16:20,var="bud_pcap",vartype="",lsigma=-1,rsigma=2,totbins=30,perlabel="per capita",rsunit="",
                       Boxtitle="DRAINS",xstart=0,ystart=0){
        # change to lakhs or crores, e.g. u="crore"
        chng_unit <- function(value,u) {
                div=case_when(u=="crore" ~ 7,u=="lakh" ~ 5, u=="thousand" ~ 3, T ~ 0)
                out <- (value/(10^div)) %>% round_to_fraction(4)
                if(vartype=="perc") out <- percent(value,digits = 0)
                out
        }
        prfx <- ifelse(rsunit=="","","Rs")
        ushort <- case_when(rsunit=="crore"~ "cr", rsunit=="lakh" ~ "L",rsunit=="thousand" ~ "K", T ~"")
        pwr <- case_when(rsunit=="crore" ~ 7,rsunit=="lakh" ~ 5, rsunit=="thousand" ~ 3, T ~ 0)
        dt_axis <- fread("axis_titles.csv")
        Keywords <- citzport_data$keywords %>% str_split(",") %>% 
                unlist %>% str_trim %>% unique %>% str_subset(keypatt) %>% paste(collapse =",")
        Years <- yr %>% paste(collapse =",")
        
        dt <- ward_ranks(dt = citzport_data, keypat = keypatt,yr = yr,wmaster = ward_master)
        dt2 <- dt[,{
                sdev <- sd(get(var))
                mean_val <- mean(get(var))
                sigma <- (get(var) - mean_val)/sdev
                .SD %>% mutate(sigma=sigma)
        }]
        setDT(dt2)
        
        dt2[,expense_class:=case_when(sigma>rsigma ~ "High",sigma<=lsigma ~"Low",T ~ "Normal" )]
        mean_val <-  mean(dt[[var]],na.rm = T)
        mean_label <- mean_val %>% chng_unit(rsunit) %>% paste(prfx,.,rsunit,perlabel) %>% paste("Mean=",.)
        
        low_box_text <-  dt2[expense_class=="Low",ward_name] %>% paste(collapse ="\n")
        high_box_text <-  dt2[expense_class=="High",ward_name] %>% paste(collapse ="\n")
        
        vect <- dt2[[var]]
        stopifnot(length(vect)>0)
        low_thr <- dt2[expense_class=="Low",max(get(var))]
        low_thr_label  <- low_thr %>% chng_unit(rsunit) %>% paste(prfx,.,rsunit,perlabel)
        high_thr <- dt2[expense_class=="High",min(get(var))] 
        high_thr_label  <-  high_thr %>% chng_unit(rsunit) %>% paste(prfx,.,rsunit,perlabel)
        
        g1 <- 
                dt2  %>% 
                ggplot(aes(get(var))) + 
                geom_histogram(aes(fill=expense_class),col="grey36",bins = totbins) +  
                scale_fill_manual(values = c(Low="coral1",Normal="darkgrey",High="lawngreen")) +
                #scale_x_continuous(labels = unit_format(unit = "cr", scale = 1e-7)) +
                scale_x_continuous(labels = function(x) paste(x/10^pwr,ushort)) +
                xlab(dt_axis[colname==var,axisname] %>% paste(rsunit)) + 
                ylab("Wards") +
                geom_vline(xintercept = mean_val,lty=2)
        
        labdt <- ggplot_build(plot = g1)$data[[1]] %>% as.data.table()
        labdt[,rn:=seq_along(x)]
        labdt2 <- labdt[rn<=totbins,.(x,y,xmin,xmax)]
        labdt2[,wards:=dt2[get(var)<=xmax & get(var)>=xmin,ward_name %>% unique %>% paste(collapse = "\n") %>% str_replace("\r"," ")],by=x]
        labdt2[,status:=case_when(xmax<=low_thr ~ "L",xmin>=high_thr ~ "H", T ~ "N")]
        
        ylim <- labdt$y %>% max # new way to find the scales of a histogram for label positioning
        xlim <- labdt$x %>% max
        
        
        g1  +
                geom_text_repel(data=data.table(x=mean_val,y=max(labdt2$y),label=mean_label),aes(x=x,y=y,label=label),col="blue",size=6,hjust="right") +
                annotate("label",x=xlim,y=ylim,label=Boxtitle,col="blue",size=10,hjust="right",vjust="center") +
                geom_label_repel(data = labdt2[status %in% c("L","H")],mapping = aes(x,y,label=wards),
                                 xlim=c(xstart,NA),ylim =c(0,ystart),vjust="top",alpha=0.8,segment.alpha = 0.8,nudge_y = 2) +
                # annotate("label",x =  -Inf,y=Inf,label=low_box_text,size=4,vjust="top",hjust="left",alpha=0.7,fill="coral1") +
                # annotate("label",x = Inf,y=Inf,label=high_box_text,size=4,vjust="top",hjust="right",alpha=0.7,fill="lawngreen") + 
                labs(title = "Ward Histogram",subtitle = paste("Project Types:",Keywords),
                     caption = paste0("Wards with very low value (Value < ",chng_unit(low_thr,u = rsunit),rsunit, ") and wards with extremely high value (Value > ", chng_unit(high_thr,rsunit), rsunit,") compared to normal are shown in the callouts")) +
                theme(plot.caption = element_text(size = 10,colour = "red",face = "italic"))
        
}

# Still to fill up the annotate layers... thinking
create_infog <- function(citzport_data = citzport_test, wardno=150,keypatt="Foot",yr=16:20,var1="bud_pcap"){
        dt_axis <- fread("axis_titles.csv")
        Keywords <- citzport_data$keywords %>% str_split(",") %>% unlist %>% str_trim %>% unique %>% str_subset(keypatt) %>% paste(collapse =",")
        Years <- yr %>% paste(collapse =",")
        
        dt <- ward_ranks(dt = citzport_data, keypat = keypatt,yr = yr,wmaster = ward_master)
        dt <- dt[get(var)<=clip*max(get(var))]
        wname <- dt[ward==wardno,ward_name]
}

waffle_budgets <- function(wno=150,dt=citzport_test,fy=16:20,row=4){
        wname <- ward_names[ward==wno,ward_name]
        waf <- function(dt,y,r) {
                df <- as.data.frame(dt[fy==y])
                wdata <- c(Spent= df[1,2],Unspent=df[1,3])
                waffle(equal = T,wdata,rows = r,colors = c("#228B22","#B0E0E6"))
        }
        dt <- dt[ward==wno,.(bud=sum(approvedamount)/1e7,exp=sum(totbillamt,na.rm = T)/1e7),.(fy)][,unspent:=bud-exp]
        iron(waf(dt,fy,r = row))
}

scatter1 <- function(dt=citzport_test,yr=16:20,wdet=ward_master,perc=0.2){
        wsum <-  ward_ranks(dt=dt,yr = yr,wmaster = wdet)
        wsum[,ward_display:=ifelse(bud_ut<perc | bud_ut>1.2,ward_name,NA)]
        wsum[,extreme_wards:=ifelse(bud_ut< perc | bud_ut>1.2,"ABN","NORM")]
        wsum %>% 
                ggplot(aes(totapp,totexp)) + 
                geom_point(aes(color=extreme_wards)) + 
                scale_color_manual(values = c(ABN="hotpink",NORM="grey")) +
                geom_abline(slope = 1,lty=2,col="red") +
                ggrepel::geom_text_repel(aes(totapp,totexp,label=ward_display))
        
}


# ward_master DT has can be read with an fread to bbmp_ward_master.csv
ward_ranks <- function(dt=citzport_test,wmaster=ward_master,yr=16:20,keypat="Foot"){
        setnames(wmaster, old ="ward_no",new = "ward",skip_absent = T)
        
        
        if(nchar(keypat)==0 | is.na(keypat)) 
                wsum <- dt[fy %in% yr,.(totapp=sum(approvedamount,na.rm = T),totexp=sum(totbillamt,na.rm = T)),.(ward)]
        else
                wsum <- 
                        dt[fy %in% yr & grepl(keypat,keywords),.(totapp=sum(approvedamount,na.rm = T),totexp=sum(totbillamt,na.rm = T)),.(ward)]
        
        wsum %<>% left_join(wmaster,by="ward") %>% arrange(ward)
        
        setDT(wsum,key="ward")
        
        wsum <- 
                wsum[,.(totapp=sum(totapp) %>% round_to_fraction(denominator = 4),
                        totexp=sum(totexp) %>% round_to_fraction(denominator = 4), 
                        exp_pkm=(sum(totexp)/road_length_kms) %>% round_half_up(),
                        exp_psqkm=(sum(totexp)/area_sq_km) %>% round_half_up(),
                        exp_pcap=(sum(totexp)/population_2011) %>% round_half_up(),
                        bud_pkm=(sum(totapp)/road_length_kms) %>% round_half_up(),
                        bud_psqkm=(sum(totapp)/area_sq_km) %>% round_half_up(),
                        bud_pcap=(sum(totapp)/population_2011) %>% round_half_up(),
                        bud_ut=(sum(totexp)/sum(totapp)) %>% percent()
                ),
                .(ward,ward_name,road_length_kms,area_sq_km,population_2011,lakes_number,parks_number,playgrounds_number,govt_schools_number,bus_stops_number, street_lights_number)]
        
        wrank <- 
                wsum[order(-bud_pkm)][,rank_bud_pkm:=seq_along(ward)
                ][order(-exp_pkm)][,rank_exp_pkm:=seq_along(ward)
                ][order(-road_length_kms)][,rank_road_len:=seq_along(ward)
                ][order(-area_sq_km)][,rank_area:=seq_along(ward)
                ][order(-totapp)][,rank_totapp:=seq_along(ward)
                ][order(-totexp)][,rank_totexp:=seq_along(ward)
                ][order(-bud_ut)][,rank_bud_ut:=seq_along(ward)
                ][order(-bud_pcap)][,rank_bud_per_cap:=seq_along(ward)
                ][order(-exp_pcap)][,rank_exp_per_cap:=seq_along(ward)
                ][order(-bud_psqkm)][,rank_bud_psqkm:=seq_along(ward)]
        wrank
}

# slight modification of prev ranking. This one takes a custom sring to be applied in description over and above the keyword
ward_ranks_item <- function(dt=citzport_test,pat="Street",k="Light",yr=16:20,wnames=wards){
        wsum <- 
                dt[fy %in% yr & grepl(pat,description,ig=T) & grepl(k,keywords,ig=T),.(totapp=sum(approvedamount)/1e7,totexp=sum(totbillamt,na.rm = T)/1e7),.(ward)]
        wsum <- wsum[order(-totapp)][,rank_appr:=seq_along(ward)]
        wsum <- wsum[order(-totexp)][,rank_exp:=seq_along(ward)]
        wsum %>% left_join(wnames,by=c(ward="Ward No")) %>% setcolorder(c("ward","Ward Name"))
}

# summary report on payment days / unpaid bills percent FY wise / budget head category wise
# add more mbudget head cateogy if needed
bill_report <- function(fullState=T){
        bills_master %>% left_join(master_approval[,.(jn,budgethead)]) %>% 
                {.[,.(projects=.N,avgpaydays=median(rtgsdate - sdt,na.rm = T),unpaidProjects=sum(ifelse(is.na(rtgsdate),1,0))),.(budgethead,fy)
                ][,budcat:=case_when(
                        fullState & grepl("14th|State Finance",budgethead,ig=T) ~ "StateFin",
                        !fullState & grepl("14th",budgethead,ig=T) ~ "StateFin",
                        #grepl("\\bCM\\b",budgethead,ig=T) ~ "CMNgrOthana",
                        grepl("\\bEWS\\b|\\bSC\\b|backward",budgethead,ig=T) ~ "EWSAreas",
                        T ~ "Others"
                )
                ][,.(projects=sum(projects),Unpaid=sum(unpaidProjects),Avgpaydays=mean(avgpaydays,na.rm = T)),.(budcat,fy)
                ][,prc_unpaid:=percent(Unpaid/projects)] %>% as_tibble()
                }
}
# check trends of any type of project by modifying the case_when
trend1 <- function(){
        bills_master %>% left_join(master_approval) %>% 
                {.[,budcat:=case_when(
                        grepl("\\bCM\\b",budgethead,ig=T) ~ "CMNgrOthana",
                        grepl("\\bEWS\\b|\\bSC\\b|backward",descr,ig=T) ~ "EWSAreas",
                        T ~ "Others"
                )
                ][,.(projects=.N,Unpaid=sum(ifelse(is.na(rtgsdate),1,0)),budget=round(sum(approvedamount,na.rm = T)/1e7,2),totexp=round(sum(nett,na.rm = T)/1e7,2)),.(budcat,fy)
                ][,prc_unpaid:=percent(Unpaid/projects)]
                }
}

# html report on top 20 wards in terms of approved amounts in FY 2020
top20 <- function(topn=20){
        ward_names <- readxl::read_excel("Corporators_Contacts.xlsx") %>% select(1:2) %>% setDT %>% setnames(qc(ward,ward_name))
        master_approval[ward<200 & fy>13,.(totamt=round(sum(approvedamount)/1e7,1)),.(ward,fy)][order(ward)] %>% dcast(ward~fy,value.var="totamt") %>% .[order(-`20`)] %>% head(topn) %>% left_join(ward_names) %>% setcolorder(c("ward","ward_name")) %>% htmlTable(rnames = F,align="rlrrrrrrr",css.cell="padding-left:0.5em",caption = "All amounts in Rs Crores")
}

hist_proj <- function(dt,years=17:20,var="proj_days",title="Project Days",yaxis="Project Counts"){
        dt1 <- dt[fy %in% years]
        ymax <-  dt1[get(var) %in% 1:1000,get(var)] %>% as.numeric %>% hist(breaks=30*(0:40),xlim = c(0,1000),) %>% .$counts %>% max
        dt1 %>% ggplot + 
                geom_histogram(aes(get(var)),binwidth = 30) + xlim(c(0,1095)) + 
                geom_vline(xintercept = c(365,mean2(dt1[,get(var)]),730),lty=c(2,3,2),size=0.5) + 
                annotate("rect",xmin = 365*(1:3) - 185, ymin = ymax*0.54,xmax=365*(1:3) - 80,ymax=ymax*0.66,fill="dodgerblue1")  + 
                annotate("text",x = 365*(1:3) - 137, y = ymax*0.6,
                         label=c(dt1[,.(sum(get(var)<365,na.rm = T)/(sum(!is.na(get(var)))),
                                        sum(between(get(var),365,730),na.rm = T)/sum(!is.na(get(var))),
                                        sum(between(get(var),730,1090),na.rm = T)/sum(!is.na(get(var))))] %>% 
                                         as.numeric %>% percent()),color="white",size=6) + 
                annotate("text",x = 365*(1:3) - 180,y=ymax,label=paste("Year-",1:3),size=10) + 
                annotate("text",x=dt1[,get(var)]%>% mean2 + 40,y=ymax*0.9,label=paste("Mean:",dt1[,get(var)] %>% mean2,"days"), size=4) +
                xlab(label = "No. of Days") + 
                ylab(label = yaxis) +
                ggtitle(title,paste("Years:",paste(years,collapse = ",")))
        
        
}

# for Sowmya
plot_garbage <- function(wastedt=w1){
        ggplot(wastedt) + 
                geom_path(aes(year,value),size=0.5) + 
                geom_point(aes(year,value,color=variable),size=3) + 
                facet_wrap( ~variable,scales = "free_y",ncol = 1,labeller = labeller(variable=c(
                        "waste" = "Tonnes Per Day Waste",
                        "pop" = "Population in Millions",
                        "pc" = "Waste Per capita")
                )) + xlab("YEAR") + 
                scale_color_discrete(name="Legend",labels=c("Waste","Population","Percapita Waste")) +
                ggtitle("BANGALORE WASTE GROWTH")
}


# ward_names DT must exist as it is joined
prep_plot <- function(dt){
        setDT(dt)
        dt <- ward_names[dt,on="ward"]
        dt[,wrd_fct:=factor(ward,levels = unique(ward),labels = unique(ward_name),ordered = T)]
        dt[,delay:=Dat_paid - Dat_finish]
        dt[,Mdelay:=median(delay,na.rm = T),.(fy,ward)]
        dt[,tot_nspent:=sum(NSpent_num,na.rm = T),.(fy,ward)]
        dt[,clean_amt:=sum(ifelse(is.na(delay),0,NSpent_num)),.(fy,ward)]
        dt[,yr_strt:=year(Dat_start)]
        dt[,yr_finsh:=year(Dat_finish)]
        dt[,yr_paid:=year(Dat_paid)]
        dt[,tot_proj:=.N,.(fy,ward)]
        dt[!is.na(Dat_finish),execyr:=-as.numeric(paste0("20",fy)) + year(Dat_finish)]
        # assuming year of payment is year of execution when Finish date is missing:
        dt[is.na(Dat_finish) & !is.na(Dat_paid),execyr:=-as.numeric(paste0("20",fy)) + year(Dat_paid)] 
        dt[,payyr:=factor(case_when(execyr==0 ~ "0: SAME YEAR",
                                    execyr ==1 ~ "1: NEXT YR", 
                                    execyr==2 ~ "2: NEXT TO NEXT YR",
                                    execyr==3 ~ "3: YEAR 3",
                                    TRUE ~ "NO EXEC DATE"))]
        dt[,flag:=ifelse(is.na(Dat_paid) & !is.na(Dat_finish),"FINSH & NOTPAID",
                         ifelse(is.na(Dat_paid) & is.na(Dat_finish), "NOTFIN & NOTPAID",
                                ifelse(!is.na(Dat_paid) & !is.na(Dat_finish), "FINSH & PAID", "NOTFIN & PAID" )))]
        
}

plot_trend_proj <- function(dt=citzport_test,w=151,keypat="Drain"){
        types <- dt[ward==w & grepl(keypat,keywords,ig=T)] %>% cSplit("keywords",direction = "tall") %>% .[,keywords] %>% unique
        wname <- ward_names[ward==w,ward_name]
        pdata <- dt[ward==w & grepl(keypat,keywords,ig=T),.(appr=sum(approvedamount)/1e7,Expenses=sum(totbillamt,na.rm=T)/1e7),fy][order(fy)]
        pdata[,`Unspent Budget`:=appr-Expenses]
        pdata %>% 
                melt(id.var="fy",variable.name="Amount") %>% 
                filter(Amount!="appr") %>% 
                mutate(Amount=factor(Amount,ordered = T,levels = c("Unspent Budget","Expenses"))) %>% 
                ggplot(aes(fy,value)) + geom_col(aes(fill=Amount),position = "stack") + 
                scale_fill_manual(values = c("Unspent Budget"="grey",Expenses="steelblue2")) + 
                ggtitle(paste(wname," - ","Ward:",w),paste("Expenses on:",paste(types,collapse = ", "))) + xlab("FY") + ylab("Rs Crores")
}
