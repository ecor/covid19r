##
rm(list=ls())

library(dplyr)
library(magrittr)
library(stringplus)
library(ggplot2)
library(stringr)
library(reshape2)


wpath <- '/home/ecor/Dropbox/R-packages/COVID-19/'

#####

out_csv <- wpath+'dati-regioni/dpc-covid19-ita-regioni.csv' 
out <- out_csv %>% read.table(sep=",",header=TRUE,quote=NULL)
out$data <- as.Date(out$data)
out$totale_casi_viventi <- as.numeric(out$totale_casi)-as.numeric(out$deceduti)

##out2 %>% filter(denominazione_regione=="Lombardia")

##out2 <- out %>% select(data,denominazione_regione,totale_ospedalizzati,totale_attualmente_positivi,totale_casi,dimessi_guariti,deceduti,tamponi)
out <- out %>% melt(id=c("data","denominazione_regione"))
names(out)[names(out)=="data"] <- "date"
names(out)[names(out)=="denominazione_regione"] <- "region"
out <- out[!(out$variable %in% c("stato","codice_regione","lat","long")),]
out$value <- as.numeric(out$value)


### http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
levels(out$variable)[levels(out$variable)=="tamponi"] <- "lA0_tamponi"
levels(out$variable)[levels(out$variable)=="totale_casi_viventi"] <- "lA1_positivi"
###levels(out$variable)[levels(out$variable)=="dimessi_guariti_level"] <- "lA2_positivi_dimessi"
levels(out$variable)[levels(out$variable)=="totale_attualmente_positivi"] <- "lB2_attualmente_positivi"
levels(out$variable)[levels(out$variable)=="totale_ospedalizzati"] <- "lB3_attualmente_ospedalizzati"
levels(out$variable)[levels(out$variable)=="terapia_intensiva"] <- "lB4_terapia_intensiva"
levels(out$variable)[levels(out$variable)=="deceduti"] <- "lC5_deceduti"
###

oo <- unique(out$variable) %>% as.character() 
varl <- oo[str_sub(oo,1,1)=="l"] %>% sort()
colrl <- c("#feedde","#fdd0a2","#fdae6b","#fd8d3c","#e6550d","#a63603")
names(colrl) <- varl
###
###
out <- out %>% filter(value>0) %>% filter(variable %in% varl) %>% mutate(variable=as.character(variable))

###
 


region <- "Emilia Romagna"
regions <- "P.A. Trento"
regions <- "P.A. Bolzano"
#regions <- "Friuli Venezia Giulia"
#regions <- "Emilia Romagna"
regions <- "Lombardia"
#regions <- "Liguria"
#regions <- "Toscana"
#regions <- "Lazio"
#regions <- "Piemonte"
#regions <- "Marche"
#regions <- "Campania"
#regions <- "Basilicata"
##stop("HERE")
#out <- out %>% filter(region==regions)
##stop("HERE")
###
regioni_ <- out$region %>% unique()  
nord <- c("Lombardia","Veneto","Emilia Romagna","Piemonte","P.A. Trento","P.A. Bolzano","Friuli Venezia Giulia","Liguria","Valle d'Aosta")
centro <- c("Toscana","Lazio","Umbria","Marche","Abruzzo","Molise")
sud <- as.character(regioni_)[!(as.character(regioni_) %in% c(nord,centro))] 
regions <- sud
regions <- "Lombardia"
##regions <- nord
out <- out %>% filter(region %in% regions) %>% group_by(date,variable) %>% summarize(value=sum(value)) %>% as.data.frame()


out <- out[order(as.character(out$variable)),]
yrange <- range(out$value)
ymax <- yrange[2]
ymin <- yrange[1]
ymean <- mean(yrange)
ydelta <- diff(yrange)/2
maxperc <- "auto"
lagday <- 5
lockdown <- as.Date("2020-03-09")
#### Plotting 

out <- out %>% dcast(date ~ variable)


out[is.na(out)] <- 0
for (i in 2:(ncol(out)-2)) {
	
	out[,i] <- out[,i]-out[,i+1]
	
}
####
ratio <- out

for (i in 2:ncol(out)) {
	
	ratio[-1,i] <- diff(out[,i])/out[-ncol(out),i]*100
	ratio[1,i] <- NA
}


#####




out <- out %>% melt(id="date")
ratio <- ratio %>% melt(id="date")
names(ratio)[names(ratio)=="value"] <- "variation_perc"
##ratio <- ratio %>% filter(date %in% (max(out$date)-1:lagday+1))
ratio <- ratio %>% filter(date %in% (lockdown+2):max(out$date))
if (maxperc=="auto") maxperc <- range(ratio$variation_perc,na.rm=TRUE) %>% abs() %>% max()
if (maxperc>100) maxperc <- 100

out <- full_join(out,ratio)


out$plotted_variation_perc <- (out$variation_perc)*ydelta/maxperc+ymean
variation_perc_breaks <- out$variation_perc[which(out$date==max(out$date))] %>% c(as.integer(c(-1,-0.5,0,0.5,1)*maxperc)) %>% sort()
breaks_y <- out$value[which(out$date==max(out$date))] %>% rev()  %>% cumsum() %>% sort()

breaks_x <- out$date %>% unique() %>% sort()
ii_labels_x <- breaks_x %>% extract(as.numeric(.-.[1])%%4==0)
labels_x <- breaks_x %>% as.character()
labels_x[!(breaks_x %in% ii_labels_x)] <- ""
gg <- NULL
gg <- ggplot()+geom_area(aes(x=date,y=value,group=variable,fill=variable,col=variable),data=out,position="stack",alpha=0.5)
gg <- gg+theme_bw()+scale_colour_manual(values = colrl)+scale_fill_manual(values = colrl)###scale_alpha_discrete(range = (c(0.1, 1))) ###+scale_fill_brewer(palette="Dark2")+scale_color_brewer(palette="Dark2")  
gg <- gg+geom_vline(aes(xintercept=lockdown),linetype=2,color="red")
gg <- gg+geom_line(aes(x=date,y=plotted_variation_perc,group=variable,linetype=variable),data=out)
gg <- gg+geom_point(aes(x=date,y=plotted_variation_perc,group=variable,pch=variable),data=out)
gg <- gg+geom_hline(yintercept=ymean,linetype=2)##

##gg <- gg+
##gg <- gg+scale_y_log10()


gg <- gg+scale_y_continuous(breaks=breaks_y,sec.axis = sec_axis(~ . *maxperc/ydelta-maxperc/ydelta*ymean,breaks=variation_perc_breaks,name="Aumenti Giornalieri [%]"))
gg <- gg+scale_x_date(breaks=breaks_x,labels=labels_x)
###gg <- gg+ylim(ymin,ymax)
gg <- gg+xlab("Tempo")+ylab("Persone")+ggtitle(sprintf("Casi in %s",paste(regions,collapse=",")))
#####gg <- gg+scale_y_continuous("Incremento",sec.axis = sec_axis(~ ./ydelta*maxperc))