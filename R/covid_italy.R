NULL
#' Coovid19 Epidemic in a region or a group of regions of Italy
#' 
#' @param file file (CSV format) Genetsally available on \url{https://github.com/pcm-dpc/COVID-19/blob/master/dati-regioni/dpc-covid19-ita-regioni.csv}
#' @param regione name of the Italian Region , e.g. \code{"Lombardia"}. Please see examples for correct Italian Region names.
#' @param start_date,end_date start and dates (optinal) for x axis  
#' @param layer layer that can be plotted. See defauls values.
#' @param percscale scale of percenteage axis (secondary axis). Numerical value between 0 and 1. 
#' @param maxperc perecentage that is plotted for each trend. 
#' @param pop data frame containg population data for each ragion. See default. Source: ISTAT.  
#' @param prevalence logical valus. If code{FALSE} (default) output values refer to the number of cases / people, if it is \code{TRUE} output values refer to prevalence (cases per 1000 inhabitants).
#' 
#' @export
#' 
#' @importFrom stringr str_sub
#' @importFrom dplyr filter
#' @importFrom utils read.table
#' @importFrom magrittr %>% extract
#' @importFrom ggplot2 aes geom_hline geom_point geom_vline ggplot ggtitle
#' @importFrom ggplot2 xlab ylab geom_line sec_axis theme_bw geom_area
#' @importFrom ggplot2 scale_colour_manual scale_fill_manual scale_x_date scale_y_continuous
#' @importFrom reshape2 dcast melt
#' @importFrom dplyr mutate select group_by full_join summarize 
#' 
#' @examples
#' 
#' library(stringplus)
#' wpath <- '/home/ecor/Dropbox/R-packages/COVID-19/'
#' 
#' 
#' 
#' out_csv <- wpath+'dati-regioni/dpc-covid19-ita-regioni.csv' 
#' 
#' out <- covid19Italy(file=out_csv,regione="Lombardia")
#' out <- covid19Italy(file=out_csv,regione="P.A. Trento")
#' nord <- c("Lombardia","Veneto","Emilia Romagna",
#' "Piemonte","P.A. Trento","P.A. Bolzano","Friuli Venezia Giulia",
#' "Liguria","Valle d'Aosta")
#' centro <- c("Toscana","Lazio","Umbria","Marche","Abruzzo","Molise")
#' sud <- c("Puglia","Campania","Calabria","Sicilia","Basilicata","Sardegna")
#' 
#' out <- covid19Italy(file=out_csv,regione=nord) 
#' out <- covid19Italy(file=out_csv,regione=centro)
#' out <- covid19Italy(file=out_csv,regione=sud)
#' 
#' out <- covid19Italy(file=out_csv,regione="tutte")
#' out <- covid19Italy(file=out_csv,regione="all")
#' 
#' ## To see the plot: 
#' gg <- attr(out,"ggplot") 
#' print(gg)
#' 
#' ## To select a specific time period:
#' out <- covid19Italy(file=out_csv,regione="Veneto",
#' start_date=as.Date("2020-03-06"),end_date=as.Date("2020-03-12"))
#' 
#' ## To remove the non-positive tessted cases ("lA0_tamponi")
#' layer=c("lA0_tamponi","lA1_positivi","lA2_positivi_dimessi","lB2_attualmente_positivi","lB3_attualmente_ospedalizzati","lB4_terapia_intensiva","lC5_deceduti")
#' out <- covid19Italy(file=out_csv,regione="Lombardia",layer=layer[-1])
#' 
#' ## prevalence (epidemiology)
#' 
#'  pop <- read.table(system.file('demography/italy/data/popolazione.csv',package="covid19r"),sep=',',header=TRUE)
#' 	out <- covid19Italy(file=out_csv,regione=nord,prevalence=TRUE)
#' 
#' 
covid19Italy <- function(file,regione="Lombardia",start_date=NA,end_date=NA,
layer=c("lA0_tamponi","lA1_positivi","lA2_positivi_dimessi","lB2_attualmente_positivi","lB3_attualmente_ospedalizzati","lB4_terapia_intensiva","lC5_deceduti"),
percscale=0.25,maxperc="auto",pop=read.table(system.file('demography/italy/data/popolazione.csv',package="covid19r"),sep=',',header=TRUE,quote=""),prevalence=FALSE) {
	
	##
	value <- NULL
	variable <- NULL
	plotted_variation_perc <- NULL
	region <- NULL
	##
	layer_f=c("lA0_tamponi","lA1_positivi","lA2_positivi_dimessi","lB2_attualmente_positivi","lB3_attualmente_ospedalizzati","lB4_terapia_intensiva","lC5_deceduti")
	mlayer <- which(layer_f==sort(layer)[1])
	layer <- layer_f[mlayer:length(layer_f)]
	##
	out <- file %>% read.table(sep=",",header=TRUE,quote=NULL)
	out$data <- as.Date(out$data)
	out$totale_casi_viventi <- as.numeric(out$totale_casi)-as.numeric(out$deceduti)
	out$totale_attualmente_positivi <- out$totale_ospedalizzati+out$isolamento_domiciliare
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
	
	out <- out %>% filter(value>0) %>% filter(variable %in% varl) 
	out <- out %>% filter(variable %in% layer)
	out <- out %>% mutate(variable=as.character(variable))
	## Filtering 
	
	if (!is.na(start_date)) out <- out %>% filter(date>=start_date)
	if (!is.na(end_date)) out <- out %>% filter(date<=end_date)
	
	if (tolower(regione[1]) %in% c("all","tutte")) regione <- out$region %>% unique() %>% as.character()
	
	
	out <- out %>% filter(region %in% regione) %>% group_by(date,variable) %>% summarize(value=sum(value)) %>% as.data.frame()
	
	####
	####
	if (prevalence==TRUE) {
	  
	  pops <- pop  %>% filter(N_Regione %in% regione) %>% select(Popolazione) %>% sum()
	  out$value <- out$value/pops*1000
	}
	
	
	####
	####
	
	out <- out[order(as.character(out$variable)),]
	yrange <- range(out$value)
	ymax <- yrange[2]
	ymin <- yrange[1]
	ymean <- mean(yrange)
	ydelta <- diff(yrange)/2
	oo3 <<- yrange
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
	ratio <- ratio %>% filter(date %in% (lockdown-14):max(out$date))
	if (maxperc=="auto") maxperc <- range(ratio$variation_perc,na.rm=TRUE) %>% abs() %>% max()
	if (maxperc>100) maxperc <- 100
	
	out <- full_join(out,ratio)
	
	out$variation_perc[out$variation_perc>maxperc] <- maxperc
	out$variation_perc[out$variation_perc<(-maxperc)] <- -maxperc
	
	out$plotted_variation_perc <- (out$variation_perc)*ydelta/maxperc+ymean
	variation_perc_breaks <- out$variation_perc[which(out$date==max(out$date))] %>% c(as.integer(seq(from=-1,to=1,by=percscale)*maxperc)) %>% sort()
	breaks_y <- out$value[which(out$date==max(out$date))] %>% rev()  %>% cumsum() %>% sort()
	
	breaks_x <- out$date %>% unique() %>% sort()
	ii_labels_x <- breaks_x %>% extract(as.numeric(breaks_x-breaks_x[1])%%4==0)
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
	
	oo1 <<- variation_perc_breaks
	
	oo2 <<- breaks_y
	###
	dtyprec <- 100
	
	###
	nbb <- 5
	dbreaks_y0 <- 10^(as.integer(log10(max(breaks_y))))/nbb
  breaks_y0 <- seq(from=0,to=max(breaks_y),by=dbreaks_y0)
 ## breaks_x <- out$date %>% unique() %>% sort()
  ii_labels_y0 <- breaks_y0 %>% extract(as.numeric(breaks_y0-breaks_y0[1])%%(dbreaks_y0*nbb)==0)
  labels_y0 <- breaks_y0 %>% as.character()
  labels_y0[!(breaks_y0 %in% ii_labels_y0)] <- ""
  
  
	gg <- gg+scale_y_continuous(breaks=breaks_y0,labels=labels_y0,minor_breaks=breaks_y,sec.axis = sec_axis(~ . *maxperc/ydelta-maxperc/ydelta*ymean,breaks=variation_perc_breaks,name="Aumenti Giornalieri [%]"))
	gg <- gg+scale_x_date(breaks=breaks_x,labels=labels_x)
if (prevalence==TRUE) {
  
  ylabt <- "Prevalenza (Casi su 1000 abitanti)"
  
} else {
  
  ylabt <- "Casi / Persone"
  
}
	gg <- gg+xlab("Tempo")+ylab(ylabt)+ggtitle(sprintf("Casi in %s",paste(regione,collapse=",")))
	
	attr(out,"ggplot") <- gg
	return(out)
}