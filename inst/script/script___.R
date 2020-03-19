##NULL##
#rm(list=ls())

library(dplyr)
library(magrittr)
library(stringplus)
library(ggplot2)
library(stringr)
library(reshape2)

source('/home/ecor/Dropbox/R-packages/covid19r/R/covid_italy.R')


wpath <- '/home/ecor/Dropbox/R-packages/COVID-19/'

#####

out_csv <- wpath+'dati-regioni/dpc-covid19-ita-regioni.csv' 

out <- covid19Italy(file=out_csv,regione="Lombardia",start_date='') 

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
sud <- c("Puglia","Campania","Calabria","Sicilia","Basilicata","Sardegna")


regions <- sud
regions <- "Lombardia"

out <- covid19Italy(file=out_csv,regione=centro,start_date='') 
out <- covid19Italy(file=out_csv,regione="tutte",start_date='') 
out <- covid19Italy(file=out_csv,regione=sud,start_date='') 






