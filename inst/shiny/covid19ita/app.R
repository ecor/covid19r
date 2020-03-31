# Load packages
library(shiny)
library(shinythemes)
library(remotes)
  if (!("covid19r" %in% installed.packages()[,1])) install_github("ecor/covid19r")
library(covid19r)

# if (o==FALSE) {
#   install.packages("shiny")
#   o <- require(shinythemes)
# }
# ###
# o <- require(covid19r)
# if (o==FALSE) {
#   if (!require(devtools)) install.packages("devtools")
#   devtools::install_github("ecor/covid19r")
#   o <- require(covid19r)
# }

####

nord <- c("Lombardia","Veneto","Emilia Romagna",
"Piemonte","P.A. Trento","P.A. Bolzano","Friuli Venezia Giulia",
"Liguria","Valle d'Aosta")
centro <- c("Toscana","Lazio","Umbria","Marche","Abruzzo","Molise")
sud <- c("Puglia","Campania","Calabria","Sicilia","Basilicata","Sardegna")
layer <- c("lA0_tamponi","lA1_positivi","lA2_positivi_dimessi","lB2_attualmente_positivi","lB3_attualmente_ospedalizzati","lB4_terapia_intensiva","lC5_deceduti")
###
url <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
data_csv <-tempfile()

download.file(url=url, destfile=data_csv)
###

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
		titlePanel("Covid19 Pandemia in Italia (Dati Regionali)"),
		sidebarLayout(
				sidebarPanel(
						
						# Select type of trend to plot
				    checkboxGroupInput(inputId = "regione", label = strong("Regioni"),
								choices = c(nord,centro,sud,"Tutte") %>% sort(),
								selected = "Lombardia",width='100%',inline=TRUE),
				    # Select type of trend to plot
				    checkboxGroupInput(inputId = "layer", label = strong("Variabli:"),
				                       choices = layer,
				                       selected = layer,width='100%',inline=TRUE),
						# Select date range to be plotted
						dateRangeInput("date", strong("Selezione Date (opzionale)"), start = "2007-01-01", end = "2017-07-31",
								min = "2020-02-20", max = Sys.Date(),language="it",separator="a"),
	
				width=12),
				
				# Output: Description, plot, and reference
				mainPanel(
						plotOutput(outputId = "covi19dItalyPlot", height = "2000px"),

						
					  tags$a(href = "httpss//www.protezionecivile.gov.it", "Presidenza del Consiglio dei Ministri - Dipartimento della Protezione Civile", target = "_blank"),
	          tags$a(href = "https://github.com/pcm-dpc", "Official Github Data Repository", target = "_blank")  
				,width=12)
		)
)

# Define server function
server <- function(input, output) {
	
	output$covi19dItalyPlot <- renderPlot({
	
	      layer <- input$layer
	      regione <- input$regione
	      if (length(regione)==0) regione <- "Tutte"
	      if ("Tutte" %in% regione) regione <- "Tutte"
				covid19Italy(file=data_csv,regione=regione,start_date=input$date[1],end_date=input$date[2],layer=layer) %>% attr("ggplot") %>% print()
	})
	output$desc <- renderText({
			  
	      url_github <- a("Github Data Repository", href="https://github.com/pcm-dpc")
	      url <- a("Presidenza del Consiglio dei Ministri - Dipartimento della Protezione Civile",href="http://www.protezionecivile.it/")
				o <- paste( "Sorgente dati e rigraziamenti:",url," ; ","Protezione Civile Github Repository:",url_github)
				print(o)
				})
	
}

# Create Shiny object
shinyApp(ui = ui, server = server)