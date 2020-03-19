# Load packages
library(shiny)
library(shinythemes)
library(covid19r)
library(stringplus)
#library(dplyr)
#library(readr)
## suggestion: https://shiny.rstudio.com/
# Load data
#trend_data <- read_csv("data/trend_data.csv")
#trend_description <- read_csv("data/trend_description.csv")
##
wpath <- '/home/ecor/Dropbox/R-packages/COVID-19/'
data_csv <- wpath+'dati-regioni/dpc-covid19-ita-regioni.csv' 

nord <- c("Lombardia","Veneto","Emilia Romagna",
"Piemonte","P.A. Trento","P.A. Bolzano","Friuli Venezia Giulia",
"Liguria","Valle d'Aosta")
centro <- c("Toscana","Lazio","Umbria","Marche","Abruzzo","Molise")
sud <- c("Puglia","Campania","Calabria","Sicilia","Basilicata","Sardegna")
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
##


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
		titlePanel("Covid19 Pandemia in Italia (Dati Regionali)"),
		sidebarLayout(
				sidebarPanel(
						
						# Select type of trend to plot
				    checkboxGroupInput(inputId = "regione", label = strong("Regioni"),
								choices = c(nord,centro,sud,"Tutte") %>% sort(),
								selected = "Lombardia",width='100%',inline=TRUE),
						
						# Select date range to be plotted
						dateRangeInput("date", strong("SElezione Date (opzionale)"), start = "2007-01-01", end = "2017-07-31",
								min = "2020-02-20", max = Sys.Date(),language="it",separator="a"),
						
						# Select whether to overlay smooth trend line
	###					checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
						
						# Display only if the smoother is checked
#						conditionalPanel(condition = "input.smoother == true",
#								sliderInput(inputId = "f", label = "Smoother span:",
#										min = 0.01, max = 1, value = 0.67, step = 0.01,
#										animate = animationOptions(interval = 100)),
#								HTML("Higher values give more smoothness.")
#						)
				width=12),
				
				# Output: Description, lineplot, and reference
				mainPanel(
						plotOutput(outputId = "covi19dItalyPlot", height = "2000px"),
						textOutput(outputId = "desc")
				##		tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
				,width=12)
		)
)

# Define server function
server <- function(input, output) {
	#print(input$date[1])
	# Subset data
	# selected_trends <- reactive({
	# 			req(input$date)
	# 			validate(need(!is.na(input$date$start) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
	# 			validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
	# 			trend_data %>%
	# 					filter(
	# 							type == input$type,
	# 							date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
	# 							))
	# 		})
	
	
	# Create scatterplot object the plotOutput function is expecting
	output$covi19dItalyPlot <- renderPlot({
	  #    date_start <- input$dateRangeInput_name_one[1]
	 #    date_end <- input$dateRangeInput_name_one[2]
	      regione <- input$regione
	      if (length(regione)==0) regione <- "Tutte"
	      if ("Tutte" %in% regione) regione <- "Tutte"
				covid19Italy(file=data_csv,regione=regione,start_date=input$date[1],end_date=input$date[2]) %>% attr("ggplot") %>% print()
	})
	output$desc <- renderUI({
			  
	      url_github <- a("Github Data Repository", href="https://github.com/pcm-dpc")
	      url <- a("Presidenza del Consiglio dei Ministri - Dipartimento della Protezione Civile",href="http://www.protezionecivile.it/")
				o <- paste( "Sorgente dati e rigraziamenti:",url," ; ","Protezione Civile Github Repository:",url_github)
				tagList(o)
				})
	
}

# Create Shiny object
shinyApp(ui = ui, server = server)