#shinydashboard for Weekly Production Report 
#ui.R

library(shiny)
library(shinydashboard)
library(xts)
library(d3heatmap)
library(dygraphs)
library(rCharts)
library(gplots)
library(DT)

dashboardPage(
	dashboardHeader(title = "Production Report"),
	dashboardSidebar(
		sidebarMenu(
			br(),
			menuItem(strong("Operations Overview"), tabName="opOverview", icon = icon("gear"),
				badgeLabel = "new", badgeColor = "green"),			

			br(),
			menuItem(strong("Department Overview"), tabName="deptOverview", icon = icon("line-chart")),		
			
			#br(),
			#menuItem(strong("Acutal Scrap by Dept"), tabName="scrap_overview", icon = icon("trash-o")),

			br(),
			menuItem(strong("Work Center Overview"), tabName = "wcOverview", icon = icon("area-chart")), 

			br(),
			menuItem(strong("Work Center Statistics"), tabName="stats", icon=icon("calculator"),
				badgeLabel = "new", badgeColor = "green"),
			hr(),

		selectInput("department",
			label=h4("Department:"),
			choices = list("Extrusion","MBValue","Printing","Slitting","Lamination"),
				selected = "Extrusion"),
		
		conditionalPanel(
			condition= "input.department == 'Extrusion'",
			selectInput("ext.wc", 
				label = h4("Work Center:"),
				choices = list("EC2","EC3","EM2","EM3","EM4","Wincaster","Wincoater 1","Wincoater 2"), 
				selected = "EC2")),

		conditionalPanel(
			condition= "input.department == 'MBValue'",
			selectInput("mb.wc", 
				label = h4("Work Center:"),
				choices = list("MB1","MB2","MB3"), 
				selected = "MB1")),

		conditionalPanel(
			condition= "input.department == 'Printing'",
			selectInput("print.wc", 
				label = h4("Work Center:"),
				choices = list("PC2","PC3","PT1","PW2","PW3"), 
				selected = "PC2")),
	
		conditionalPanel(
			condition= "input.department == 'Slitting'",
			selectInput("slit.wc", 
				label = h4("Work Center:"),
				choices = list("RS08","RS09","RS10","RS11","RS12","RS13","RS14","RS15","RS16","RS17","RS18","RS19"), 
				selected = "RS08")),

		conditionalPanel(
			condition= "input.department == 'Lamination'",
			selectInput("lam.wc", 
				label = h4("Work Center:"),
				choices = list("LC1","LF1","LN1","LN2","LN3"), 
				selected = "LC1")),

		selectInput("startYear", 
			label = h4("Year:"),
			choices = list(2013,2014,2015),
			selected = "2015"),

		checkboxInput("outliers", 
			label = h5("Show outliers"), 
			value = TRUE),

		hr()
		)#sidebarMenu
	), #dashboardSidebar

	dashboardBody(
		tabItems(

			tabItem(tabName = "opOverview",
				
					tabBox(
						title= tagList(shiny::icon("gear"),"Operations Overview"), height = "1000px", width="750px",
						
						tabPanel('%Productivity',	
							strong(h2("Winpak Division - Operations")),
							hr(),

							fluidRow(
								valueBoxOutput("deptprodThisWeek"),
								valueBoxOutput("deptprodLastWeek"),
								valueBoxOutput("deptprodYear")
							), #fluidRow

							fluidRow(
								showOutput("prod.linechart","nvd3"))
						),
						
						tabPanel('%Actual Scrap',
							strong(h2("Winpak Division - Operations")),
							hr(),

							fluidRow(
								valueBoxOutput("deptscrapThisWeek"),
								valueBoxOutput("deptscrapLastWeek"),
								valueBoxOutput("deptscrapYear")
							),

							fluidRow(
								showOutput("scrap.linechart","nvd3"))
						)
					)
				),

			tabItem(tabName = "deptOverview",
				
					tabBox(
						title = tagList(shiny::icon("bar-chart"),"Department Overview"), height = "1000px", width="750px",
						
						tabPanel('%Productivity',
							strong(h2(textOutput("department.select"))),
							hr(),
				
							fluidRow(
								valueBoxOutput("productivityBoxThisWeek"),
								valueBoxOutput("productivityBoxLastWeek"),
								valueBoxOutput("productivityBoxYear")
							),
				
							br(),
							fluidRow(
								showOutput("prod.barchart","nvd3"))
						),

						tabPanel('%Actual Scrap',
							strong(h2(textOutput("department.select2"))),
							hr(),

							fluidRow(
								valueBoxOutput("scrapBoxThisWeek"),
								valueBoxOutput("scrapBoxLastWeek"),
								valueBoxOutput("scrapBoxYear")
							),
				
							br(),
							fluidRow(
								showOutput("scrap.barchart","nvd3"))
						)
					)
			),

				#fluidRow(column(11, offset = 1,
				#	plotOutput("heatmap",height=700,width="90%")))),
				#fluidRow(column(10, offset = 1,
					#d3heatmapOutput("heatmap",height=700,width="90%")))),

			tabItem(tabName = "wcOverview",
				
				strong(h2(textOutput("department.select3"))),
				strong(h3(textOutput("workcenter.select"))),

				fluidRow(
					br(),
					dygraphOutput("dygraph", height=900, width="90%"))),					

			tabItem(tabName = "stats",

				fluidRow(
					column(10, offset=1,
					DT::dataTableOutput("prod.Table"))),
				
				#hr(),
				#strong(h4("Rows Selected:")),
				#verbatimTextOutput("table.indices"),

				hr(),

				fluidRow(
					column(4, offset = 1,
						strong(h4("Spread of Data by Week:")),
						plotOutput("sample.boxplot",height=450, width=600)),

					column(6, offset = 1,
						strong(h4("Mean of Prod/Scrap by Work Center")),
						plotOutput("prodscrapWC",height=450, width =700))
					),
				
				br(),
				fluidRow(
					column(4, offset = 1,
						strong(h4("Summary Statistics:")),
						tableOutput("sample.number"),

						fluidRow(
							strong(h4("Run Summary by MO Number:")),
							tableOutput("morundata"))
					),
	
					column(4, offset = 2,
						DT::dataTableOutput("moNumber"))
				)

				#fluidRow(
				#	column(5, offset = 1,
				#		strong(h4("Run Summary by MO Number:")),
				#		tableOutput("morundata"))
			#	)

#						br(),
#						fluidRow(							
#							strong(h4("Summary Statistics:")),
#							tableOutput("sample.number")),
#
#						br(),
#						fluidRow(
#							strong(h4("Run Summary by MO Number:")),
#							tableOutput("morundata"))						
#					),
#				
#					column(4, 
#						strong(h4("Mean of Prod/Scrap by Work Center")),
#						plotOutput("prodscrapWC",height=450, width =700),
#
				#		br(),
				#		fluidRow(
				#			column(4,offset=1,
				#			DT::dataTableOutput("moNumber")))
				#	)
				
				#)				
								
			)

				#br(),
				#fluidRow(showOutput("pie.prod","nvd3")))


				#fluidRow(column(10, offset = 1, strong(h3("Yearly Productivity Mean by Work Center")))),
				
				#fluidRow(showOutput("prod.barchart","nvd3")),
				#hr(),

				#fluidRow(column(10, offset = 1, strong(h3("Yearly Scrap Mean by Work Center")))))
				
				#fluidRow(showOutput("scrap.barchart","nvd3")))		
			
		)#tabItems
	) #dashboardBody
)#dashboardPage
	