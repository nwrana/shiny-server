#shinydashboard for Weekly Production Report
#server.R

############################ DATA ENTRY AND MANIPULATION ######################

### STEP1: LOAD PACKAGES ###

library(shinydashboard)
library(shiny)
library(dplyr)
library(rCharts) #to draw ineractive barcharts
library(gplots) #to draw heatmap.2()
#library(RColorBrewer) #add custom colors to heatmap
#library(plot3D) #to drawr contour plots and 3D perspective images
library(DT) # create HTML data tables

### STEP2: LOAD DATA INTO R ###
#data <- read.csv("./Production.Dashboard/data/data.csv", header=TRUE)
data <- read.csv("/home/nwrana/documents/shiny-server data/Production.Dashboard.data/data/data.csv", header=TRUE)
#data <- read.csv("./data/data.csv", header=TRUE)
#data <- read.csv("C:/Users/w15psnnw/Documents/3. R Programming/R-Shiny Apps/PRODUCTION RAW DATA/data.csv", header=TRUE)
#data <- read.csv("/srv/shiny-server/PRODUCTION RAW DATA/data.csv", header=TRUE)

### STEP3: ORGANIZE/SUMMARIZE DATA FOR VISUALIZATION ###

test <- data %>%
	mutate(Scrap.Percent = Actual.Scrap/Net.Issue) %>%
	mutate(Util.Percent = Run.Time/(Run.Time+Setup.Time+Down.Time)) %>%
	mutate(Earned.Run.Time = ((Produced+Doctoring+On.Hold+Off.Cut)+(Net.Issue*Planned.Scrap.Percentage))*Hours.Per.Kg) %>%
	mutate(Eff.Percent = Earned.Run.Time / Run.Time) %>%
	mutate(Prod.Percent = Util.Percent*Eff.Percent) 

test.clean <- test %>%
	filter(Scrap.Percent > 0 & Util.Percent > 0 & Eff.Percent > 0 & Prod.Percent > 0) %>%
	filter(Scrap.Percent < 1) %>%
	filter(Eff.Percent < 2.5) %>% #based on 99% of the numbers are below 200%
	distinct(Mo.Number)

####################

#4.1:%SCRAP
perc.Scrap <- data %>%
		select(Finish.Date, Mo.Number, Work.Center, Net.Issue, Actual.Scrap, Year, Month, Week, Department) %>%
		filter(Actual.Scrap != "NA") %>%
		filter(Net.Issue > 0 & Actual.Scrap > 0) %>%
		mutate(Scrap.Percent = Actual.Scrap/Net.Issue) %>%
		filter(Scrap.Percent > 0 & Scrap.Percent < 1)

#4.2:%UTILIZATION
perc.Util <- data %>%
		select(Finish.Date, Mo.Number, Work.Center, Run.Time, Setup.Time, Down.Time, Other.Time, Year, Month, Week, Department) %>%
		filter(Run.Time != "NA") %>%
		filter(Run.Time > 0 & Setup.Time >= 0 & Down.Time >= 0 & Other.Time >= 0) %>%
		mutate(Util.Percent = Run.Time/(Run.Time+Setup.Time+Down.Time)) %>%
		filter(Util.Percent < 1) #100% Util = 20% of all runs. Removed this stat

#4.3:%EFFICIENCY
perc.Eff <- data %>%
		select(Finish.Date, Mo.Number, Work.Center, Run.Time, Produced, Doctoring, On.Hold, Off.Cut, Net.Issue, Planned.Scrap.Percentage, Hours.Per.Kg, Year, Month, Week, Department) %>%
		filter(Produced != "NA" & Run.Time != "NA") %>% 
		filter(Produced > 0 & Run.Time > 0 & Doctoring >= 0 & On.Hold >= 0 & Off.Cut >= 0 & Planned.Scrap.Percentage >= 0 & Hours.Per.Kg > 0 & Net.Issue > 0) %>%
		mutate(Earned.Run.Time = ((Produced+Doctoring+On.Hold+Off.Cut)+(Net.Issue*Planned.Scrap.Percentage))*Hours.Per.Kg) %>%
		mutate(Eff.Percent = Earned.Run.Time / Run.Time) %>%
		filter(Eff.Percent < 10)

#4.4:%PRODUCTIVITY
x1 <- perc.Util %>%
	select(Work.Center, Mo.Number, Year, Month, Week, Util.Percent, Department, Finish.Date)

x2 <- perc.Eff %>%
	select(Mo.Number, Eff.Percent)

#combine both matrices based on common Mo numbers, remove any duplicates, and add new column
perc.Prod <- inner_join(x1, x2, by = "Mo.Number") %>%
		distinct(Mo.Number)%>%
		mutate(Prod.Percent = Util.Percent*Eff.Percent)

#### used to create Summary Table ########################################

prodscrapTable <- test.clean %>%
	filter(Year == max(data$Year)) %>%
	group_by(Department,Week) %>%
	summarise(Productivity = round(mean(Prod.Percent)*100,1), 
		Actual.Scrap = round(mean(Scrap.Percent)*100,1),
		Efficiency = round(mean(Eff.Percent)*100,1),
		Utilization = round(mean(Util.Percent)*100,1))

prod.Weekly <- perc.Prod %>%
	filter(Year == max(data$Year)) %>% 
	group_by(Department, Week) %>%
	summarize(Mean = round(mean(Prod.Percent)*100,1)) 

scrap.Weekly <- perc.Scrap %>%
	filter(Year == max(data$Year)) %>% 
	group_by(Department, Week) %>%
	summarize(Mean = round(mean(Scrap.Percent)*100,1))

eff.Weekly <- perc.Eff %>%
	filter(Year == max(data$Year)) %>% 
	group_by(Department, Week) %>%
	summarize(Mean = round(mean(Eff.Percent)*100,1))

util.Weekly <- perc.Util %>%
	filter(Year == max(data$Year)) %>% 
	group_by(Department, Week) %>%
	summarize(Mean = round(mean(Util.Percent)*100,1))

#prodscrapTable <- cbind(prod.Weekly, scrap.Weekly$Mean, eff.Weekly$Mean, util.Weekly$Mean)
#colnames(prodscrapTable) <- c("Department","Week","Productivity","Actual.Scrap","Efficiency","Utilization")

####### used to calculate interactive barcharts ########

##create a new data.frame that combines scrap and productivity data
scrap.nvd3 <- perc.Scrap %>%
	select(Mo.Number, Year, Department, Work.Center, Scrap.Percent)

prod.nvd3 <- perc.Prod %>%
	select(Mo.Number, Prod.Percent)

#df.nvd3 <- inner_join(scrap.nvd3, prod.nvd3 ,by="Mo.Number") %>%
#	distinct(Mo.Number)

df.nvd3 <- test.clean %>%
	select(Year,Week,Department,Work.Center,Scrap.Percent,Prod.Percent)

year.split <- split(df.nvd3, as.factor(df.nvd3$Year)) #creating a list of data.frames, separated by Year

############################ ShinyDashboard Server ######################

function(input,output) {

###### Set reactive arguments ######################

	datasetInput <- reactive({
		if(input$department == "Extrusion") {
			switch(input$ext.wc,"EC2" = "WXEC2","EC3"="WXEC3","EM2"="WXEM2","EM3"="WXEM3","EM4"="WXEM4","Wincaster"="WXEWN1","Wincoater 1"="WXELW1","Wincoater 2"="WXELW2")}
		else if(input$department == "MBValue") {
			switch(input$mb.wc,"MB1" = "WXEMB1","MB2" = "WXEMB2","MB3" = "WXEMB3")}
		else if(input$department == "Printing") {	
			switch(input$print.wc,"PC2" = "WXPC2","PC3"="WXPC3","PT1"="WXPT1","PW2"="WXPW2","PW3"="WXPW3")}
		else if(input$department == "Slitting") {
			switch(input$slit.wc,"RS08"= "WXRS08","RS09"="WXRS09","RS10"="WXRS10","RS11"="WXRS11","RS12"="WXRS12","RS13"="WXRS13","RS14"="WXRS14","RS15"="WXRS15","RS16"="WXRS16","RS17"="WXRS17","RS18"="WXRS18","RS19"="WXRS19")}
		else if(input$department == "Lamination") {
			switch(input$lam.wc,"LC1"="WXLC1","LF1"="WXLF1","LN1"="WXLN1","LN2"="WXLN2","LN3"="WXLN3")}
	})

##### Text Output ########

	output$department.select <- renderText({ input$department }) 
	output$department.select2 <- renderText({ input$department }) 
	output$department.select3 <- renderText({ input$department }) 

	output$workcenter.select <- renderText({ datasetInput() })


####### PRODUCTIVITY valueBox calculations ########

#Yearly Productivity
year.Prod <- reactive({
	yp <- perc.Prod %>%
			filter(Year == input$startYear) %>%
			filter(Department == input$department) %>%
			summarise(Mean = mean(Prod.Percent))

	yp <- round(yp*100,1)
})

#Current Week Productivity
current.Week <- reactive({
	wp <- perc.Prod %>%
			filter(Year == input$startYear) %>%
			filter(Department == input$department) %>%
			group_by(Week) %>%
			summarise(Mean = mean(Prod.Percent*100))

	cw <- wp[length(wp$Mean),] %>%
		select(Mean)
	cw <- round(cw,1)
})

#Previous Week Productivity

previous.Week <- reactive({
	wp <- perc.Prod %>%
			filter(Year == input$startYear) %>%
			filter(Department == input$department) %>%
			group_by(Week) %>%
			summarise(Mean = mean(Prod.Percent*100))

	pw <- wp[length(wp$Mean)-1,] %>%
			select(Mean)
	pw <- round(pw,1)
})

####### SCRAP valueBox calculations ########

#Yearly Productivity
year.Scrap <- reactive({
	ys <- perc.Scrap %>%
			filter(Year == input$startYear) %>%
			filter(Department == input$department) %>%
			summarise(Mean = mean(Scrap.Percent))

	ys <- round(ys*100,1)
})

#Current Week Productivity
current.Week.Scrap <- reactive({
	ws <- perc.Scrap %>%
			filter(Year == input$startYear) %>%
			filter(Department == input$department) %>%
			group_by(Week) %>%
			summarise(Mean = mean(Scrap.Percent*100))

	cws <- ws[length(ws$Mean),] %>%
		select(Mean)
	cws <- round(cws,1)
})

#Previous Week Productivity

previous.Week.Scrap <- reactive({
	ws <- perc.Scrap %>%
			filter(Year == input$startYear) %>%
			filter(Department == input$department) %>%
			group_by(Week) %>%
			summarise(Mean = mean(Scrap.Percent*100))

	pws <- ws[length(ws$Mean)-1,] %>%
			select(Mean)
	pws <- round(pws,1)
})

###### Overall Productivity valueBox calculations ######

total.year.prod <- reactive({
	tp <- prod.Weekly$Mean
	tp <- round(mean(tp),1) })
	
total.previous.week.prod <- reactive ({
	prev.week <- max(prod.Weekly$Week) - 1
	week.prod <- prod.Weekly %>%
		filter(Week == prev.week)
	week.prod <- round(mean(week.prod$Mean),1) })

total.current.week.prod <- reactive({
	max.week <- max(prod.Weekly$Week)
	week.prod <- prod.Weekly %>%
		filter(Week == max.week)
	week.prod <- round(mean(week.prod$Mean),1) })

###### Overall Scrap valueBox calculations ######

total.year.scrap <- reactive({
	ts <- scrap.Weekly$Mean
	ts <- round(mean(ts),1) })
	
total.previous.week.scrap <- reactive ({
	prev.week <- max(scrap.Weekly$Week) - 1
	week.scrap <- scrap.Weekly %>%
		filter(Week == prev.week)
	week.scrap <- round(mean(week.scrap$Mean),1) })

total.current.week.scrap <- reactive({
	max.week <- max(scrap.Weekly$Week)
	week.scrap <- scrap.Weekly %>%
		filter(Week == max.week)
	week.scrap <- round(mean(week.scrap$Mean),1) })

#### Summary Table #####

	output$prod.Table <- DT::renderDataTable(
		prodscrapTable, rownames=TRUE, server=FALSE, selection="single",class = "cell-border stripe", 
		filter="top",caption = "Table 1: Summary by Department by Week") #%>%
			#formatStyle(c("Productivity","Actual.Scrap"), fontWeight = "bold")

### Display which rows have been selected by the user ###

	output$table.indices <- renderPrint({
		input$prod.Table_rows_selected })

#### Summary Table Statistics #############
	
	output$sample.number <- renderTable({
	
	s <- input$prod.Table_rows_selected #each number is stored as a single element in a list
		
	if (length(s) == 0){
	
		tmp <- do.call(data.frame, 
          			list(sd = "NA", median = "NA",min = "NA",max = "NA", n = "NA"))
		return(tmp)

	} else {		
		search.matrix <- prodscrapTable[s,] %>%
			select(Department, Week)%>%
			as.data.frame(.)		

		summary.stats <- test.clean %>%
			filter(Year == max(data$Year)) %>%
			filter(Department %in% search.matrix[,1]) %>%
			filter(Week %in% search.matrix[,2])%>%
			select(Prod.Percent,Scrap.Percent,Eff.Percent,Util.Percent)
		
		#summary.stats <- perc.Prod %>%
		#	filter(Year == max(data$Year)) %>%
		#	filter(Department %in% search.matrix[,1]) %>%
		#	filter(Week %in% search.matrix[,2])%>%
		#	select(Prod.Percent,Eff.Percent,Util.Percent)

	tmp <- do.call(data.frame, 
          list( 	mean = apply(summary.stats,2,mean),
			sd = apply(summary.stats, 2, sd),
                	median = apply(summary.stats, 2, median),
               	min = apply(summary.stats, 2, min),
              	max = apply(summary.stats, 2, max),
             	n = apply(summary.stats, 2, length)))

		
		return(tmp)}
	})

#### Summary Boxplot #########

	output$sample.boxplot <- renderPlot({

	s <- input$prod.Table_rows_selected

	search.matrix <- prodscrapTable[s,] %>%
			select(Department, Week)%>%
			as.data.frame(.)
				
		summary.stats <- test.clean %>%
			filter(Year == max(data$Year)) %>%
			filter(Department %in% search.matrix[,1]) %>%
			filter(Week %in% search.matrix[,2])%>%
			select(Prod.Percent,Scrap.Percent, Eff.Percent,Util.Percent)

	return(boxplot(summary.stats,
		xlab = "Statistic",
		ylab = "Percent,%",
		#ylim = c(0,quantile(summary.stats$Eff.Percent,0.95)),
		outline=input$outliers))

})



#
#		#search.matrix <- prodscrapTable[as.numeric(rownames(prodscrapTable))%in% s,] %>%
#		final <- NULL
#		for(i in 1:length(s)){
#			temp <- s[[i]]
#			final <- cbind(final, temp)}
#		sum.selection <- NULL	
#		for(i in 1:length(search.matrix)){
#			
#			search <- search.matrix[i,]
#
#			summary.stats <- perc.Prod %>%
#				filter(Year == max(data$Year)) %>%
#				filter(Department %in% search[,1]) %>%
#				filter(Week %in% search[,2])	
#	
#			sum.selection <- rbind(sum.selection, summary.stats)}
	
### summary of MO numbers for a selected weekly entry ###	

	modata <- reactive ({

		s <- input$prod.Table_rows_selected

		search.matrix <- prodscrapTable[s,] %>%
			select(Department, Week)%>%
			as.data.frame(.)
				
		summary.moNumber <- test.clean %>%
			filter(Year == max(data$Year)) %>%
			filter(Department %in% search.matrix[,1]) %>%
			filter(Week %in% search.matrix[,2])%>%
			select(Mo.Number,Work.Center, Product.Number)

		return(summary.moNumber)
	})
 	
	output$moNumber <- DT::renderDataTable(
		modata(), rownames=FALSE, server=TRUE, selection="multiple",class = "cell-border stripe", 
		filter="top", caption = "Table 2: Summary of Mo Numbers")	

### summary of Run specs for a given MO Number ###

	output$morundata <- renderTable({

		s <- input$moNumber_rows_selected

#		search.matrix <- modata()[s,]%>%
#			select(Mo.Number) %>%
#			as.data.frame(.)
#		
		summary.mo <- test.clean %>%
			filter(Mo.Number %in% s) %>%
			select(Mo.Number,Finish.Date,Net.Issue,Produced,Actual.Scrap,Run.Time,Setup.Time,Other.Time,Down.Time)%>%
			mutate(Total.Time = Run.Time + Setup.Time + Other.Time + Down.Time) %>%
			as.data.frame(.)

		return(summary.mo)
	})

### summary of productivity and scrap by work center ###

	wcsummaryData <- reactive ({
	
		s <- input$prod.Table_rows_selected

		search.matrix <- prodscrapTable[s,] %>%
			select(Department, Week) %>%
			as.data.frame(.)

		summary.wc <- test.clean %>%
			filter(Year == max(data$Year)) %>%
			filter(Department %in% search.matrix[,1]) %>%
			filter(Week %in% search.matrix[,2])%>%
			select(Work.Center, Prod.Percent, Scrap.Percent) %>%
			group_by(Work.Center) %>%
			summarise(Prod.Mean = mean(Prod.Percent), Scrap.Mean = mean(Scrap.Percent)) %>%
			as.data.frame(.)

		return(summary.wc)
	})

	output$prodscrapWC <- renderPlot ({

	#Need to get data into correct format
	sum <- wcsummaryData()[,2:3]
	names <- wcsummaryData()[,1]
	
	trans <- t(as.matrix(sum))
	colnames(trans) <- names

	return(barplot(trans, beside=TRUE, col=c("darkgray","lightgray"), legend=rownames(trans)))

	})
			
#####used to create line charts NVD3 ########

	output$prod.linechart <- renderChart2({
		prod <- nPlot(Productivity ~ Week, group = "Department",
			data = prodscrapTable,
			type = "lineChart")
		prod$addParams(width = 1200, height = 750)
		prod$xAxis(axisLabel = "Week")
		prod$yAxis(axisLabel = "Productivity, %")
			
		return(prod)
	})

	output$scrap.linechart <- renderChart2({
		scrap <- nPlot(Actual.Scrap ~ Week, group = "Department",
			data = prodscrapTable,
			type = "lineChart")
		scrap$addParams(width = 1200, height = 750)
		scrap$xAxis(axisLabel = "Week")
		scrap$yAxis(axisLabel = "Actual Scrap, %")

		return(scrap)
	})


############## Dygraph interactive line charts ###################################

	output$dygraph <- renderDygraph({

	prod <- perc.Prod %>%
		#filter(Year == input$startYear) %>%
		filter(Department == input$department) %>%
		filter(Work.Center == datasetInput()) %>%
		select(Finish.Date, Prod.Percent, Week) %>%
		mutate(Finish.Date = as.Date(Finish.Date)) %>%
		#mutate(Week = as.factor(format(Finish.Date,'%W'))) %>%
		mutate(Week = as.Date(cut(Finish.Date, breaks="week"))) %>% #in order to use xts, weeks must be in Dates format. Week starts on a Monday
		group_by(Week) %>%
		summarise(mean=mean(Prod.Percent))%>%
		as.data.frame(.)

	scrap <- perc.Scrap %>%
		#filter(Year == input$startYear) %>%
		filter(Department == input$department) %>%
		filter(Work.Center == datasetInput()) %>%
		select(Finish.Date, Scrap.Percent, Week) %>%
		mutate(Finish.Date = as.Date(Finish.Date)) %>%
		#mutate(Week = as.factor(format(Finish.Date,'%W'))) %>%
		mutate(Week = as.Date(cut(Finish.Date, breaks="week"))) %>%
		group_by(Week) %>%
		summarise(mean=mean(Scrap.Percent))%>%
		as.data.frame(.)

	df <- full_join(prod,scrap,by="Week") #additonal rows are written as NA
	colnames(df) <- c("Week","Productivity","Scrap")

	df.xts <- xts(df, order.by=df$Week) #must convert dataframe to an xts timeseries matrix

	dygraph(df.xts, main="Productivity vs Actual Scrap, %") %>%
		dyRangeSelector() %>%
		dyAxis("x", label="Date") %>%
		dyAxis("y", label='Percent,%') %>%
		dySeries("Productivity",label="Productivity",fillGraph=TRUE) %>%
		dySeries("Scrap",label="Scrap",fillGraph=TRUE) %>%
		dyHighlight(highlightCircleSize = 5, 
             	 highlightSeriesBackgroundAlpha = 0.65,        
		     	 hideOnMouseOut = TRUE)
 	})

	output$year <- renderText({
		paste("Year:", input$startYear)
	})

	output$dept <- renderText({
		paste("Department:", input$department)
	})


#####D3heatmap not compatible with other java widgets running on same server 

	#output$heatmap <- renderD3heatmap({
	output$heatmap <- renderPlot({	
	
		review.Year <- perc.Prod %>% 
				filter(Year == input$startYear)
			
		test <- split(review.Year, review.Year$Department) #creates list of dataframes organized by dept
		
		#Need loop because we have to group data 2x (by Department, by week) 
		final <- NULL
		final.name <- NULL
		for(i in 1:length(test)){
			temp <- test[[i]]
			df <- tapply(temp$Prod.Percent,temp$Week,mean)
			final <- cbind(final,df)
			colname <- names(test[i]) #label the column with the appropriate department
			final.name <- cbind(final.name, colname)
		}		

		final <- as.data.frame(final)
		colnames(final) <- final.name
		final2 <- as.matrix(final)
		m <- t(final2) #transpose matrix to reorganize rows/columsn of heatmap
		
		#d3heatmap(m, dendrogram="none", scale="col", colors="Blues") #, color=heat.colors(256))
		my_palette <-  colorRampPalette(c("darkgreen", "white"))(n = 1000)		

		heatmap.2(m, xlab="Production Week", scale="col",
		density.info="none",trace="none",
		col=my_palette, dendrogram="none", Colv="NA", cexRow=1.5,cexCol=1.5,
		lmat = rbind(c(0,3),c(2,1),c(0,4)),lhei = c(0.5,10,1.5),lwid = c(0.1,4))

	})

#	output$week_Prod <- renderPlot({
#	
#		weekTime <- perc.Prod %>%
#			filter(Work.Center == "WXPW2") %>% #datasetInput())%>%
#			filter(Year == 2014) %>% #input$startYear) %>%	
#			group_by(as.numeric(Week)) %>%
#			summarise(mean=mean(Prod.Percent))
#				
#		plot(weekTime, 
#			type="l",
#			xlab = "Week",ylab = "Avg. %Prod",
#			xlim = c(0,52),
#			ylim = c(0,max(weekTime[,2])))
#
#		points(weekTime)
#		abline(h=0.45, col="red")
#	})


#### KPI valueBoxes PRODUCTIVITY ####

	output$productivityBoxYear <- renderValueBox({

		valueBox(
			paste(year.Prod(), "%"),
			"Year",
			icon=icon("line-chart"),color="purple")
	})
		
	output$productivityBoxLastWeek <- renderValueBox ({
		
		if(previous.Week() > year.Prod()){
			valueBox(
				paste(previous.Week(),"%"),
				"Previous Week",
				icon=icon("arrow-up"),color="olive")
		} else if(previous.Week() < year.Prod()){
			valueBox(
				paste(previous.Week(),"%"),
				"Previous Week",
				icon=icon("arrow-down"),color="red")
		} else {
			valueBox(
				paste(previous.Week(),"%"),
				"Previous Week",
				icon=icon("arrows-h"),color="purple")}
		})
		
	output$productivityBoxThisWeek <- renderValueBox ({
		
		if(current.Week() > previous.Week()){
			valueBox(
				paste(current.Week(),"%"),
				"Current Week",
				icon=icon("arrow-up"),color="olive")
		} else if(current.Week() < previous.Week()){
			valueBox(
				paste(current.Week(),"%"),
				"Current Week",
				icon=icon("arrow-down"),color="red")
		} else {
			valueBox(
				paste(current.Week(),"%"),
				"Current Week",
				icon=icon("arrows-h"),color="purple")}
	})

#### KPI valueBoxes SCRAP ####

	output$scrapBoxYear <- renderValueBox({

		valueBox(
			paste(year.Scrap(), "%"),
			"Year",
			icon=icon("trash-o"),color="purple")
	})
		
	output$scrapBoxLastWeek <- renderValueBox ({
		
		if(previous.Week.Scrap() > year.Scrap()){
			valueBox(
				paste(previous.Week.Scrap(),"%"),
				"Previous Week",
				icon=icon("arrow-up"),color="red")
		} else if(previous.Week.Scrap() < year.Scrap()){
			valueBox(
				paste(previous.Week.Scrap(),"%"),
				"Previous Week",
				icon=icon("arrow-down"),color="olive")
		} else {
			valueBox(
				paste(previous.Week.Scrap(),"%"),
				"Previous Week",
				icon=icon("arrows-h"),color="purple")}
		})
		
	output$scrapBoxThisWeek <- renderValueBox ({
		
		if(current.Week.Scrap() > previous.Week.Scrap()){
			valueBox(
				paste(current.Week.Scrap(),"%"),
				"Current Week",
				icon=icon("arrow-up"),color="red")
		} else if(current.Week.Scrap() < previous.Week.Scrap()){
			valueBox(
				paste(current.Week.Scrap(),"%"),
				"Current Week",
				icon=icon("arrow-down"),color="olive")
		} else {
			valueBox(
				paste(current.Week.Scrap(),"%"),
				"Current Week",
				icon=icon("arrows-h"),color="purple")}
	})

####### Department PRODUCTION Overview valueboxes ###############

	output$deptprodYear <- renderValueBox ({
		
		max.year <- max(data$Year)
			
		valueBox(
			paste(total.year.prod(), "%"),
			paste("YTD:",max.year),
			icon=icon("line-chart"),color="purple")
	})
	
	output$deptprodLastWeek <- renderValueBox({
		
		prev.week <- max(prod.Weekly$Week) - 1	

		if(total.previous.week.prod() > total.year.prod()){
			valueBox(
				paste(total.previous.week.prod(), "%"),
				paste("Last Week:",prev.week),
				icon=icon("arrow-up"),color="olive")

		} else if(total.previous.week.prod() < total.year.prod()){
			valueBox(
				paste(total.previous.week.prod(), "%"),
				paste("Last Week:",prev.week),
				icon=icon("arrow-down"),color="red")

		} else {
			valueBox(
				paste(total.previous.week.prod(), "%"),
				paste("Last Week:",prev.week),
				icon=icon("arrow-h"),color="purple")}
	})

	output$deptprodThisWeek <- renderValueBox ({
		
		this.week <- max(prod.Weekly$Week)
		
		if(total.current.week.prod() > total.previous.week.prod()){
			valueBox(
				paste(total.current.week.prod(), "%"),
				paste("This Week:",this.week),
				icon=icon("arrow-up"),color="olive")
		
		} else if(total.current.week.prod() < total.previous.week.prod()){
			valueBox(
				paste(total.current.week.prod(), "%"),
				paste("This Week:",this.week),
				icon=icon("arrow-down"),color="red")

		} else {
			valueBox(
				paste(total.current.week.prod(), "%"),
				paste("This Week:",this.week),
				icon=icon("arrow-h"),color="purple")}
	})


####### Department SCRAP Overview valueboxes ###############

	output$deptscrapYear <- renderValueBox ({
		
		max.year <- max(data$Year)
			
		valueBox(
			paste(total.year.scrap(), "%"),
			paste("YTD:",max.year),
			icon=icon("trash-o"),color="purple")
	})
	
	output$deptscrapLastWeek <- renderValueBox({
		
		prev.week <- max(scrap.Weekly$Week) - 1	

		if(total.previous.week.scrap() > total.year.scrap()){
			valueBox(
				paste(total.previous.week.scrap(), "%"),
				paste("Last Week:",prev.week),
				icon=icon("arrow-up"),color="red")

		} else if(total.previous.week.scrap() < total.year.scrap()){
			valueBox(
				paste(total.previous.week.scrap(), "%"),
				paste("Last Week:",prev.week),
				icon=icon("arrow-down"),color="olive")

		} else {
			valueBox(
				paste(total.previous.week.scrap(), "%"),
				paste("Last Week:",prev.week),
				icon=icon("arrow-h"),color="purple")}
	})

	output$deptscrapThisWeek <- renderValueBox ({
		
		this.week <- max(scrap.Weekly$Week)
		
		if(total.current.week.scrap() > total.previous.week.scrap()){
			valueBox(
				paste(total.current.week.scrap(), "%"),
				paste("This Week:",this.week),
				icon=icon("arrow-up"),color="red")
		
		} else if(total.current.week.scrap() < total.previous.week.scrap()){
			valueBox(
				paste(total.current.week.scrap(), "%"),
				paste("This Week:",this.week),
				icon=icon("arrow-down"),color="olive")

		} else {
			valueBox(
				paste(total.current.week.scrap(), "%"),
				paste("This Week:",this.week),
				icon=icon("arrow-h"),color="purple")}
	})
			
###################### rCharts - Interactive BarCharts ######################

	output$prod.barchart <- renderChart2({
		
		bars.prod <- NULL 
		for(i in 1:length(year.split)) {
			temp.prod <- year.split[[i]]
			sub.prod <- temp.prod %>%
				filter(Department == input$department) %>%
				group_by(Work.Center,Year) %>%
				summarise(Mean.Prod = mean(Prod.Percent), Mean.Scrap = mean(Scrap.Percent)) %>%
				mutate(Mean.Prod = round(Mean.Prod*100,1))%>%
				mutate(Mean.Scrap = round(Mean.Scrap*100,1))
			bars.prod <- rbind(bars.prod,sub.prod)
		}
		
		current.Year <- as.factor(data$Year)
		current.Year <- length(levels(current.Year))-1

		temp <- year.split[[current.Year]]

		current.Week <-max(temp$Week)
					
		current.Week.prod <- temp %>%
			filter(Department == input$department) %>%
			filter(Week == current.Week) %>%
			group_by(Work.Center) %>%
			summarise(Mean.Prod = mean(Prod.Percent),Mean.Scrap = mean(Scrap.Percent)) %>%
			mutate(Mean.Prod = round(Mean.Prod*100,1),Mean.Scrap = round(Mean.Scrap*100,1))
		
		l <- length(current.Week.prod$Work.Center)
		current.Week.prod$Year <- rep("Current.Week",l)
					
		currentWeekProd <- current.Week.prod[,c(1,4,2,3)]

		testing <- rbind(bars.prod,currentWeekProd)
		
		prod.plot <- nPlot(Mean.Prod~Work.Center, group="Year",
		data=testing,type="multiBarChart")
			prod.plot$xAxis(axisLabel = "Work Center")
			prod.plot$yAxis(axisLabel = "Productivity")
			prod.plot$addParams(width = 1100, height = 750)

		return(prod.plot)
	})

	output$scrap.barchart <- renderChart2({
		
		bars.scrap <- NULL 
		for(i in 1:length(year.split)) {
			temp.scrap <- year.split[[i]]
			sub.scrap <- temp.scrap %>%
				filter(Department == input$department) %>%
				group_by(Work.Center,Year) %>%
				summarise(Mean.Scrap = mean(Scrap.Percent)) %>%
				mutate(Mean.Scrap = round(Mean.Scrap*100,1))
			bars.scrap <- rbind(bars.scrap,sub.scrap)
		}
		
		current.Year <- as.factor(data$Year)
		current.Year <- length(levels(current.Year))-1

		temp <- year.split[[current.Year]]

		current.Week <-max(temp$Week)
					
		current.Week.scrap <- temp %>%
			filter(Department == input$department) %>%
			filter(Week == current.Week) %>%
			group_by(Work.Center) %>%
			summarise(Mean.Scrap = mean(Scrap.Percent)) %>%
			mutate(Mean.Scrap = round(Mean.Scrap*100,1))
		
		l <- length(current.Week.scrap$Work.Center)
		current.Week.scrap$Year <- rep("Current.Week",l)
					
		currentWeekScrap <- current.Week.scrap[,c(1,3,2)]

		testing <- rbind(bars.scrap,currentWeekScrap)

#		bars.scrap <- NULL 
#		for(i in 1:length(year.split)) {
#			temp.scrap <- year.split[[i]]
#			sub.scrap <- temp.scrap %>%
#				filter(Department == input$department) %>%
#				group_by(Work.Center,Year) %>%
#				summarise(Mean.Prod = mean(Prod.Percent), Mean.Scrap = mean(Scrap.Percent)) %>%
#				mutate(Mean.Prod = round(Mean.Prod*100,1))%>%
#				mutate(Mean.Scrap = round(Mean.Scrap*100,1))
#			bars.scrap <- rbind(bars.scrap,sub.scrap)
#	}
		scrap.plot <- nPlot(Mean.Scrap~Work.Center, group="Year",
		data=testing,type="multiBarChart")
			scrap.plot$xAxis(axisLabel = "Work Center")
			scrap.plot$yAxis(axisLabel = "Actual Scrap")
			scrap.plot$addParams(width = 1100, height = 750)

		return(scrap.plot)
	})

##### PIE chart - Production Overview by Department #####

#	output$pie.prod <- renderChart2({
#
#	pie.Prod <- perc.Prod %>%
#		filter(Year == input$startYear) %>%
#		select(Year, Department, Prod.Percent) %>%
#		group_by(Department) %>%
#		summarise(mean.prod = mean(Prod.Percent))%>%
#		mutate(mean.prod = round(mean.prod*100,2))%>%
#		as.data.frame(.)
#
#	prod_pieChart <- nPlot(x="Department", y="mean.prod", data=pie.Prod, type="pieChart")
#		prod_pieChart$chart(tooltipContent = "#! function(key, y, e, graph){return '<h3>' + 'Depatment: ' + key + '</h3>' + '<p>'+ 'Value ' + y} !#")
#		prod_pieChart$set(width = 800, height = 500)
#	
#	return(prod_pieChart)
#	
#	})

}#server
