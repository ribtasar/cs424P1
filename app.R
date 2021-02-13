


# Project 1 CS 424 Spring 2020 UIC - Rabia Ibtasar
#To deploy 
#library(rsconnect)
#rsconnect::deployApp('path/to/your/app')
#libraries to include


library(usmap)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(DT)
library(gridExtra)
#library(jpeg)
#library(grid)
#library(leaflet)
#library(scales)

#set the correct wd
#setwd('C:/Users/ribta/OneDrive/Desktop/UIC/Courses/Spring 2021/CS424/project1')

#Cleaning up the data for analysis
#read the energy data into file 
energy_file<-read.csv(file = 'annual_generation_state.csv')

#remane the columns name for ease
names(energy_file)[5] <- "GENERATION"
names(energy_file)[4]<-"SOURCE"
names(energy_file)[3]<-"PRODUCER"

#check the column types
#sapply(energy_file,class)

#remove all the , from GEN column
#convert GEN column into numeric values need to use gsub instead of sub

energy_file$GENERATION <- as.numeric( gsub(",", "", energy_file$GENERATION))

#head(energy_file)
#sort the STATE column to find missing values
energy_file<-energy_file[order(energy_file$STATE),]

#use subset to remove the STATES with empty cells
energy_file<-subset(energy_file, STATE!="  ")

#change the US-Total values to US-TOTAL so that all are in Uppercase now
energy_file[,2] = toupper(energy_file[,2])

#remove all rows where generation is negative but still keep the ones with 0
energy_file <- subset(energy_file, GENERATION>=0)

#removethe 4 energy sources that are not required.42293 remain
energy_file <- subset(energy_file, (SOURCE!= "Other"))
energy_file <- subset(energy_file, SOURCE!= "Other Gases")
energy_file <- subset(energy_file, SOURCE!= "Other Biomass")
energy_file <- subset(energy_file, SOURCE!= "Pumped Storage")


#convert the columns into factors
energy_file$STATE <- as.factor(energy_file$STATE)
energy_file$PRODUCER <- as.factor(energy_file$PRODUCER)
energy_file$SOURCE <- as.factor(energy_file$SOURCE)


#rename energy sources to make easier: hydro, wood, electric, solar

levels(energy_file$SOURCE)[levels(energy_file$SOURCE)=="Hydroelectric Conventional"]<- "Hydro"
levels(energy_file$SOURCE)[levels(energy_file$SOURCE)=="Wood and Wood Derived Fuels"]<- "Wood"
levels(energy_file$SOURCE)[levels(energy_file$SOURCE)=="Solar Thermal and Photovoltaic"] <- "Solar"

#initial Data cleanup complete
#create two new separate datasets for plotting the total amount and percentages of each energy source: amountsData and percentsData

#sourceData<-subset(energy_file, energy_file$SOURCE!="Total")
#totalAmtStates<-subset(energy_file,energy_file$PRODUCER=="Total Electric Power Industry")
#amountsData<-energy_file %>% group_by(YEAR,SOURCE) %>% summarize(Amount=sum(GENERATION))
#percentsData<-sourceData %>% count(YEAR,SOURCE) %>% group_by(YEAR) %>% mutate(percent=(n/sum(n))*100) %>% ungroup()

#subset of file in which we get all the Total Electric Power Industry rows that gives us the total amount for each energy source for the state/year
amountsData<-subset(energy_file,energy_file$PRODUCER=="Total Electric Power Industry")
#sourceData does not have the totals for plotting
sourceData<-subset(amountsData, amountsData$SOURCE!="Total")


#create a second data set for the percent data
onlyTotals<-filter(amountsData,SOURCE=="Total")
onlyTotals<-rename(onlyTotals, TOTALS=GENERATION)
percentsData<-amountsData %>% right_join(onlyTotals,by=c("YEAR","STATE"))
percentsData <- mutate(percentsData,(PERCENTS=GENERATION/TOTALS)*100)
percentsData<-rename(percentsData, PRODUCER=PRODUCER.x, SOURCE=SOURCE.x,TOTALSforYEAR=TOTALS)
names(percentsData)[names(percentsData) == "(PERCENTS = GENERATION/TOTALS) * 100"] <- "PERCENTS"

#for table displays hiding columns
displayAmounts<- select(amountsData,YEAR,STATE,SOURCE,GENERATION)
displayPercents<- select(percentsData,YEAR,STATE,SOURCE,PERCENTS)


#create menu items to select different years, states, and sources
years<-c(1990:2019)
sourceList<-as.list(levels(sourceData$SOURCE))
stateList<-as.list(levels(sourceData$STATE))

options(scipen = 999)

colors<-brewer.pal(n=10,name="Set3")
#colors contain the names of 10 different colors
#create a color vector corresponding to levels in the T1 variable in dat
useColors<-c("Coal"="#8DD3C7","Geothermal"="#FFFFB3","Hydro"="#BEBADA","Natural Gas"="#FB8072","Nuclear"="#80B1D3","Petroleum"="#FDB462","Solar"="#B3DE69","Total"="#FCCDE5","Wind"="#D9D9D9","Wood"="#BC80BD")

#create the sidecar menu for dashboard
sidebar <-dashboardSidebar (disable = FALSE, collapsed = FALSE,
                            
                            sidebarMenu(
                              menuItem("Summary", tabName = "summary", icon =NULL),
                              menuItem("About", tabName = "about", icon = NULL),
                              menuItem("Interesting Stuff", tabName = "trends", icon = NULL),
                              #menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                              #menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                              menuItem("Comparison", tabName = "comparison", icon =icon("dashboard"))
                            )
)#dashboardsidebarend

body<-dashboardBody(
  tabItems( 
    #first tab is the about page
    tabItem(tabName = "about",
            box(
              width = 4,
              background = "light-blue",
              p("The data for this app is from https://www.eia.gov/electricity/data/state/. This app
                has been created by Rabia Ibtasar. The last version was updated on Saturday, 13 February
                2021.")
            )
            # h2("About")
    ),
    #second Tab Item
    tabItem(tabName = "trends",
            fluidRow(
              
              radioButtons("trendsButton", "Interesting Comparisons:",
                           c("Coal vs Solar" = "trend1",
                             "Wind Vs Solar" = "trend2",
                             "Nuclear Vs Coal" = "trend3",
                             "Wind Vs Solar" = "trend4",
                             "Wind Vs Nuclear"='trend5')),
              
              box(title = "A: Amount of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                  plotOutput("t1", height = "300px", width="300px"),
                  plotOutput("t2", height = "150px", width="150px"),
                  plotOutput("t3", height = "150px", width="150px"),
                  plotOutput("t4", height = "150px", width="150px"),
                  
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Amount of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsA1", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Percent of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsA2", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Amount of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsB1", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Percent of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsB2", height = "150px", width="150px"))
                     )
              )
              
            ),
            fluidRow(
              column(3,
                     fluidRow(
                       box(title = "A: Graph of Energy for source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsA3", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: % Graph of Energy source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsA4", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Graph of Energy for source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsB3", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B:%Graph of Energy for source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsB4", height = "150px", width="150px"))
                     )
              )
            ),
            fluidRow(
              column(3,
                     fluidRow(
                       box(title = "A: Graph of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsA5", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Graph of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsA6", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Graph of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsB5", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Graph of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsB6", height = "150px", width="150px"))
                     )
              )
              
            ),
            fluidRow(
              column(3,
                     fluidRow(
                       box(title = "A: Map of Energy for source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsA7", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: % Map of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsA8", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Map of Energy per source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsB7", height = "150px", width="150px"))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B:% Map of Energy for source", solidHeader = TRUE, status = "primary", width =20,height=20,
                           plotOutput("trendsB8", height = "150px", width="150px"))
                     )
              )
              
            )#fluidrow end
    ),
    
    #third tab opens up the summary page for all the energy sources in the US
    tabItem(tabName = "summary",
            fluidRow(
              column(6,
                     fluidRow(
                       box(title = "Amount of each energy source from per year 1990-2019", solidHeader = TRUE, status = "primary", width =30,
                           plotOutput("summ1", height = 300))
                     )
              ),
              column(6,
                     fluidRow(
                       box(title = "Percent of each energy source per year from 1990-2019", solidHeader = TRUE, status = "primary", width = 30,
                           plotOutput("summ2", height = 300))
                     )
              ),
            ),#fluidrow end
            fluidRow(
              column(6,
                     fluidRow(
                       box(title = "Amount line graph of each energy source per year 1990-2019", solidHeader = TRUE, status = "primary", width =30,
                           plotOutput("summ3", height = 300))
                     )
              ),
              column(6,
                     fluidRow(
                       box(title = "Percent line graph of each energy source per year from 1990-2019", solidHeader = TRUE, status = "primary", width = 30,
                           plotOutput("summ4", height = 300))
                     )
              ),
            ),
            fluidRow(
              column(6,
                     fluidRow(
                       box(title = "Amount of energy source ", solidHeader = TRUE, status = "primary", width =30,
                           DT::dataTableOutput("summ5",height = 200))
                     )
              ),
              column(6,
                     fluidRow(
                       box(title = "Percent of energy source", solidHeader = TRUE, status = "primary", width = 30,
                           DT::dataTableOutput("summ6", height = 200))
                     )
              ),
            )
            
    ),#end of Summary TAB ui code
    
    #the fourth tab for comparison that uses the menu buttons
    tabItem(tabName = "comparison",
            fluidRow(
              column(6,            
                     selectInput("yearA", "Panel A: YEAR", years, selected = 2019,width="200px"),
                     selectInput("stateA", "Panel A:STATE", stateList, selected = "IL",width="200px"),
                     selectInput("sourceA", "Panel A:SOURCE", sourceList, selected = "Coal",width="150px"),
                     #menuItem("", tabName = "cheapBlankSpace", icon = NULL)
              ),
              column(6,
                     #menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     selectInput("yearB", "Panel B: YEAR ", years, selected = 2019,width="200px"),
                     selectInput("stateB", "Panel B:STATE", stateList, selected = "Total",width="200px"),
                     selectInput("sourceB", "Panel B:SOURCE", sourceList, selected = "Coal",width="250px")
              )
              
            ),
            fluidRow(
              
              
              column(3,
                     fluidRow(
                       box(title = "A: Amount of Energy per source", solidHeader = TRUE, status = "primary", width =30,
                           plotOutput("plotA1", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Percent of Energy per source", solidHeader = TRUE, status = "primary", width = 30,
                           plotOutput("plotA2", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Amount of Energy per source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB1", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Percent of Energy per source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB2", height = 300))
                     )
              )
              
            ),
            fluidRow(
              column(3,
                     fluidRow(
                       box(title = "A: Amount Graph of Energy for source", solidHeader = TRUE, status = "primary", width = 30,
                           plotOutput("plot3", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Percent Graph of Energy for source", solidHeader = TRUE, status = "primary", width =30,
                           plotOutput("plot4", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Amount of Energy for source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB3", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Percent of Energy for source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB4", height = 300))
                     )
              )
            ),
            fluidRow(
              column(3,
                     fluidRow(
                       box(title = "A: Amount of Energy for source", solidHeader = TRUE, status = "primary", width = 30,
                           dataTableOutput("plotA5", height = 200))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Percent of Energy for source", solidHeader = TRUE, status = "primary", width = 30,
                           dataTableOutput("plotA6", height = 200))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Amount of Energy per source", solidHeader = TRUE, status = "info", width = 30,
                           dataTableOutput("plotB5", height = 200))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Percent of Energy per source", solidHeader = TRUE, status = "info", width = 30,
                           dataTableOutput("plotB6", height = 200))
                     )
              )
              
            ),
            fluidRow(
              column(3,
                     fluidRow(
                       box(title = "A: Amount of Energy for source", solidHeader = TRUE, status = "primary", width = 30,
                           plotOutput("plotA7", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Percent of Energy for source", solidHeader = TRUE, status = "primary", width = 30,
                           plotOutput("plotA8", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Amount of Energy per source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB7", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Percent of Energy for source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB8", height = 300))
                     )
              )
              
            )#fluidrow end
            
    )#tabitem end
  )#tabitems end
)#dashboard body end


# Create the shiny dashboard
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "CS 424 Project 1 Comparing Energy sources in the US"), sidebar, body
)

server <- function(input, output) {
  
  # increase the default font size and create theme for blank backgrounds for plots and legends at the bottom
  theme_set(theme_grey(base_size = 11)) 
  theme_bare<- theme(panel.background=element_blank(),
                     legend.direction = "horizontal", legend.position = "bottom"
  )
  
  # summ1 output for rendering
  output$summ1 <- renderPlot({
    ggplot(data=sourceData)+ geom_bar(mapping=aes(x=YEAR,y=GENERATION,fill=SOURCE),stat="identity")+
      ggtitle("Total Amount of each energy source per year from 1990-2019")+ylab("Energy Amount (MWh)")+ theme_bare+
      scale_fill_manual(values=useColors)
  })
  
  #summ2 output for rendering
  output$summ2 <- renderPlot({
    #stacked bar charts showing the percentage of each energy source per year from 1990 - 2019
    ggplot(data=sourceData)+ geom_bar(mapping=aes(x=YEAR,fill=SOURCE),position="fill")+
      ggtitle("Percent of each energy source from 1990-2019")+ylab ('Percent')+
      scale_y_continuous(labels = scales::percent)+theme_bare+ scale_fill_manual(values=useColors)
  })
  
  # summ3 line graph of total amount of each energy source
  output$summ3 <- renderPlot({
    ggplot(data=sourceData,aes(x=YEAR,y=GENERATION))+ 
      stat_summary(aes(group=SOURCE,color=SOURCE),fun=sum, geom="line") +
      scale_y_continuous(name="Energy Amount")+
      theme_bare+ scale_color_manual(values=useColors)
  })
  
  #summ4 line graph of percent of each energy source
  output$summ4 <- renderPlot({
    
    s4<-subset(percentsData, percentsData$SOURCE !="Total" & percentsData$STATE=="US-TOTAL")
    
    ggplot(data=s4,aes(x=YEAR,y=PERCENTS,group=SOURCE, color=SOURCE))+ geom_line()+
      scale_y_continuous(name = "Percents")+
      theme_bare+ scale_color_manual(values=useColors)
  })
  # summ5 is the table of raw numbers 
  output$summ5 <- DT::renderDataTable({
    DT::datatable(displayAmounts, rownames=FALSE)
  })
  
  #summ6 is percents table
  output$summ6 <- DT::renderDataTable({
    displayPercents 
  })
  
  
  # calculate the values one time and re-use them in multiple charts to speed things up
  #amtReactive<- reactive ({
  # subset(energy_file,STATE==input$stateA & YEAR==input$yearA 
  #       & SOURCE==input$sourceA)
  #})
  
  
  # For the COMPARISON TAB : plotA1 output for rendering
  output$plotA1 <- renderPlot({
    
    t1<-subset(energy_file, energy_file$SOURCE==input$sourceA & energy_file$STATE==input$stateA)
    ggplot(data=t1)+ geom_bar(mapping=aes(x=input$yearA,fill=input$sourceA))+
      ggtitle("Amount of energy source for year")+ylab("Energy Amount (MWh)")+
      xlab("Year") + theme_bare + 
      scale_fill_manual(name=input$SOURCE,values=useColors)
    
  })
  
  output$plotA2 <- renderPlot({
    t2<-subset(energy_file, energy_file$SOURCE==input$sourceA & energy_file$STATE==input$stateA)
    ggplot(data=t2)+ geom_bar(mapping=aes(x=input$yearA,fill=input$sourceA),position="fill")+
      ggtitle("Percent of energy source for year")+ylab ('Percent')+  xlab("Year")+
      scale_y_continuous(labels = scales::percent)+theme_bare+ 
      scale_fill_manual(name=input$SOURCE, values=useColors)
  })
  
  #line chart for energy amount
  output$plotA3 <- renderPlot({
    t3<-subset(amountsData, amountsData$SOURCE==input$sourceA & amountsData$STATE==input$stateA)
    ggplot(data=t3,aes(x=YEAR,y=GENERATION))+ 
      stat_summary(aes(group=input$SOURCE,color=input$SOURCE),fun=sum, geom="line") +
      scale_y_continuous(name="Energy Amount")+
      theme_bare+ scale_color_manual(values=useColors)
  })
  
  #line chart for percent NOT DONE
  output$plotA4 <- renderPlot({
    t4<-subset(energy_file, energy_file$SOURCE==input$sourceA & energy_file$STATE==input$stateA)
    ggplot(data=t4,aes(x=YEAR,y=GENERATION))+ 
      stat_summary(aes(group=input$SOURCE,color=input$SOURCE),fun=sum, geom="line") +
      scale_y_continuous(name="Energy Amount")+
      theme_bare+ scale_color_manual(values=useColors)
  })
  
  #table amount
  output$plotA5 <- DT::renderDataTable({
    DT::datatable(displayAmounts, rownames=FALSE)
  })
  
  #table percent
  output$plotA6 <- DT::renderDataTable({
    DT::datatable(displayAmounts, rownames=FALSE)
  })
  
  #heatmap of the US for amount
  output$plotA7 <- renderPlot({
    map7<-subset(amountsData, amountsData$SOURCE==input$sourceA & amountsData$STATE==input$stateA)
    map7<-select(map7,STATE,GENERATION)
    map7<-rename(map7,fips=STATE)
    map7$fips<-as.character(map7$fips)
    map7$fips<-fips(map7$fips)
    
    plot_usmap(data=map7,values="GENERATION")+
      labs(title = "US States",subtitle = "US Map for Energy Amounts.") + 
      theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    
  })
  #heatmap of the US for percent needs DF with two columns(FIPS,Values)
  output$plotA8 <- renderPlot({
    map8<-subset(percentsData, percentsData$SOURCE==input$sourceA & percentsData$STATE==input$stateA)
    map8<-select(map8,STATE,PERCENTS)
    map8<-rename(map8,fips=STATE)
    map8$fips<-as.character(map8$fips)
    map8$fips<-fips(map8$fips)
    
    plot_usmap(data=map8,values="PERCENTS")+
      labs(title = "US States",subtitle = "US Map for Percents.") + 
      theme(panel.background = element_rect(color = "black", fill = "lightblue"))
  })
  
  #Rendering for Panel B
  
  output$plotB1 <- renderPlot({
    b1<-subset(energy_file, energy_file$SOURCE==input$sourceB & energy_file$STATE==input$stateB)
    ggplot(data=b1)+ geom_bar(mapping=aes(x=input$yearB,fill=input$sourceB))+
      ggtitle("Amount of energy source for year")+ylab("Energy Amount (MWh)")+
      xlab("Year") + theme_bare + 
      scale_fill_manual(name=input$SOURCE,values=useColors)
  })
  
  output$plotB2 <- renderPlot({
    b2<-subset(energy_file, energy_file$SOURCE==input$sourceB & energy_file$STATE==input$stateB)
    ggplot(data=b2)+ geom_bar(mapping=aes(x=input$yearB,fill=input$sourceB),position="fill")+
      ggtitle("Percent of energy source for year")+ylab ('Percent')+  xlab("Year")+
      scale_y_continuous(labels = scales::percent)+theme_bare+ 
      scale_fill_manual(name=input$SOURCE, values=useColors)
  })
  
  #line chart amount
  output$plotB3 <- renderPlot({
    b3<-subset(energy_file, energy_file$SOURCE==input$sourceB & energy_file$STATE==input$stateB)
    ggplot(data=b3,aes(x=YEAR,y=GENERATION))+ 
      stat_summary(aes(group=input$SOURCE,color=input$SOURCE),fun=sum, geom="line") +
      scale_y_continuous(name="Energy Amount")+
      theme_bare+ scale_color_manual(values=useColors)
  })
  
  #line chart percent NOT DONE YET 
  output$plotB4 <- renderPlot({
    b4<-subset(energy_file, energy_file$SOURCE==input$sourceB & energy_file$STATE==input$stateB)
    ggplot(data=b4,aes(x=YEAR,y=GENERATION))+ 
      stat_summary(aes(group=input$SOURCE),fun=sum, geom="line") +
      scale_y_continuous(name="Percent")+
      theme_bare+ scale_color_manual(values=useColors)
  })
  
  #table amount
  output$plotB5 <- DT::renderDataTable({
    displayAmounts
  })
  
  
  #table percent
  output$plotB6 <- DT::renderDataTable({
    displayPercents
  })
  
  
  #heatmap of the US for amounnt
  output$plotB7 <- renderPlot({
    bmap7<-subset(amountsData, amountsData$SOURCE==input$sourceB & amountsData$STATE==input$stateB)
    bmap7<-select(bmap7,STATE,GENERATION)
    bmap7<-rename(bmap7,fips=STATE)
    bmap7$fips<-as.character(bmap7$fips)
    bmap7$fips<-fips(bmap7$fips)
    
    plot_usmap(data=bmap7,values="GENERATION")+
      labs(title = "US States",subtitle = "US Map for Energy Amounts.") + 
      theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    
    
  })
  #heatmap of the US for percent needs DF with two colums(FIPS,Values)
  output$plotB8 <- renderPlot({
    bmap8<-subset(percentsData, percentsData$SOURCE==input$sourceB & percentsData$STATE==input$stateB)
    bmap8<-select(bmap8,STATE,PERCENTS)
    bmap8<-rename(bmap8,fips=STATE)
    bmap8$fips<-as.character(bmap8$fips)
    bmap8$fips<-fips(bmap8$fips)
    
    plot_usmap(data=bmap8,values="PERCENTS")+
      labs(title = "US States",subtitle = "US Map for Percents.") + 
      theme(panel.background = element_rect(color = "black", fill = "lightblue"))
    
  })
  
  
  #for the trends TAB
  output$plottrendsA1 <- renderPlot({
    
    ggplot(data=energy_file)+ geom_bar(mapping=aes(x=YEAR,fill=SOURCE))+
      ggtitle("Amount of energy source for year")+ylab("Energy Amount (MWh)")+
      xlab("Year") + theme_bare + 
      scale_fill_manual(name=SOURCE,values=useColors)
    
  })
 
  
}#server end


shinyApp(ui = ui, server = server)
