


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
sourceData<-subset(energy_file, energy_file$SOURCE!="Total")
amountsData<-energy_file %>% group_by(YEAR,SOURCE) %>% summarize(Amount=sum(GENERATION))
percentsData<-sourceData %>% count(YEAR,SOURCE) %>% group_by(YEAR) %>% mutate(percent=(n/sum(n))*100) %>% ungroup()

#create menu items to select different years, states, and sources
years<-c(1990:2019)
sourceList<-as.list(levels(sourceData$SOURCE))
stateList<-as.list(levels(sourceData$STATE))


#create color vector to use color blind safe colors and consistency for plotting the types of energy sources
#colors<-c('Coal'='#a50026','Geothermal'='#d73027','Hydro'='#f46d43','Natural Gas'='#fdae61','Nuclear'='#fee090','Petroleum'='#e0f3f8','Solar'='#abd9e9',Total='#313695','Wind'='#74add1','Wood'='#4575b4')
#scale_color_manual(values=colors)

#create the sidecar menu for dashboard
sidebar <-dashboardSidebar (disable = FALSE, collapsed = FALSE,
                           
                           sidebarMenu(
                             menuItem("Summary", tabName = "summary", icon =NULL),
                             menuItem("About", tabName = "about", icon = NULL),
                             menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                             menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                             menuItem("Comparison", tabName = "comparison", icon =icon("dashboard")),
                             selectInput("yearA", "Select the year to visualize in Panel A", years, selected = 2019),
                             selectInput("stateA", "Select the year to visualize in Panel A", stateList, selected = "IL"),
                             selectInput("sourceA", "Select the source to visualize in Panel A", sourceList, selected = "Coal"),
                             selectInput("yearB", "Select the year to visualize in Panel B", years, selected = 2019),
                             selectInput("stateB", "Select the year to visualize in Panel B", stateList, selected = "IL"),
                             selectInput("sourceB", "Select the source to visualize in Panel B", sourceList, selected = "Coal")
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
    #second tab opens up the summary page for all the energy sources in theUS
    tabItem(tabName = "summary",
            fluidRow(
              column(6,
                     fluidRow(
                       box(title = "Amount of each energy source from per year 1990-2019", solidHeader = TRUE, status = "primary", width =40,
                           plotOutput("summ1", height = 300))
                     )
              ),
              column(6,
                     fluidRow(
                       box(title = "Percent of each energy source per year from 1990-2019", solidHeader = TRUE, status = "primary", width = 40,
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
                       box(title = "Amount of each energy source per year from 1990-2019", solidHeader = TRUE, status = "primary", width =30,
                           dataTableOutput("summ5", height = 200))
                     )
              ),
              column(6,
                     fluidRow(
                       box(title = "Percent of each energy source per year from 1990-2019", solidHeader = TRUE, status = "primary", width = 30,
                           dataTableOutput("summ6", height = 200))
                     )
              ),
            )
    ),#end of Summary TAB ui code
    
   #the third tab for comparison that uses the menu buttons
    tabItem(tabName = "comparison",
            fluidRow(
              column(3,
                     fluidRow(
                       box(title = "A: Amount of Energy per source", solidHeader = TRUE, status = "primary", width =30,
                           plotOutput("plotA1", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Percentage of Energy per source", solidHeader = TRUE, status = "primary", width = 30,
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
                       box(title = "B: Percentage of Energy per source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB2", height = 300))
                     )
              )
              
            ),
            fluidRow(
              column(3,
                     fluidRow(
                       box(title = "A: Line grapgh for Amount of Energy per source", solidHeader = TRUE, status = "primary", width = 30,
                           plotOutput("plotA3", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Line grapgh for percentage of Energy per source", solidHeader = TRUE, status = "primary", width =30,
                           plotOutput("plotA4", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Amount of Energy per source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB3", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Percentage of Energy per source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB4", height = 300))
                     )
              )
            ),
            fluidRow(
              column(3,
                     fluidRow(
                       box(title = "A: Amount of Energy per source", solidHeader = TRUE, status = "primary", width = 30,
                           plotOutput("plotA5", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Percentage of Energy per source", solidHeader = TRUE, status = "primary", width = 30,
                           plotOutput("plotA6", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Amount of Energy per source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB5", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "B: Percentage of Energy per source", solidHeader = TRUE, status = "info", width = 30,
                           plotOutput("plotB6", height = 300))
                     )
              )
              
            ),
            fluidRow(
              column(3,
                     fluidRow(
                       box(title = "A: Amount of Energy per source", solidHeader = TRUE, status = "primary", width = 30,
                           plotOutput("plotA7", height = 300))
                     )
              ),
              column(3,
                     fluidRow(
                       box(title = "A: Percentage of Energy per source", solidHeader = TRUE, status = "primary", width = 30,
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
                       box(title = "B: Percentage of Energy per source", solidHeader = TRUE, status = "info", width = 30,
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
  theme_set(theme_grey(base_size = 12)) 
  theme_bare<- theme(panel.background=element_blank(),
                     panel.grid=element_blank(),legend.direction = "horizontal", legend.position = "bottom"
  )
 
  # summ1 output for rendering
  output$summ1 <- renderPlot({
    ggplot(data=sourceData)+ geom_bar(mapping=aes(x=YEAR,y=GENERATION,fill=SOURCE),stat="identity")+
    ggtitle("Total Amount of each energy source per year from 1990-2019")+ylab("Energy Amount (MWh)")+ theme_bare
  })
  
  #summ2 output for rendering
  output$summ2 <- renderPlot({
    #stacked bar charts showing the percentage of each energy source per year from 1990 - 2019
    ggplot(data=sourceData)+ geom_bar(mapping=aes(x=YEAR,fill=SOURCE),position="fill")+
      ggtitle("Percent of each energy source from 1990-2019")+ylab ('Percent')+
      scale_y_continuous(labels = scales::percent)+theme_bare
  })
  # summ3 line graph of total amount of each energy source
  output$summ3 <- renderPlot({
    ggplot(data=sourceData,aes(x=YEAR,y=GENERATION))+ 
      stat_summary(aes(group=SOURCE,color=SOURCE),fun=sum, geom="line") +
      scale_y_continuous(name="Energy Amount")+
      theme_bare
  })
  
  #summ4 line graph of percent of each energy source
  output$summ4 <- renderPlot({
    ggplot(data=percentsData,aes(x=YEAR,y=percent))+
      stat_summary(aes(group=SOURCE,color=SOURCE),fun=sum, geom="line") +
      scale_y_continuous(name="Energy Amount")+
      theme_bare
  })
  # summ5 is the table of raw numbers 
  output$summ5 <- DT::renderDataTable({
   amountsData
    })
  
  #summ6 is percents table
  output$summ6 <- DT::renderDataTable({
    percentsData
  })
  
  
  # calculate the values one time and re-use them in multiple charts to speed things up
    amtReactive<- reactive ({
    subset(energy_file,energy_file$STATE==input$stateA & energy_file$YEAR==input$yearA 
           & energy_file$SOURCE==input$sourceA)
    })
    
  
  # For the COMPARISON TAB : plotA1 output for rendering
  output$plotA1 <- renderPlot({
    a<-amtReactive()
    ggplot(data=a)+ geom_bar(mapping=aes(x=input$yearA,y=a[,input$sourceA]),stat="identity")+
    ggtitle("Total Amount of energy source for selected year")+ylab("Energy Amount (MWh)")+ theme_bare
    
  })
  
  output$plotA2 <- renderPlot({
    ggplot(data=sourceData)+ geom_bar(mapping=aes(x=input$yearA,y=GENERATION,fill=input$sourceA),stat="identity")+
      ggtitle("Total Amount of each energy source per year from 1990-2019")+ylab("Energy Amount (MWh)")+ theme_bare
    
  })
  
 # output$plotA3 <- renderPlot({
  #  a<-amtReactive()
   # ggplot(data=a,aes(x=input$YEAR,y=a[,input$sourceA]))+ 
    #  stat_summary(aes(group=input$SOURCE,color=SOURCE),fun=sum, geom="line") +
     # scale_y_continuous(name="Energy Amount")+
      #theme_bare
#  })
  
  output$plotA4 <- renderPlot({
    ggplot(data=sourceData)+ geom_bar(mapping=aes(x=input$yearA,y=GENERATION,fill=input$sourceA),stat="identity")+
      ggtitle("Total Amount of each energy source per year from 1990-2019")+ylab("Energy Amount (MWh)")+ theme_bare
    
  })
  output$plotA5 <- renderPlot({
    ggplot(data=sourceData)+ geom_bar(mapping=aes(x=input$yearA,y=GENERATION,fill=input$sourceA),stat="identity")+
      ggtitle("Total Amount of each energy source per year from 1990-2019")+ylab("Energy Amount (MWh)")+ theme_bare
    
  })
  output$plotA6 <- renderPlot({
    ggplot(data=sourceData)+ geom_bar(mapping=aes(x=input$yearA,y=GENERATION,fill=input$sourceA),stat="identity")+
      ggtitle("Total Amount of each energy source per year from 1990-2019")+ylab("Energy Amount (MWh)")+ theme_bare
    
  })
  
  #heatmap of the US for amounnt
  output$plotA7 <- renderPlot({
    ggplot(data=sourceData)+ geom_bar(mapping=aes(x=input$yearA,y=GENERATION,fill=input$sourceA),stat="identity")+
      ggtitle("Total Amount of each energy source per year from 1990-2019")+ylab("Energy Amount (MWh)")+ theme_bare
    
  })
  #heatmap of the US for percentage
  output$plotA8 <- renderPlot({
    ggplot(data=sourceData)+ geom_bar(mapping=aes(x=input$yearA,y=GENERATION,fill=input$sourceA),stat="identity")+
      ggtitle("Total Amount of each energy source per year from 1990-2019")+ylab("Energy Amount (MWh)")+ theme_bare
    
  })
  
  #Rendering for Panel B
  
  
  
  
}#server end


shinyApp(ui = ui, server = server)

