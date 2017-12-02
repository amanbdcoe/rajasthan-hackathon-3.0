library(shiny)
library(shinydashboard)
library(tseries)
library(reshape)
library(dplyr)
library(forecast)
library(plyr)
#croprot<-read.csv("croprotation.csv",stringsAsFactors = F)
read<-read.csv("rainfall.csv",stringsAsFactors = FALSE)
read<-read[1:36,2]
read<-as.character(read)
crop_data<-read.csv("rainmonth.csv",stringsAsFactors = F)
crop_name<-as.character(crop_data$crop)
ui<-dashboardPage(skin = "purple", 
                  
                  
                  dashboardHeader(title="Rainfall Prediction"),
                  
                  
                  dashboardSidebar(
                    
                    sidebarMenu(
                      id="sidebarmenu",
                      #selectInput(inputId="",label=""),##for language        
                      menuItem(h5("Home"),tabName="nodata",icon=icon("home","fa-3x",lib="glyphicon")),        
                      menuItem(h5("Plot/Data"),tabName = "Plot",icon=icon("line-chart","fa-3x")),
                      conditionalPanel("input.sidebarmenu==='Plot'",
                                       selectInput(inputId="region",label = "Select Region",read,selected="WEST UTTAR PRADESH"),
                                       selectInput(inputId="year",label="Select Year",2010:2014)
                                       
                      ),
                      menuItem(h5("Crop Cultivation"),tabName ="CSeason",icon=icon("grain","fa-3x",lib="glyphicon")),
                      conditionalPanel("input.sidebarmenu==='CSeason'",
                                       selectInput(inputId="regioncrop",label="Select Region",c("WEST UTTAR PRADESH")),
                                       selectInput(inputId = "seasoncrop",label="Select Season",c("SUMMER(MAY:OCT)","SPRING(JAN:APR)","WINTER(NOV:DEC)")),
                                       uiOutput("crop")
                      ),
                      
                      menuItem(h5("FAQ's"),tabName="faq",icon=icon("cog","fa-3x",lib="glyphicon")),
                      menuItem(h5("Queries"),tabName="Questions",icon=icon("search","fa-3x",lib="glyphicon"))
                    )
                  ),
                  dashboardBody(
                    tabItems(
                      tabItem(tabName = "Plot",fluidRow(plotOutput("RainPlot"),title = "Predicted Plot"),fluidRow(box(tableOutput("RainData"),title = "Predicted rainfall data"))),
                      tabItem(tabName = "nodata",fluidPage(h1("A HYBRID MODEL FOR RAINFALL PREDICTION AND FARMER SUPPORT SYSTEM",align="center"),br(),h4("This model uses MACHINE LEARNING and ARTIFICIAL INTELLIGENCE for the prediction of rainfall patterns. Various factors which have crucial effect on the rainfall pattern are also included to form the most accurate model possible ,which can be used to predict the rainfall pattern ."),br(),h4("It is user friendly system which educate the farmers about the forecasted weather and various other vital factors about their crop."),br(),h4("In case of any query,farmers can simply write their question in the designated space and the same will be replied by any expert  "))),
                      #tabItem(tabName="SCrops",fluidPage(tableOutput("cseason"))),
                      tabItem(tabName="CSeason",fluidPage(tableOutput("scrop"))),
                      
                      
                      #tabItem(tabName ="Query",fluidPage(textAreaInput(inputId ="query",label=h1('Enter Your Query'),width ="500px",rows="10",resize = NULL)))
                      tabItem(tabName = "faq",fluidPage(h4(" Whether Government gives subsidy on freight?"),p("The Union Government subsidies the urea manufacturing units for the cost of transportation to facilitate the availability of urea at the same maximum retail price all over the country."),br(),
                                                        h4(" How quality control is ensured for fertilizers?"),p(" The FCO has laid down fertilizer-wise detailed specifications and no fertilizer, not meeting the said specification, can be sold in the country for agricultural purposes. It also lays down detailed procedure for sampling and analysis of each fertilizer. "),br(),h4(" Why subsidy is not given directly to farmers?"),p(" The main objective of the Government is to make available fertilizers at reasonable/affordable prices for agricultural purposes including farmers. This objective is achieved through fixation of statutorily backed uniform MRP for urea and indicative MRP for fertilizers covered under the Concession Scheme on sale of decontrolled P&K fertilizers. "),br(),
                                                        h4("What is the selling price of fertilizers?"),p("Urea is the only fertilizer with statutorily controlled price & movement. the Concession Scheme on sale of phosphatic & potassic (P&K) fertilizers provides for indicative Maximum Retail Price declared by Department of Fertilizers at which these fertilizers are to be sold. "))),
                      
                      
                      tabItem(tabName = "Questions",fluidPage(h1("Enter Your Query and we will be contacting you soon..."),br(),textAreaInput(inputId ="Quest",label=NULL,width ="500px",rows="10",
                                                                                                                                              resize = NULL)),column(4,textInput("Mob","Please Enter Your Mobile No")),
                              br(),column(4,actionButton("go","Submit!!!")),br(),textOutput("Ques"),br(),uiOutput("Result")
                      )
                      
                    )
                    
                  )
)