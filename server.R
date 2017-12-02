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

server<-function(input, output){
  
  data<-read.csv("rainfall.csv",stringsAsFactors = FALSE)
  output$RainPlot<-renderPlot({
    data<-filter(data,SD_Name==input$region)
    
    ####   JAN-APR
    
    testjan<-filter(data,YEAR<=input$year)
    trainjan<-filter(data,YEAR<(input$year))
    trainjan<-select(trainjan,YEAR,JAN:APR)
    testjan<-select(testjan,YEAR,JAN:APR)
    trainjan<-melt(trainjan,c("YEAR"))
    testjan<-melt(testjan,c("YEAR"))
    trainjan<-arrange(trainjan,YEAR)
    testjan<-arrange(testjan,YEAR)
    trainjan<-ts(trainjan$value,frequency=4)
    testjan<-ts(testjan$value,frequency=4)
    #output$RainPlot<-renderPlot(plot(decompose(trainjan)))
    jantrend<-decompose(trainjan)$trend
    #jantrendtest<-decompose(testjan)$trend
    janseasonal<-decompose(trainjan)$seasonal
    janrandom<-decompose(trainjan)$random
    jantrend<-na.omit(jantrend)
    #jantrendtest<-na.omit(jantrendtest)
    janrandom<-na.omit(janrandom)
    jantrend<-ts(jantrend,frequency=4)
    #jantrendtest<-ts(jantrendtest,frequency = 4)
    janseasonal<-ts(janseasonal,frequency=4)
    janrandom<-ts(janrandom,frequency = 4)
    #output$RainPlot<-renderPlot(plot(janrandom))
    modjantrend<-auto.arima(jantrend)
    modjanrandom<-auto.arima(janrandom)
    #modjantrend<-Arima(jantrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
    #modjanrandom<-Arima(janrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
    
    forjantrend<-forecast(modjantrend,h=4)
    forjanrandom<-forecast(modjanrandom,h=4)
    forjantrend<-as.data.frame(forjantrend)
    janseas<-head(janseasonal,4)
    janseas<-as.data.frame(janseas)
    forjanrandom<-as.data.frame(forjanrandom)
    janseas<-as.data.frame(janseas)
    
    janfinal<-janseas$x+forjantrend$`Point Forecast`+forjanrandom$`Point Forecast`
    #output$RainPlot<-renderPlot(plot(janfinal))
    
    
    testjan1<-filter(data,YEAR==input$year)
    testjan1<-filter(testjan1,SD_Name==input$region)
    testjan1<-select(testjan1,YEAR,JAN:APR)
    testjan1<-melt(testjan1,c("YEAR"))
    testjan1<-ts(testjan1$value,frequency=4)
    janfinal<-ifelse(janfinal>0,janfinal,0)
    
    #MAY-OCT
    
    testmay<-filter(data,YEAR<=input$year)
    trainmay<-filter(data,YEAR<(input$year))
    trainmay<-select(trainmay,YEAR,MAY:OCT)
    testmay<-select(testmay,YEAR,MAY:OCT)
    trainmay<-melt(trainmay,c("YEAR"))
    testmay<-melt(testmay,c("YEAR"))
    trainmay<-arrange(trainmay,YEAR)
    testmay<-arrange(testmay,YEAR)
    trainmay<-ts(trainmay$value,frequency=6)
    testmay<-ts(testmay$value,frequency=6)
    #output$RainPlot<-renderPlot(plot(decompose(trainmay)))
    maytrend<-decompose(trainmay)$trend
    #maytrendtest<-decompose(testmay)$trend
    mayseasonal<-decompose(trainmay)$seasonal
    mayrandom<-decompose(trainmay)$random
    maytrend<-na.omit(maytrend)
    #maytrendtest<-na.omit(maytrendtest)
    mayrandom<-na.omit(mayrandom)
    maytrend<-ts(maytrend,frequency=6)
    #maytrendtest<-ts(maytrendtest,frequency = 4)
    mayseasonal<-ts(mayseasonal,frequency=6)
    mayrandom<-ts(mayrandom,frequency = 6)
    #output$RainPlot<-renderPlot(plot(mayrandom))
    modmaytrend<-auto.arima(maytrend)
    modmayrandom<-auto.arima(mayrandom)
    #modmaytrend<-Arima(maytrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
    #modmayrandom<-Arima(mayrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
    
    formaytrend<-forecast(modmaytrend,h=6)
    formayrandom<-forecast(modmayrandom,h=6)
    formaytrend<-as.data.frame(formaytrend)
    mayseas<-head(mayseasonal,6)
    mayseas<-as.data.frame(mayseas)
    formayrandom<-as.data.frame(formayrandom)
    mayseas<-as.data.frame(mayseas)
    
    mayfinal<-mayseas$x+formaytrend$`Point Forecast`+formayrandom$`Point Forecast`
    #output$RainPlot<-renderPlot(plot(mayfinal))
    
    
    testmay1<-filter(data,YEAR==input$year)
    testmay1<-filter(testmay1,SD_Name==input$region)
    testmay1<-select(testmay1,YEAR,MAY:OCT)
    testmay1<-melt(testmay1,c("YEAR"))
    testmay1<-ts(testmay1$value,frequency=4)
    mayfinal<-ifelse(mayfinal>0,mayfinal,0)
    
    ##NOV-DEC
    
    testnov<-filter(data,YEAR<=input$year)
    trainnov<-filter(data,YEAR<(input$year))
    trainnov<-select(trainnov,YEAR,MAY:OCT)
    testnov<-select(testnov,YEAR,MAY:OCT)
    trainnov<-melt(trainnov,c("YEAR"))
    testnov<-melt(testnov,c("YEAR"))
    trainnov<-arrange(trainnov,YEAR)
    testnov<-arrange(testnov,YEAR)
    trainnov<-ts(trainnov$value,frequency=2)
    testnov<-ts(testnov$value,frequency=2)
    #output$RainPlot<-renderPlot(plot(decompose(trainnov)))
    novtrend<-decompose(trainnov)$trend
    #novtrendtest<-decompose(testnov)$trend
    novseasonal<-decompose(trainnov)$seasonal
    novrandom<-decompose(trainnov)$random
    novtrend<-na.omit(novtrend)
    #novtrendtest<-na.omit(novtrendtest)
    novrandom<-na.omit(novrandom)
    novtrend<-ts(novtrend,frequency=2)
    #novtrendtest<-ts(novtrendtest,frequency = 4)
    novseasonal<-ts(novseasonal,frequency=2)
    novrandom<-ts(novrandom,frequency = 2)
    #output$RainPlot<-renderPlot(plot(novrandom))
    modnovtrend<-auto.arima(novtrend)
    modnovrandom<-auto.arima(novrandom)
    #modnovtrend<-Arima(novtrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
    #modnovrandom<-Arima(novrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
    
    fornovtrend<-forecast(modnovtrend,h=2)
    fornovrandom<-forecast(modnovrandom,h=2)
    fornovtrend<-as.data.frame(fornovtrend)
    novseas<-head(novseasonal,2)
    novseas<-as.data.frame(novseas)
    fornovrandom<-as.data.frame(fornovrandom)
    novseas<-as.data.frame(novseas)
    
    novfinal<-novseas$x+fornovtrend$`Point Forecast`+fornovrandom$`Point Forecast`
    #output$RainPlot<-renderPlot(plot(novfinal))
    
    
    testnov1<-filter(data,YEAR==input$year)
    testnov1<-filter(testnov1,SD_Name==input$region)
    testnov1<-select(testnov1,YEAR,MAY:OCT)
    testnov1<-melt(testnov1,c("YEAR"))
    testnov1<-ts(testnov1$value,frequency=4)
    novfinal<-ifelse(novfinal>0,novfinal,0)
    
    
    janfinal<-as.data.frame(janfinal)
    mayfinal<-as.data.frame(mayfinal)
    novfinal<-as.data.frame(novfinal)
    jan<-as.numeric(janfinal$x)
    jan<-as.data.frame(jan)
    jan<-rename(jan,c(jan="rain"))
    
    may<-as.numeric(mayfinal$x)
    may<-as.data.frame(may)
    may<-rename(may,c(may="rain"))
    
    nov<-as.numeric(novfinal$x)
    nov<-as.data.frame(nov)
    nov<-rename(nov,c(nov="rain"))
    final<-rbind(jan,may,nov)
    final<-ts(final$rain,frequency=1)
    plot(final,xlab="month",ylab = "rainfall(cm)")
  })
  output$RainData<-renderTable({data<-filter(data,SD_Name==input$region)
  
  ####   JAN-APR
  
  testjan<-filter(data,YEAR<=input$year)
  trainjan<-filter(data,YEAR<(input$year))
  trainjan<-select(trainjan,YEAR,JAN:APR)
  testjan<-select(testjan,YEAR,JAN:APR)
  trainjan<-melt(trainjan,c("YEAR"))
  testjan<-melt(testjan,c("YEAR"))
  trainjan<-arrange(trainjan,YEAR)
  testjan<-arrange(testjan,YEAR)
  trainjan<-ts(trainjan$value,frequency=4)
  testjan<-ts(testjan$value,frequency=4)
  #output$RainPlot<-renderPlot(plot(decompose(trainjan)))
  jantrend<-decompose(trainjan)$trend
  #jantrendtest<-decompose(testjan)$trend
  janseasonal<-decompose(trainjan)$seasonal
  janrandom<-decompose(trainjan)$random
  jantrend<-na.omit(jantrend)
  #jantrendtest<-na.omit(jantrendtest)
  janrandom<-na.omit(janrandom)
  jantrend<-ts(jantrend,frequency=4)
  #jantrendtest<-ts(jantrendtest,frequency = 4)
  janseasonal<-ts(janseasonal,frequency=4)
  janrandom<-ts(janrandom,frequency = 4)
  #output$RainPlot<-renderPlot(plot(janrandom))
  modjantrend<-auto.arima(jantrend)
  modjanrandom<-auto.arima(janrandom)
  #modjantrend<-Arima(jantrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  #modjanrandom<-Arima(janrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  
  forjantrend<-forecast(modjantrend,h=4)
  forjanrandom<-forecast(modjanrandom,h=4)
  forjantrend<-as.data.frame(forjantrend)
  janseas<-head(janseasonal,4)
  janseas<-as.data.frame(janseas)
  forjanrandom<-as.data.frame(forjanrandom)
  janseas<-as.data.frame(janseas)
  
  janfinal<-janseas$x+forjantrend$`Point Forecast`+forjanrandom$`Point Forecast`
  #output$RainPlot<-renderPlot(plot(janfinal))
  
  
  testjan1<-filter(data,YEAR==input$year)
  testjan1<-filter(testjan1,SD_Name==input$region)
  testjan1<-select(testjan1,YEAR,JAN:APR)
  testjan1<-melt(testjan1,c("YEAR"))
  testjan1<-ts(testjan1$value,frequency=4)
  janfinal<-ifelse(janfinal>0,janfinal,0)
  
  #MAY-OCT
  
  testmay<-filter(data,YEAR<=input$year)
  trainmay<-filter(data,YEAR<(input$year))
  trainmay<-select(trainmay,YEAR,MAY:OCT)
  testmay<-select(testmay,YEAR,MAY:OCT)
  trainmay<-melt(trainmay,c("YEAR"))
  testmay<-melt(testmay,c("YEAR"))
  trainmay<-arrange(trainmay,YEAR)
  testmay<-arrange(testmay,YEAR)
  trainmay<-ts(trainmay$value,frequency=6)
  testmay<-ts(testmay$value,frequency=6)
  #output$RainPlot<-renderPlot(plot(decompose(trainmay)))
  maytrend<-decompose(trainmay)$trend
  #maytrendtest<-decompose(testmay)$trend
  mayseasonal<-decompose(trainmay)$seasonal
  mayrandom<-decompose(trainmay)$random
  maytrend<-na.omit(maytrend)
  #maytrendtest<-na.omit(maytrendtest)
  mayrandom<-na.omit(mayrandom)
  maytrend<-ts(maytrend,frequency=6)
  #maytrendtest<-ts(maytrendtest,frequency = 4)
  mayseasonal<-ts(mayseasonal,frequency=6)
  mayrandom<-ts(mayrandom,frequency = 6)
  #output$RainPlot<-renderPlot(plot(mayrandom))
  modmaytrend<-auto.arima(maytrend)
  modmayrandom<-auto.arima(mayrandom)
  #modmaytrend<-Arima(maytrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  #modmayrandom<-Arima(mayrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  
  formaytrend<-forecast(modmaytrend,h=6)
  formayrandom<-forecast(modmayrandom,h=6)
  formaytrend<-as.data.frame(formaytrend)
  mayseas<-head(mayseasonal,6)
  mayseas<-as.data.frame(mayseas)
  formayrandom<-as.data.frame(formayrandom)
  mayseas<-as.data.frame(mayseas)
  
  mayfinal<-mayseas$x+formaytrend$`Point Forecast`+formayrandom$`Point Forecast`
  #output$RainPlot<-renderPlot(plot(mayfinal))
  
  
  testmay1<-filter(data,YEAR==input$year)
  testmay1<-filter(testmay1,SD_Name==input$region)
  testmay1<-select(testmay1,YEAR,MAY:OCT)
  testmay1<-melt(testmay1,c("YEAR"))
  testmay1<-ts(testmay1$value,frequency=4)
  mayfinal<-ifelse(mayfinal>0,mayfinal,0)
  
  ##NOV-DEC
  
  testnov<-filter(data,YEAR<=input$year)
  trainnov<-filter(data,YEAR<(input$year))
  trainnov<-select(trainnov,YEAR,MAY:OCT)
  testnov<-select(testnov,YEAR,MAY:OCT)
  trainnov<-melt(trainnov,c("YEAR"))
  testnov<-melt(testnov,c("YEAR"))
  trainnov<-arrange(trainnov,YEAR)
  testnov<-arrange(testnov,YEAR)
  trainnov<-ts(trainnov$value,frequency=2)
  testnov<-ts(testnov$value,frequency=2)
  #output$RainPlot<-renderPlot(plot(decompose(trainnov)))
  novtrend<-decompose(trainnov)$trend
  #novtrendtest<-decompose(testnov)$trend
  novseasonal<-decompose(trainnov)$seasonal
  novrandom<-decompose(trainnov)$random
  novtrend<-na.omit(novtrend)
  #novtrendtest<-na.omit(novtrendtest)
  novrandom<-na.omit(novrandom)
  novtrend<-ts(novtrend,frequency=2)
  #novtrendtest<-ts(novtrendtest,frequency = 4)
  novseasonal<-ts(novseasonal,frequency=2)
  novrandom<-ts(novrandom,frequency = 2)
  #output$RainPlot<-renderPlot(plot(novrandom))
  modnovtrend<-auto.arima(novtrend)
  modnovrandom<-auto.arima(novrandom)
  #modnovtrend<-Arima(novtrend,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  #modnovrandom<-Arima(novrandom,order=c(3,0,0),seasonal=list(order=c(2,0,0),period=4))
  
  fornovtrend<-forecast(modnovtrend,h=2)
  fornovrandom<-forecast(modnovrandom,h=2)
  fornovtrend<-as.data.frame(fornovtrend)
  novseas<-head(novseasonal,2)
  novseas<-as.data.frame(novseas)
  fornovrandom<-as.data.frame(fornovrandom)
  novseas<-as.data.frame(novseas)
  
  novfinal<-novseas$x+fornovtrend$`Point Forecast`+fornovrandom$`Point Forecast`
  #output$RainPlot<-renderPlot(plot(novfinal))
  
  
  testnov1<-filter(data,YEAR==input$year)
  testnov1<-filter(testnov1,SD_Name==input$region)
  testnov1<-select(testnov1,YEAR,MAY:OCT)
  testnov1<-melt(testnov1,c("YEAR"))
  testnov1<-ts(testnov1$value,frequency=4)
  novfinal<-ifelse(novfinal>0,novfinal,0)
  
  
  janfinal<-as.data.frame(janfinal)
  mayfinal<-as.data.frame(mayfinal)
  novfinal<-as.data.frame(novfinal)
  jan<-as.numeric(janfinal$x)
  jan<-as.data.frame(jan)
  jan<-rename(jan,c(jan="rain"))
  
  may<-as.numeric(mayfinal$x)
  may<-as.data.frame(may)
  may<-rename(may,c(may="rain"))
  
  nov<-as.numeric(novfinal$x)
  nov<-as.data.frame(nov)
  nov<-rename(nov,c(nov="rain"))
  
  final<-rbind(jan,may,nov)
  final<-ts(final$rain,frequency=12)
  
  cbind(month_name=month.name,rainfall=format(round(final, 2), nsmall = 2))
  })
  output$scrop<-renderTable({
    
    if(input$selcrop!="all"){
      crops_data<-filter(crop_data,(crop_data$crop==input$selcrop))
      crops_data<-select(crops_data,crop:CropRotation)
      
    }
    else if(input$seasoncrop=="SPRING(JAN:APR)"){
      
      crops_data<-filter(crop_data,(crop_data$sow_month=="JAN"|crop_data$sow_month=="FEB"|crop_data$sow_month=="MAR"|crop_data$sow_month=="APR"))
      crops_data<-select(crops_data,crop:CropRotation)
    }
    else  if(input$seasoncrop=="SUMMER(MAY:OCT)"){
      crops_data<-filter(crop_data,(crop_data$sow_month=="MAY"|crop_data$sow_month=="JUN"|crop_data$sow_month=="JUL"|crop_data$sow_month=="OCT"))
      crops_data<-select(crops_data,crop:CropRotation)
    }
    else{
      crops_data<-filter(crop_data,(crop_data$sow_month=="NOV"|crop_data$sow_month=="DEC"))
      crops_data<-select(crops_data,crop:CropRotation)
    }
    crops_data
  })
  var<-reactive(
    
    if(input$seasoncrop=="SPRING(JAN:APR)")
    {
      crop1<-filter(crop_data,(crop_data$sow_month=="JAN"|crop_data$sow_month=="FEB"|crop_data$sow_month=="MAR"|crop_data$sow_month=="APR"))
      crop1$crop
    }
    else  if(input$seasoncrop=="SUMMER(MAY:OCT)")
    {  crop1<-filter(crop_data,(crop_data$sow_month=="MAY"|crop_data$sow_month=="JUN"|crop_data$sow_month=="JUL"|crop_data$sow_month=="OCT"))
    crop1$crop
    }
    else{
      crop1<-filter(crop_data,(crop_data$sow_month=="NOV"|crop_data$sow_month=="DEC"))
      crop1$crop
    } 
    
  )
  
  output$crop<-renderUI({
    
    selectInput("selcrop","Select Crop",choices=c("all",var()))
    
  })
  
  output$Ques<-renderText({
    form<-read.csv("query.csv",stringsAsFactors = FALSE)
    form1=form
    form1<-select(form1,MOB:QUES)
    asked<-data.frame(MOB=input$Mob,QUES=input$Quest)
    if(input$Mob!=''&input$Quest!='')
    {  form1<-rbind(form1,asked)
    write.csv(form1,"query.csv")
    paste("Your Query is submitted")
    }
    else
      paste("Enter Full Details")
  })
  
  output$Result<-renderUI({
    form<-read.csv("query.csv",stringsAsFactors = FALSE)
    form<-select(form,MOB:QUES)
    form2=form
    fluidRow(renderDataTable(form2))
  })
  
  
  
}
