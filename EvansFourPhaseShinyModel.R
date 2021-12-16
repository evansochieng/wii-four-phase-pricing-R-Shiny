library(shiny)
library(RColorBrewer)
library(dplyr)

ui<-fluidPage (
    titlePanel("PIXEL524507 FABA BEANS DROUGHT COVER"),
    
    fluidRow(
      h3(strong("Customer Personal Information")),
      column(6,
             textInput(inputId="name", "Enter your name", value = "")
      ),
      column(6,
             numericInput("contact", "Enter your Phone number", value = "")
      )
    ),
    
    fluidRow(
      column(4,
             wellPanel(
               h3(strong("Crop Specifications")),
               h4(strong("Cover Period")),
               sliderInput(inputId="emerg", label="Emergence Phase Cover Period", 1, 366, value=c(153, 162)),
               sliderInput(inputId="veg", label="Vegetative Phase Cover Period", 1, 366, value=c(163, 212)),
               sliderInput(inputId="rep", label="Reproductive Phase Cover Period", 1, 366, value=c(213, 222)),
               sliderInput(inputId="pod", label="Podding Phase Cover Period", 1, 366, value=c(223, 232)),
               
               h4(strong("Trigger")),
               selectInput("emergtrig", "Emergence Phase Trigger", choices=seq(15,25,1)),
               selectInput("vegtrig", "Vegetative Phase Trigger", choices=seq(180,220,5)),
               selectInput("reptrig", "Reproductive Phase Trigger", choices=seq(35,45,1)),
               selectInput("podtrig", "Podding Phase Trigger", choices=seq(40,60,2)),
               
               h4(strong("Exit")),
               selectInput("emergex", "Emergence Phase Exit", seq(5,10,1)),
               selectInput("vegex", "Vegetative Phase Exit", seq(100,140,5)),
               selectInput("repex", "Reproductive Phase Exit", seq(15,25,1)),
               selectInput("podex", "Podding Phase Exit", seq(10,30,2)),
               
               h4(strong("Weight")),
               selectInput("emerg.weight","Emergence Phase weight", seq(0.25,1,0.05)),
               selectInput("veg.weight","Vegetative Phase weight", seq(0.25,1,0.05)),
               selectInput("rep.weight","Reproductive Phase weight", seq(0.25,1,0.05)),
               selectInput("pod.weight","Podding Phase weight", seq(0.25,1,0.05)),
  
               h3(strong("Pricing Factors")),
               h4(strong("Sum Insured")),
               numericInput("amount","Sum Insured", 0, 50000, 10000),
               
               h4(strong("Deductible")),
               selectInput(inputId="type",
                           label="Deductible type",
                           choices=c(1,2), selected = 2),
               helpText("Note: 1= Franchise deductible",
                        "2= Ordinary deductible"),
               
               radioButtons(inputId="deductible",
                            label="Deductible level",
                            choices=c(10.0, 15, 20.0), selected = 20.0),
               helpText("Note: These are values of deductibles",
                        "      Pick one that best suits you")
             )
      ),
      
      column(8,
             h3(strong("Historical Payouts")),
             h4("Emergence Phase Historical Payout"),
             plotOutput("plot1", width="100%"),
             
             h4("Vegetative Phase Historical Payout"),
             plotOutput("plot2", width="100%"),
             
             h4("Reproductive Phase Historical Payout"),
             plotOutput("plot3", width="100%"),
             
             h4("Podding Phase Historical Payout"),
             plotOutput("plot4", width="100%")
      )
               
    ),
    
    fluidRow(
      column(6,
             h3("Premium"),
             tableOutput("table")
      ),
      column(6,
             actionButton(inputId = "click", label = "SUBMIT")
      )
      
    )
    

)

server<- function(input,output) {
  setwd("C:/Users/HP/Desktop/Syngenta/Aug-Dec 2020/RShiny/R Shiny")
  data<-read.csv("Pixel524507.csv", header=TRUE)
  data[data<0]<-NA
  new.data<-select(data, -X)
  colnames(new.data)<-substr(colnames(new.data),2,nchar(colnames(new.data)))
  
  emerg.rainfall_slider<-reactive({
    emerg.rain_received<-data.frame(data=NA, nrow=1, ncol=ncol(new.data))
    for(i in 1:ncol(new.data)){emerg.rain_received[i]<-sum(new.data[input$emerg[1]:input$emerg[2], i], na.rm = TRUE)}
    emerg.rain_received })
  
  veg.rainfall_slider<-reactive({
    veg.rain_received<-data.frame(data=NA, nrow=1, ncol=ncol(new.data))
    for(i in 1:ncol(new.data)){veg.rain_received[i]<-sum(new.data[input$veg[1]:input$veg[2], i], na.rm = TRUE)}
    veg.rain_received })
  
  rep.rainfall_slider<-reactive({
    rep.rain_received<-data.frame(data=NA, nrow=1, ncol=ncol(new.data))
    for(i in 1:ncol(new.data)){rep.rain_received[i]<-sum(new.data[input$rep[1]:input$rep[2], i], na.rm = TRUE)}
    rep.rain_received })
  
  pod.rainfall_slider<-reactive({
    pod.rain_received<-data.frame(data=NA, nrow=1, ncol=ncol(new.data))
    for(i in 1:ncol(new.data)){pod.rain_received[i]<-sum(new.data[input$pod[1]:input$pod[2], i], na.rm = TRUE)}
    pod.rain_received })
  
  emerg.trigger_slider<-reactive({
    emerg.tr<-as.numeric(input$emergtrig) })
  
  veg.trigger_slider<-reactive({
    veg.tr<-as.numeric(input$vegtrig) })
  
  rep.trigger_slider<-reactive({
    rep.tr<-as.numeric(input$reptrig) })
  
  pod.trigger_slider<-reactive({
    pod.tr<-as.numeric(input$podtrig) })
  
  emerg.exit_slider<-reactive({
    emerg.ex<-as.numeric(input$emergex) })
  
  veg.exit_slider<-reactive({
    veg.ex<-as.numeric(input$vegex) })
  
  rep.exit_slider<-reactive({
    rep.ex<-as.numeric(input$repex) })
  
  pod.exit_slider<-reactive({
    pod.ex<-as.numeric(input$podex) })
  
  emerg.payout_slider<-reactive({
    
    emerg.rain.received<-emerg.rainfall_slider()
    emerg.per.trigger<-as.numeric(emerg.trigger_slider())
    emerg.per.exit<-as.numeric(emerg.exit_slider())
    
    emerg.tick<-as.numeric(input$emerg.weight)/(emerg.per.trigger - emerg.per.exit)
    
    emerg.payout<-data.frame(data=NA, nrow=1, ncol=ncol(new.data))
    for(i in 1:ncol(new.data)){emerg.payout[i]<-
      if(emerg.rain.received[i]>emerg.per.exit & emerg.rain.received[i]<=emerg.per.trigger){
        emerg.payout[i]<-abs((emerg.rain.received[i] - emerg.per.trigger))*emerg.tick
      }else if(emerg.rain.received[i]<=emerg.per.exit){
        emerg.payout[i] <-as.numeric(input$emerg.weight)
      }else{
        emerg.payout[i] <-0
      }
    }
    colnames(emerg.payout)<-colnames(new.data)
    emerg.payout
  })
  
  veg.payout_slider<-reactive({
    
    veg.rain.received<-veg.rainfall_slider()
    veg.per.trigger<-as.numeric(veg.trigger_slider())
    veg.per.exit<-as.numeric(veg.exit_slider())
    
    veg.tick<-as.numeric(input$veg.weight)/(veg.per.trigger - veg.per.exit)
    
    veg.payout<-data.frame(data=NA, nrow=1, ncol=ncol(new.data))
    for(i in 1:ncol(new.data)){veg.payout[i]<-
      if(veg.rain.received[i]>veg.per.exit & veg.rain.received[i]<=veg.per.trigger){
        veg.payout[i]<-abs((veg.rain.received[i] - veg.per.trigger))*veg.tick
      }else if(veg.rain.received[i]<=veg.per.exit){
        veg.payout[i] <-as.numeric(input$veg.weight)
      }else{
        veg.payout[i] <-0
      }
    }
    colnames(veg.payout)<-colnames(new.data)
    veg.payout
  })
  
  rep.payout_slider<-reactive({
    
    rep.rain.received<-rep.rainfall_slider()
    rep.per.trigger<-as.numeric(rep.trigger_slider())
    rep.per.exit<-as.numeric(rep.exit_slider())
    
    rep.tick<-as.numeric(input$rep.weight)/(rep.per.trigger - rep.per.exit)
    
    rep.payout<-data.frame(data=NA, nrow=1, ncol=ncol(new.data))
    for(i in 1:ncol(new.data)){rep.payout[i]<-
      if(rep.rain.received[i]>rep.per.exit & rep.rain.received[i]<=rep.per.trigger){
        rep.payout[i]<-abs((rep.rain.received[i] - rep.per.trigger))*rep.tick
      }else if(rep.rain.received[i]<=rep.per.exit){
        rep.payout[i] <-as.numeric(input$rep.weight)
      }else{
        rep.payout[i] <-0
      }
    }
    colnames(rep.payout)<-colnames(new.data)
    rep.payout
  })
  
  pod.payout_slider<-reactive({
    
    pod.rain.received<-pod.rainfall_slider()
    pod.per.trigger<-as.numeric(pod.trigger_slider())
    pod.per.exit<-as.numeric(pod.exit_slider())
    
    pod.tick<-as.numeric(input$pod.weight)/(pod.per.trigger - pod.per.exit)
    
    pod.payout<-data.frame(data=NA, nrow=1, ncol=ncol(new.data))
    for(i in 1:ncol(new.data)){pod.payout[i]<-
      if(pod.rain.received[i]>pod.per.exit & pod.rain.received[i]<=pod.per.trigger){
        pod.payout[i]<-abs((pod.rain.received[i] - pod.per.trigger))*pod.tick
      }else if(pod.rain.received[i]<=pod.per.exit){
        pod.payout[i] <-as.numeric(input$pod.weight)
      }else{
        pod.payout[i] <-0
      }
    }
    colnames(pod.payout)<-colnames(new.data)
    pod.payout
  })
  
  output$plot1 <-renderPlot({
    emerg.payouts<-emerg.payout_slider()
    emerg.payout.mat<-as.matrix(emerg.payouts)
    emerg.colour<-brewer.pal(1:ncol(emerg.payout.mat),"Accent")
    barplot(height=emerg.payout.mat, names.arg = colnames(emerg.payout.mat), col = emerg.colour, main = "Emergence Phase Historical Payout", xlab = "Years", ylab = "Payout")
    
  })
  
  output$plot2 <-renderPlot({
    veg.payouts<-veg.payout_slider()
    veg.payout.mat<-as.matrix(veg.payouts)
    veg.colour<-brewer.pal(1:ncol(veg.payout.mat),"Dark2")
    barplot(height=veg.payout.mat, names.arg = colnames(veg.payout.mat), col = veg.colour, main = "Vegetative Phase Historical Payout", xlab = "Years", ylab = "Payout")
    
  })
  
  output$plot3 <-renderPlot({
    rep.payouts<-rep.payout_slider()
    rep.payout.mat<-as.matrix(rep.payouts)
    rep.colour<-brewer.pal(1:ncol(rep.payout.mat),"Paired")
    barplot(height=rep.payout.mat, names.arg = colnames(rep.payout.mat), col = rep.colour, main = "Reproductive Phase Historical Payout", xlab = "Years", ylab = "Payout")
    
  })
  
  output$plot4 <-renderPlot({
    pod.payouts<-pod.payout_slider()
    pod.payout.mat<-as.matrix(pod.payouts)
    pod.colour<-brewer.pal(1:ncol(pod.payout.mat),"RdBu")
    barplot(height=pod.payout.mat, names.arg = colnames(pod.payout.mat), col = pod.colour, main = "Podding Phase Historical Payout", xlab = "Years", ylab = "Payout")
    
  })
  
  emerg.average_payout <- reactive ({
    emerg.payouts<-emerg.payout_slider()
    emerg.payouts<-as.matrix(emerg.payouts)
    mean(emerg.payouts,na.rm = TRUE)
  })
  
  veg.average_payout <- reactive ({
    veg.payouts<-veg.payout_slider()
    veg.payouts<-as.matrix(veg.payouts)
    mean(veg.payouts,na.rm = TRUE)
  })
  
  rep.average_payout <- reactive ({
    rep.payouts<-rep.payout_slider()
    rep.payouts<-as.matrix(rep.payouts)
    mean(rep.payouts,na.rm = TRUE)
  })
  
  pod.average_payout <- reactive ({
    pod.payouts<-pod.payout_slider()
    pod.payouts<-as.matrix(pod.payouts)
    mean(pod.payouts,na.rm = TRUE)
  })
  
  average_payout <- reactive ({
    ep<-emerg.average_payout()
    vp<-veg.average_payout()
    rp<-rep.average_payout()
    pp<-pod.average_payout()
    payouts<-c(ep,vp,rp,pp)
    mean(payouts,na.rm = TRUE)
  })
  
  payout_factor <- reactive ({
    av.payout <- average_payout()
    
    if(input$type ==1) {
      if(av.payout >= input$deductible){
        compens<- av.payout
      }
      else{
        compens<- 0
      }
    }else{
      if(av.payout <= input$deductible){
        compens<- 0
      }
      else{
        compens<- av.payout - input$deductible
      }
      
    }
    av.payout 
  })
  
  
  output$table <- renderTable ({
    payout.factor<-payout_factor()
    payout.factor * as.numeric(input$amount)  
  }) %>% 
    bindEvent(payout_factor(), input$click)
  
  
}
shinyApp(ui=ui,server=server)
