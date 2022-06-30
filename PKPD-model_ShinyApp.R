# PK-PD-Model-Visualization
# -----
# Title: PK-PD Model Visualizaiton Shiny App
#
# Author: Hsiao-Chen Dina Kuo
#
# Purpose: 
# 1. PK analysis: Visualize observed and predicted plasma concentration–time profiles of different dosing routes
# 2. PD analysis: Visualize observed and predicted mRNA expression (Effect) to time profiles of different dosing routes
#
# Date: May 2022
#
# Reference: 
# The PK/PD modeling is based on a paper I invovled in:
# "Pharmacokinetics and pharmacodynamics of three oral formulations of curcumin in rats"
# published on J Pharmacokinet Pharmacodyn. 2020 Apr; 47(2): 131–144. doi: 10.1007/s10928-020-09675-3
# The raw data were re-modeled and the predicted concentrations and effect were obtained using Phoenix Winnonlin 8.3.3.33
# 
# Sample data sets:
# Two sample data files were uploaded (one for PK model and one for PD model visualization)
# -----

# Package installation below is only for local installation
# When you deploy your application, the shinyapps.io looking for the library() calls within your application 
# to detect the packages that your application uses.
# One should not call install.packages() call within your ui.R or server.R files.
# package <- c("Shiny", "Shinydashboard", "ggplot2", "DT", "data.table")
# install.packages(package)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(data.table)
library(scales)
library(gridExtra)


ui <-  dashboardPage(
  dashboardHeader(title = "PK-PD Model Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "PK Data Preview", tabName = "PKMenu", icon = icon("table")),
      menuItem(text = "PK Plot", tabName = "PKplotMenu", icon = icon("chart-bar")),
      menuItem(text = "PD Data Preview", tabName = "PDMenu", icon = icon("table")),
      menuItem(text = "PD Plot", tabName = "PDplotMenu", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      ## ---------------- PK Profile Visualization ----------------- ##
      tabItem(
        tabName = "PKMenu",
        ## ---------------- Read in PK Data ------------------ ##
        fileInput(inputId = "PKData", label = "Upload the PK csv file:", multiple = FALSE, accept = c(".csv")),
        checkboxInput("PKheader", label = "PK file contains a header", value = TRUE),
        DTOutput("d1")
      ),
      tabItem(
        tabName = "PKplotMenu",
        ## ---------------- Customize y Scale & Title and Generate Plot ------------------ ##
        radioButtons("PKscale", label = "Scale for y axis", choices = c("normal", "log10", "natural log", "log2")),
        checkboxInput("PKshowTitle", label = "Check to enter title", value = FALSE), 
        conditionalPanel(condition = "input.PKshowTitle == true",
                         textInput("PKtitle", label  = "Enter a plot title:", placeholder = "Title")),
        plotOutput("p1", width = "50%"),
        ## ---------------- Download Data Button------------------ ##
        downloadButton("PKdownloadData", "Download Data")
      ),
      
      ## ---------------- PD Profile Visualization ----------------- ##
      tabItem(
        tabName = "PDMenu",
        ## ---------------- Read in PD Data ------------------ ##
        fileInput(inputId = "PDData", label = "Upload the PD csv file:", multiple = FALSE, accept = c(".csv")),
        checkboxInput("PDheader", label = "PD file contains a header", value = TRUE),
        DTOutput("d2")
      ),
      tabItem(
        tabName = "PDplotMenu",
        ## ---------------- Customize y Scale & Title and Generate Plot ------------------ ##
        radioButtons("PDscale", label = "Scale for y axis", choices = c("normal", "log10", "natural log", "log2")),
        checkboxInput("PDshowTitle", label = "Check to enter title", value = FALSE), 
        conditionalPanel(condition = "input.PDshowTitle == true",
                         textInput("PDtitle", label  = "Enter a plot title:", placeholder = "Title")),
        plotOutput("p2", width = "50%"),
        ## ---------------- Download Data Button------------------ ##
        downloadButton("PDdownloadData", "Download Data")
      )
   ) 
 )
)


server <- function(input, output, session) {
  ## ---------------- PK Data Preparation ----------------- ##
  d1 <- reactive({
    
    req(input$PKData)
    d1 <- read.csv(input$PKData$datapath, header = input$PKheader)
    colnames(d1) <- c("Time", "Observed", "Predicted", "Route", "SD")
    return(d1)
    
  })
  
  ## ---------------- PK Plot Preparation ----------------- ##
  PKplotoutput <- reactive({
    
    PKplot <- ggplot(d1(), aes(x = Time, y = Observed, color = Route)) +
      geom_point(aes(shape= Route), size = 3) +
      labs(x = "Time (h)", y = "Plasma Conc (ng/mL)") +
      # Adjust the x axis range and tick interval here #
      scale_x_continuous(breaks = seq(0, 14, 2)) +
      geom_line(data = d1(), aes(x = Time, y = Predicted, group = Route), size = 1.3) +
      geom_errorbar(aes(ymin = Observed, ymax = Observed + SD), 
                    width = .4, size = 1,
                    position = position_dodge(0.07)) +
      theme_classic() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
    
    if(input$PKshowTitle == TRUE){
      PKplot <- PKplot + ggtitle(input$PKtitle) + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      
    } else {
    }
    
    if(input$PKscale == "normal"){
      return(PKplot)
      
    } else if (input$PKscale == "log10") {
      PKplot <- PKplot + scale_y_continuous(trans="log10")
      return(PKplot)
      
    } else if (input$PKscale == "natural log") {
      PKplot <- PKplot + scale_y_continuous(trans=log_trans())
      return(PKplot)
      
    } else {
      PKplot <- PKplot + scale_y_continuous(trans="log2")
      return(PKplot)
      
    }
    
  })
  

  ## ---------------- Display PK Data Table ----------------- ##
  output$d1 <- renderDT({
    req(input$PKData)
    d1 <- read.csv(input$PKData$datapath, header = input$PKheader)
    colnames(d1) <- c("Time", "Observed", "Predicted", "Route", "SD")
    return(datatable(d1))
    
  })
  
  ## ---------------- Display PK Plot ----------------- ## 
  output$p1 <- renderPlot({
    
    return(PKplotoutput())
    
  })
  
  ## ---------------- Download PK Plot in png ----------------- ##
  output$PKdownloadData <- downloadHandler(
    filename = function(){
      paste("PK - Observed and Predicted Conc", ".png", sep = "")
    },
    
    content = function(file){
      png(file, width = 800, height = 480, units = "px", pointsize = 20,
          bg = "black")
      plot(PKplotoutput())
      dev.off()
    } )
  
  ## ---------------- PD Data Preparation ----------------- ##
  d2 <- reactive({
    
    req(input$PDData)
    d2 <- read.csv(input$PDData$datapath, header = input$PDheader)
    colnames(d2) <- c("Time", "Observed", "Predicted", "Effect", "SD")
    return(d2)
    
  })
  
  ## ---------------- PD Plot Preparation ----------------- ##
  PDplotoutput <- reactive({
    
    PDplot <- ggplot(d2(), aes(x = Time, y = Observed, color = Effect)) +
      geom_point(size = 3) +
      labs(x = "Time (h)", y = "Gene Fold Change") +
      # Adjust the y axis range and tick interval here #
      scale_y_continuous(breaks = seq(0, 4, 0.5)) +
      # Adjust the x axis range and tick interval here #
      scale_x_continuous(breaks = seq(0, 14, 1)) +
      geom_line(data = d2(), aes(x = Time, y = Predicted), size = 1.3) +
      geom_errorbar(aes(ymin = Observed, ymax = Observed + SD), 
                    width = .4, size = 1,
                    position = position_dodge(0.07)) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            strip.text.x = element_text(size = 14),
            legend.position="none") +
      facet_wrap(vars(Effect))
    
    if(input$PDshowTitle == TRUE){
      PDplot <- PDplot + ggtitle(input$PDtitle) + theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
      
    } else {
    }
    
    if(input$PDscale == "normal"){
      return(PDplot)
      
    } else if (input$PDscale == "log10") {
      PDplot <- PDplot + scale_y_continuous(trans="log10")
      return(PDplot)
      
    } else if (input$PDscale == "natural log") {
      PDplot <- PDplot + scale_y_continuous(trans=log_trans())
      return(PDplot)
      
    } else {
      PDplot <- PDplot + scale_y_continuous(trans="log2")
      return(PDplot)
      
    }
    
  })
  ## ---------------- Display PD Data Table ----------------- ##
  output$d2 <- renderDT({
    req(input$PDData)
    d2 <- read.csv(input$PDData$datapath, header = input$PDheader)
    colnames(d2) <- c("Time", "Observed", "Predicted", "Effect", "SD")
    return(datatable(d2))
    
  })
  
  ## ---------------- Display PD Plot ----------------- ## 
  output$p2 <- renderPlot({
    
    return(PDplotoutput())
    
  })
  
  ## ---------------- Download PD Plot in png ----------------- ##
  output$PDdownloadData <- downloadHandler(
    filename = function(){
      paste("PD - Observed and Predicted Effect", ".png", sep = "")
    },
    
    content = function(file){
      png(file, width = 800, height = 480, units = "px", pointsize = 20,
          bg = "black")
      plot(PDplotoutput())
      dev.off()
    } )

}



shinyApp(ui = ui, server = server)

