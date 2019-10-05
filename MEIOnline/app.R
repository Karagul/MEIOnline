
# Make all lines show up DONE 
# Lose the percent / level where appropriate 
# Collapse into drop-down bars
# Merge with / embed text. This may require getting the Shiny page onto the internet, not just a server. 

library(shiny)
library(xts)
library(ggplot2)
library(reshape)
library(reshape2)
library(data.table)
library(grid)
library(gridExtra)
library(gdata)
library(magrittr)
library(shinydashboard)
library(readxl)
library(shinycssloaders)
library(zoo) 
library(stinepack) 
library(plotly)
library(rsconnect)
#rsconnect::setAccountInfo(name='panynjeconomics', token='1D519C70EC4EF1F91F7F41B331B7E6CF', secret='NEVeHJKEBKs+BQIxWi1YHcD3q04/GRXXhCIL/Dv9')

current_year = 2018
select_year = current_year-1 


ui = shinyUI(dashboardPage(
  
  dashboardHeader(title = "Monthly Economic Indicators", titleWidth = 350), #Transportation Economic Statistics
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      menuItem("Regional Economy", tabName = "region", icon = icon("wrench", lib = "glyphicon")), #("user-hard-hat")),
      menuItem("Prices", tabName = "prices", icon = icon("envelope", lib = "glyphicon")), #("dollar-icon")), 
      menuItem("Property", tabName = "property", icon = icon("home", lib = "glyphicon")), 
      menuItem("Port", tabName = "port", icon = icon("cube")),
      menuItem("Bridges & Tunnels", tabName = "tbt", icon = icon("car")),
      menuItem("Aviation", tabName = "avi", icon = icon("plane")),
      menuItem("PATH", tabName = "path", icon = icon("train")),
      menuItem("Ferry", tabName = "ferry", icon = icon("ship"))

    )
  ),
  dashboardBody(
    tabItems(
      
      
      
      # ACTUAL First tab content
      tabItem(tabName = "region", h2("Regionall Economics"),
              fluidRow(
                column(8,
                       radioButtons("rep_region", "",
                                    c("Value" = "value",
                                      "Percent Change" = "percent"), inline = TRUE)
                )
              ),
              fluidRow(
                column(3, downloadButton('download_region', 'Download Data'))
              ),
              hr(),
              fluidRow(
                column(12, plotlyOutput("region_chart", height = "800px")%>% withSpinner(color="#0dc5c1"))
              )
      ),
      
      
      
      # ACTUAL First-and-a-half tab content
      tabItem(tabName = "prices", h2("Prices"),
              fluidRow(
                column(8,
                       radioButtons("rep_prices", "",
                                    c("Value" = "value",
                                      "Percent Change" = "percent"), inline = TRUE)
                )
              ),
              fluidRow(
                column(3, downloadButton('download_prices', 'Download Data'))
              ),
              hr(),
              fluidRow(
                column(12, plotlyOutput("prices_chart", height = "800px")%>% withSpinner(color="#0dc5c1"))
              )
      ),
      
      
      
      # ACTUAL Second tab content
      tabItem(tabName = "property", h2("Property"),
              fluidRow(
                column(8,
                       radioButtons("rep_property", "",
                                    c("Value" = "value",
                                      "Percent Change" = "percent"), inline = TRUE)
                )
              ),
              fluidRow(
                column(3, downloadButton('download_property', 'Download Data'))
              ),
              hr(),
              fluidRow(
                column(12, plotlyOutput("property_chart", height = "800px")%>% withSpinner(color="#0dc5c1"))
              )                                                  
      ), 
      
      
      # First tab content
      tabItem(tabName = "port", h2("Container Import/Export Statistics"),
              fluidRow(
                column(8,
                       radioButtons("rep_port", "",
                                    c("Value" = "value",
                                      "Percent Change" = "percent"), inline = TRUE)
                )
              ),
              fluidRow(
                column(3, downloadButton('download_port', 'Download Data'))
              ),
              hr(),
              fluidRow(
                column(12, plotlyOutput("port_chart", height = "800px")%>% withSpinner(color="#0dc5c1"))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "tbt",
              h2("Bridges & Tunnels Statistics"),
              fluidRow(
                column(5,
                       radioButtons("rep_type", "",
                                    c("Vehicle Type" = "veh",
                                      "Facility" = "fac"), inline = TRUE)
                ),
                column(5,
                       radioButtons("rep_tbt", "",
                                    c("Value" = "value",
                                      "Percent Change" = "percent"), inline = TRUE)
                )
              ),
              fluidRow(
                column(3, downloadButton('download_tbt', 'Download Data'))
              ),
              hr(),
              fluidRow(
                column(12, plotlyOutput("tbt_chart", height = "800px")%>% withSpinner(color="#0dc5c1"))
                
                
              )
      ),
      # Third tab content
      tabItem(tabName = "avi",
              h2("Aviation Statistics"),
              fluidRow(
                column(8,
                       radioButtons("rep_type_avi", "Report By:",
                                    c("Airport" = "fac", "Access Type" = "atype",
                                      "Freight Type" = "ftype", "Carrier Type" = "fli_type"), inline = TRUE)
                )
              ),
              fluidRow(
                column(5,
                       radioButtons("rep_avi", "",
                                    c("Value" = "value",
                                      "Percent Change" = "percent"), inline = TRUE)
                )
              ),
              fluidRow(
                column(3, downloadButton('download_avi', 'Download Data'))
              ),
              hr(),
              fluidRow(
                column(12, plotlyOutput("avi_chart", height = "800px")%>% withSpinner(color="#0dc5c1"))
                
              )
      ),
      # Fourth tab content
      tabItem(tabName = "path", h2("PATH Ridership"),
              
              fluidRow(
                column(5,
                       radioButtons("rep_path", "",
                                    c("Value" = "value",
                                      "Percent Change" = "percent"), inline = TRUE)
                )
              ),
              fluidRow(
                column(3, downloadButton('download_path', 'Download Data'))
              ),
              hr(),
              fluidRow(
                column(12, plotlyOutput("path_chart", height = "800px")%>% withSpinner(color="#0dc5c1"))
                
              )
      ),
      # Fifth tab content
      tabItem(tabName = "ferry", h2("Ferry Ridership"),
              fluidRow(
                column(5,
                       radioButtons("rep_ferry", "",
                                    c("Value" = "value",
                                      "Percent Change" = "percent"), inline = TRUE)
                )
              ),
              fluidRow(
                column(3, downloadButton('download_ferry', 'Download Data'))
              ),
              hr(),
              fluidRow(
                column(12, plotlyOutput("ferry_chart", height = "600px")%>% withSpinner(color="#0dc5c1"))
                
              )
      ),
      
      
      
      # Sixth tab content
      tabItem(tabName = "compare", h2("Variable Comparison"),
              fluidRow(
                column(5,
                       radioButtons("rep_comp", "",
                                    c("Value" = "value",
                                      "Percent Change" = "percent"), inline = TRUE)
                ),
                column(7,
                       selectizeInput(
                         inputId = "vars", 
                         label = "Select a variable", 
                         choices = c("Air Passenger" = "air",
                                     "Vehicles" = "veh", "Ferry" = "ferry", "PATH" = "path"), 
                         selected = "air",
                         multiple = TRUE
                       )
                )
              ),
              hr(),
              fluidRow(
                column(12, plotlyOutput("comp_chart", height = "800px")%>% withSpinner(color="#0dc5c1"))
              )
      ),
      
      # Whatever tab item 
      # Make this a transportation drop-down using whatever 
      tabItem(tabName = "trans", h2("My Transport Indicators"),
              fluidRow(
                column(5, 
                       selectInput("opti", 
                                   label = "Variable", 
                                   choices = c("Unemployment", "US_CPI", "Gasoline", 
                                               "Regional_employment", "Regional_CPI", "Regional_housing_index", 
                                               "US_real_GDP", "Midtown_rent","Downtown_rent"), 
                                   selected = "US_GDP_real") 
                )
              ),
              plotOutput("graph")%>% withSpinner(color="#0dc5c1")
      )
      
      
      #      # Seventh tab item 
      #      tabItem(tabName = "eco", h2("Economic Indicators"),
      #              fluidRow(
      #                column(5, 
      #                       selectInput("opti", 
      #                                   label = "Variable", 
      #                                   choices = c("Unemployment", "US_CPI", "Gasoline", 
      #                                               "Regional_employment", "Regional_CPI", "Regional_housing_index", 
      #                                               "US_real_GDP", "Midtown_rent","Downtown_rent"), 
      #                                   selected = "US_GDP_real") 
      #                )
      #              ),
      #              plotOutput("graph")%>% withSpinner(color="#0dc5c1")
      #      )
      
      # Eighthish tab item 
      #      tabItem(tabName = "eco", h2("Economics"),
      #              fluidRow(
      #                column(5, 
      #                       selectInput("opti", 
      #                                   label = "Variable", 
      #                                   choices = c("Unemployment", "US_CPI", "Gasoline", 
      #                                               "Regional_employment", "Regional_CPI", "Regional_housing_index", 
      #                                               "US_real_GDP", "Midtown_rent","Downtown_rent"), 
      #                                   selected = "US_GDP_real") 
      #                )
      #              ),
      #              plotOutput("graph")%>% withSpinner(color="#0dc5c1")
      #      )
      
      
      
    )
  )
)
)


server = shinyServer(function(input, output) {
  m = list(l = 50, r = 50, b = 100, t = 100, pad = 4)
  region_all = data.frame(read_excel ("./mei_4_19all.xlsx", sheet = "Region"))
  prices_all = data.frame(read_excel ("./mei_4_19all.xlsx", sheet = "Prices"))
  property_all = data.frame(read_excel ("./mei_4_19all.xlsx", sheet = "Property"))
  ferry = data.frame(read_excel ("./mei_4_19all.xlsx", sheet = "Ferry"))
  port_all = data.frame(read_excel ("./mei_4_19all.xlsx",  sheet = "Port"))
  path_all = data.frame(read_excel ("./mei_4_19all.xlsx", sheet = "PATH"))
  avi_all = data.frame(read_excel ("./mei_4_19all.xlsx", sheet = "Aviation"))
  tbt_all = data.frame(read_excel ("./mei_4_19all.xlsx", sheet = "TBT"))
  
  
  econ_month = read.csv("./mei_month.csv")
  econ_quarter = read.csv("./mei_quarter.csv")
  
  
  datasetInput_region = reactive({
    region=region_all[, 1:4]
    if (input$rep_region == "value"){
      selected_data= data.frame(region[,1], region[,2:4])
    }
    else if (input$rep_region == "percent"){
      data_1=region[,2]
      data_2=region[,3]
      data_3=region[,4]
      data_diff_1=diff(data_1,12)
      data_diff_2=diff(data_2,12)
      data_diff_3=diff(data_3,12)
      data_1=data_diff_1/region[1:(NROW(region[,3])-13),2]*100
      data_2=data_diff_2/region[1:(NROW(region[,3])-13),3]*100
      data_3=data_diff_3/region[1:(NROW(region[,3])-13),4]*100
      selected_data= data.frame(region[13:NROW(region[,3]),1], data_1, data_2, data_3)
    }
  })
  
  output$region_chart = renderPlotly({
    df= datasetInput_region()
    df[,1]=as.array(df[,1])
    if (input$rep_region == "value"){
      setnames(df,1:4, c("Date", "GDPreal", "Employment", "UErate"))
      title_name="Monthly Regional Economics"
      
    }
    else if (input$rep_region == "percent"){
      setnames(df,1:4, c("Date", "GDPreal", "Employment", "UErate"))
      title_name="Monthly Regional Economics (% change)"
      
    }
    p1 = plot_ly(df, x = ~Date, y = ~GDPreal) %>%
      add_lines(name = ~"GDPreal") 
    p2 = plot_ly(df, x = ~Date, y = ~Employment) %>%
      add_lines(name = ~"Employment") 
    p3 = plot_ly(df, x = ~Date, y = ~UErate) %>%
      add_lines(name = ~"UErate")
    p = subplot(p1, p2, p3, nrows = 3, shareX = TRUE)%>%
      layout(autosize = T, margin = m,
             title = title_name, 
             xaxis = list(categoryorder = "array",categoryarray = df$Date,
                          rangeslider = list(type = "date")))
  }) 
  
  output$download_region = downloadHandler(
    
    filename = function() { 
      paste('region', '.csv', sep='') 
    },
    content = function(file) {
      out=write.csv(datasetInput_region(), file, row.names = FALSE, append = FALSE, quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", col.names = TRUE)
    }
  )
  
  
  
  
  # Prices 
  datasetInput_prices = reactive({
    prices=prices_all[, 1:3]
    if (input$rep_prices == "value"){
      selected_data= data.frame(prices[,1], prices[,2:3])
    }
    else if (input$rep_prices == "percent"){
      data_1=prices[,2]
      data_2=prices[,3]
      data_diff_1=diff(data_1,12)
      data_diff_2=diff(data_2,12)
      data_1=data_diff_1/prices[1:(NROW(prices[,3])-13),2]*100
      data_2=data_diff_2/prices[1:(NROW(prices[,3])-13),3]*100
      selected_data= data.frame(prices[13:NROW(prices[,3]),1], data_1, data_2) 
    }
  })
  
  output$prices_chart = renderPlotly({
    df= datasetInput_prices()
    df[,1]=as.array(df[,1])
    if (input$rep_prices == "value"){
      setnames(df,1:3, c("Date", "CPI", "Gasoline_cents"))
      title_name="Monthly Prices"
      
    }
    else if (input$rep_prices == "percent"){
      setnames(df,1:3, c("Date", "CPI", "Gasoline_cents"))
      title_name="Monthly Prices % change"
      
    }
    p1 = plot_ly(df, x = ~Date, y = ~CPI) %>%
      add_lines(name = ~"CPI") 
    p2 = plot_ly(df, x = ~Date, y = ~Gasoline_cents) %>%
      add_lines(name = ~"Gasoline_cents") 
    p = subplot(p1, p2,
                nrows = 2,
                shareX = TRUE)%>%
      layout(autosize = T, margin = m,
             title = title_name, 
             xaxis = list(categoryorder = "array",categoryarray = df$Date,
                          rangeslider = list(type = "date")))
  }) 
  
  output$download_prices = downloadHandler(
    
    filename = function() { 
      paste('prices', '.csv', sep='') 
    },
    content = function(file) {
      out=write.csv(datasetInput_prices(), file, row.names = FALSE, append = FALSE, quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", col.names = TRUE)
    }
  )
  
  
  # Property 
  datasetInput_property = reactive({
    property=property_all[, 1:4]
    if (input$rep_property == "value"){
      selected_data= data.frame(property[,1], property[,2:4])
    }
    else if (input$rep_property == "percent"){
      data_1=property[,2]
      data_2=property[,3]
      data_3=property[,4]
      data_diff_1=diff(data_1,12)
      data_diff_2=diff(data_2,12)
      data_diff_3=diff(data_3,12)
      data_1=data_diff_1/property[1:(NROW(property[,3])-13),2]*100
      data_2=data_diff_2/property[1:(NROW(property[,3])-13),3]*100
      data_3=data_diff_3/property[1:(NROW(property[,3])-13),4]*100
      selected_data= data.frame(property[13:NROW(property[,3]),1], data_1, data_2, data_3)
    }
  })
  
  output$property_chart = renderPlotly({
    df= datasetInput_property()
    df[,1]=as.array(df[,1])
    if (input$rep_property == "value"){
      setnames(df,1:4, c("Date", "Housing","Midtown","Downtown"))
      title_name="Property"
      
    }
    else if (input$rep_property == "percent"){
      setnames(df,1:4, c("Date", "Housing","Midtown","Downtown"))
      title_name="Property (% change)"
      
    }
    p1 = plot_ly(df, x = ~Date, y = ~Housing) %>%
      add_lines(name = ~"Housing") 
    p2 = plot_ly(df, x = ~Date, y = ~Midtown) %>%
      add_lines(name = ~"Midtown") 
    p3 = plot_ly(df, x = ~Date, y = ~Downtown) %>%
      add_lines(name = ~"Downtown")
    p = subplot(p1, p2, p3, nrows = 3, shareX = TRUE)%>%
      layout(autosize = T, margin = m,
             title = title_name, 
             xaxis = list(categoryorder = "array",categoryarray = df$Date,
                          rangeslider = list(type = "date")))
  })
  
  output$download_property = downloadHandler(
    
    filename = function() { 
      paste('property', '.csv', sep='') 
    },
    content = function(file) {
      out=write.csv(datasetInput_property(), file, row.names = FALSE, append = FALSE, quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", col.names = TRUE)
    }
  )
  
  datasetInput_port = reactive({
    port=port_all[, 1:4]
    if (input$rep_port == "value"){
      selected_data= data.frame(port[,1], port[,2:4])
    }
    else if (input$rep_port == "percent"){
      data_1=port[,2]
      data_2=port[,3]
      data_3=port[,4]
      data_diff_1=diff(data_1,12)
      data_diff_2=diff(data_2,12)
      data_diff_3=diff(data_3,12)
      data_1=data_diff_1/port[1:(NROW(port[,3])-13),2]*100
      data_2=data_diff_2/port[1:(NROW(port[,3])-13),3]*100
      data_3=data_diff_3/port[1:(NROW(port[,3])-13),4]*100
      selected_data= data.frame(port[13:NROW(port[,3]),1], data_1, data_2, data_3)
    }
  })
  
  output$port_chart = renderPlotly({
    df= datasetInput_port()
    df[,1]=as.array(df[,1])
    if (input$rep_port == "value"){
      setnames(df,1:4, c("Date", "Import", "Export", "Rail"))
      title_name="Monthly Port Import/Export"
      
    }
    else if (input$rep_port == "percent"){
      setnames(df,1:4, c("Date", "Import", "Export", "Rail"))
      title_name="Monthly Port Import/Export Year Over Year Percent Change (%)"
      
    }
    p1 = plot_ly(df, x = ~Date, y = ~Import) %>%
      add_lines(name = ~"Import TEUs") 
    p2 = plot_ly(df, x = ~Date, y = ~Export) %>%
      add_lines(name = ~"Export TEUs") 
    p3 = plot_ly(df, x = ~Date, y = ~Rail) %>%
      add_lines(name = ~"Rail Lifts")
    p = subplot(p1, p2, p3, nrows = 3, shareX = TRUE)%>%
      layout(autosize = T, margin = m,
             title = title_name, 
             xaxis = list(categoryorder = "array",categoryarray = df$Date,
                          rangeslider = list(type = "date")))
  })
  
  output$download_port = downloadHandler(
    
    filename = function() { 
      paste('port', '.csv', sep='') 
    },
    content = function(file) {
      out=write.csv(datasetInput_port(), file, row.names = FALSE, append = FALSE, quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", col.names = TRUE)
    }
  )
  
  datasetInput_tbt = reactive({
    col_b = switch(input$rep_type,
                   veh = 2,
                   fac = 5)
    col_e = switch(input$rep_type,
                   veh = 4,
                   fac = 10)
    tbt=tbt_all[, 1:11]
    
    if (input$rep_tbt == "value"){
      selected_data= data.frame(tbt[,1], tbt[,col_b:col_e]*1000)
    }
    else if (input$rep_tbt == "percent"){
      
      data_1=diff(tbt[,2],12)/tbt[1:(NROW(tbt[,5])-13),2]*100
      data_2=diff(tbt[,3],12)/tbt[1:(NROW(tbt[,5])-13),3]*100
      data_3=diff(tbt[,4],12)/tbt[1:(NROW(tbt[,5])-13),4]*100
      data_4=diff(tbt[,5],12)/tbt[1:(NROW(tbt[,5])-13),5]*100
      data_5=diff(tbt[,6],12)/tbt[1:(NROW(tbt[,5])-13),6]*100
      data_6=diff(tbt[,7],12)/tbt[1:(NROW(tbt[,5])-13),7]*100
      data_7=diff(tbt[,8],12)/tbt[1:(NROW(tbt[,5])-13),8]*100
      data_8=diff(tbt[,9],12)/tbt[1:(NROW(tbt[,5])-13),9]*100
      data_9=diff(tbt[,10],12)/tbt[1:(NROW(tbt[,5])-13),10]*100
      data_10=diff(tbt[,11],12)/tbt[1:(NROW(tbt[,5])-13),11]*100
      tbt_new= cbind(data_1, data_2, data_3,data_4, data_5, data_6, data_7, data_8, data_9, data_10)
      selected_data= data.frame(tbt[13:NROW(tbt[,5]),1], tbt_new[,(col_b-1):(col_e-1)])
      
    }
    
  })
  
  output$tbt_chart = renderPlotly({
    if (input$rep_tbt == "value")
      title_name="Monthly Eastbound Volume"
    else if (input$rep_tbt == "percent")
      title_name="Monthly Eastbound Volume Year Over Year % Change"
    df= datasetInput_tbt()
    df[,1]=as.array(df[,1])
    if (input$rep_type == "veh"){
      setnames(df,1:4, c("Date","Auto","Bus","Truck"))
      p1 = plot_ly(df, x = ~Date, y = ~Auto) %>%
        add_lines(name = ~"Auto")
      p2 = plot_ly(df, x = ~Date, y = ~Bus) %>%
        add_lines(name = ~"Bus") 
      p3 = plot_ly(df, x = ~Date, y = ~Truck) %>%
        add_lines(name = ~"Truck")
      p = subplot(p1, p2, p3, nrows = 3, shareX = TRUE)%>%
        layout(autosize = T, margin = m,
               title = title_name, 
               xaxis = list(categoryorder = "array",categoryarray = df$Date,
                            rangeslider = list(type = "date")))
    }
    else if (input$rep_type == "fac"){
      setnames(df,1:7, c("Date","GWB","LT","HT","BB","GB", "OBX"))
      p1 = plot_ly(df, x = ~Date, y = ~GWB) %>%
        add_lines(name = ~"George Washington") 
      p2 = plot_ly(df, x = ~Date, y = ~LT) %>%
        add_lines(name = ~"Lincoln")
      p3 = plot_ly(df, x = ~Date, y = ~HT) %>%
        add_lines(name = ~"Holland")
      p4 = plot_ly(df, x = ~Date, y = ~BB) %>%
        add_lines(name = ~"Bayonne") 
      p5 = plot_ly(df, x = ~Date, y = ~GB) %>%
        add_lines(name = ~"Goethal") 
      p6 = plot_ly(df, x = ~Date, y = ~OBX) %>%
        add_lines(name = ~"Outerbridge") 
      p = subplot(p1, p2, p3, p4, p5, p6, nrows = 6, shareX = TRUE)%>%
        layout(autosize = T, margin = m,
               title = title_name, 
               xaxis = list(categoryorder = "array",categoryarray = df$Date,
                            rangeslider = list(type = "date")))
    }
  })
  
  output$download_tbt = downloadHandler(
    filename = function() { 
      paste('tbt', '.csv', sep='') 
    },
    content = function(file) {
      out=write.csv(datasetInput_tbt(), file, row.names = FALSE, append = FALSE, quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", col.names = TRUE)
    }
  )
  
  datasetInput_avi = reactive({
    col_b = switch(input$rep_type_avi,
                   fac = 2,ftype = 6,fli_type=8, atype=11)
    col_e = switch(input$rep_type_avi,
                   fac = 5,ftype = 7,fli_type=9, atype=11)
    avi=avi_all[,1:12]
    
    if (input$rep_avi == "value"){
      selected_data= data.frame(avi[,1], avi[,col_b:col_e])
    }
    else if (input$rep_avi == "percent"){
      data_1=diff(avi[,2],12)/avi[1:(NROW(avi[,5])-13),2]*100
      data_2=diff(avi[,3],12)/avi[1:(NROW(avi[,5])-13),3]*100
      data_3=diff(avi[,4],12)/avi[1:(NROW(avi[,5])-13),4]*100
      data_4=diff(avi[,5],12)/avi[1:(NROW(avi[,5])-13),5]*100
      data_5=diff(avi[,6],12)/avi[1:(NROW(avi[,5])-13),6]*100
      data_6=diff(avi[,7],12)/avi[1:(NROW(avi[,5])-13),7]*100
      data_7=diff(avi[,8],12)/avi[1:(NROW(avi[,5])-13),8]*100
      data_8=diff(avi[,9],12)/avi[1:(NROW(avi[,5])-13),9]*100
      data_9=diff(avi[,10],12)/avi[1:(NROW(avi[,5])-13),10]*100
      data_10=diff(avi[,11],12)/avi[1:(NROW(avi[,5])-13),11]*100
      data_11=diff(avi[,12],12)/avi[1:(NROW(avi[,5])-13),12]*100
      avi_new= cbind(data_1, data_2, data_3,data_4, data_5, data_6, data_7, data_8, data_9, data_10, data_11)
      selected_data= data.frame(avi[13:NROW(avi[,5]),1], avi_new[,(col_b-1):(col_e-1)])
      
    }
  })
  
  output$avi_chart = renderPlotly({
    
    df= datasetInput_avi()
    df[,1]=as.array(df[,1])
    title_name="Aviation Monthly Statistics"
    if (input$rep_type_avi == "fac"){
      setnames(df,1:5, c("Date","JFK","LGA","EWR","SWF"))
      if (input$rep_avi == "value") y_name="Monthly Passengers"
      else if (input$rep_avi == "percent") y_name="Monthly Passenger Year over Year % Change"
      
      p1 = plot_ly(df, x = ~Date, y = ~JFK) %>%
        add_lines(name = ~"JFK") 
      p2 = plot_ly(df, x = ~Date, y = ~LGA) %>%
        add_lines(name = ~"LGA") 
      p3 = plot_ly(df, x = ~Date, y = ~EWR) %>%
        add_lines(name = ~"EWR") 
      p4 = plot_ly(df, x = ~Date, y = ~SWF) %>%
        add_lines(name = ~"SWF") 
      p = subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE)%>%
        layout(autosize = T, margin = m,
               title = y_name, 
               xaxis = list(categoryorder = "array",categoryarray = df$Date,
                            rangeslider = list(type = "date")))
    }
    else if (input$rep_type_avi == "ftype"){
      setnames(df,1:3, c("Date","Domestic","International"))
      if (input$rep_avi == "value") y_name="Monthly Freight Short Tons"
      else if (input$rep_avi == "percent") y_name="Monthly Freight Short Tons Year over Year % Change"
      
      p1 = plot_ly(df, x = ~Date, y = ~Domestic) %>%
        add_lines(name = ~"Domestic") 
      p2 = plot_ly(df, x = ~Date, y = ~International) %>%
        add_lines(name = ~"International") 
      p = subplot(p1, p2, nrows = 2, shareX = TRUE)%>%
        layout(autosize = T, margin = m,
               title = y_name, 
               xaxis = list(categoryorder = "array",categoryarray = df$Date,
                            rangeslider = list(type = "date")))
    }
    else if (input$rep_type_avi == "fli_type"){
      setnames(df,1:3, c("Date","Domestic","International"))
      if (input$rep_avi == "value") y_name="Monthly Number of Flights"
      else if (input$rep_avi == "percent") y_name="Monthly Number of Flights % Change"
      
      p1 = plot_ly(df, x = ~Date, y = ~Domestic) %>%
        add_lines(name = ~"Domestic")
      p2 = plot_ly(df, x = ~Date, y = ~International) %>%
        add_lines(name = ~"International")
      p = subplot(p1, p2, nrows = 2, shareX = TRUE)%>%
        layout(autosize = T, margin = m,
               title = y_name, 
               xaxis = list(categoryorder = "array",categoryarray = df$Date,
                            rangeslider = list(type = "date")))
    }  
    else if (input$rep_type_avi == "atype"){
      setnames(df,1:2, c("Date","Cars"))
      if (input$rep_avi == "value") y_name="Monthly Number of Parked Cars"
      else if (input$rep_avi == "percent") y_name="Monthly Number of Parked Cars Year over Year % Change"
      
      p1 = plot_ly(df, x = ~Date, y = ~Cars) %>%
        add_lines(name = ~"Parked Cars")
      p = subplot(p1, nrows = 1, shareX = TRUE)%>%
        layout(autosize = T, margin = m,
               title = y_name, 
               xaxis = list(categoryorder = "array",categoryarray = df$Date,
                            rangeslider = list(type = "date")))
    }
  })
  
  output$download_avi = downloadHandler(
    
    filename = function() { 
      paste('avi', '.csv', sep='') 
    },
    content = function(file) {
      out=write.csv(datasetInput_avi(), file, row.names = FALSE, append = FALSE, quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", col.names = TRUE)
    }
  )
  
  #PATH
  datasetInput_path = reactive({
    path=path_all[, 1:5]
    if (input$rep_path == "value"){
      selected_data= data.frame(path[,1], path[,2]*1000, path[,3:5])
    }
    else if (input$rep_path == "percent"){
      data_1=path[,2]
      data_2=path[,3]
      data_3=path[,4]
      data_4=path[,5]
      data_diff_1=diff(data_1,12)
      data_diff_2=diff(data_2,12)
      data_diff_3=diff(data_3,12)
      data_diff_4=diff(data_4,12)
      data_1=data_diff_1/path[1:(NROW(path[,3])-13),2]*100
      data_2=data_diff_2/path[1:(NROW(path[,3])-13),3]*100
      data_3=data_diff_3/path[1:(NROW(path[,3])-13),4]*100
      data_4=data_diff_4/path[1:(NROW(path[,3])-13),5]*100
      selected_data= data.frame(path[13:NROW(path[,3]),1], data_1, data_2,data_3, data_4)
    }
    
  })
  output$download_path = downloadHandler(
    
    filename = function() { 
      paste('path', '.csv', sep='') 
    },
    content = function(file) {
      out=write.csv(datasetInput_path(), file, row.names = FALSE, append = FALSE, quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", col.names = TRUE)
    }
  )
  
  output$path_chart = renderPlotly({
    df= datasetInput_path()
    df[,1]=as.array(df[,1])
    setnames(df,1:5, c("Date","Total","Weekday","Saturday","Sunday"))
    if (input$rep_path == "value"){
      y_name = "Monthly Passengers"
    }
    else if (input$rep_path == "percent"){
      y_name = "Monthly Passenger Year over Year % Change"
    }
    
    p1 = plot_ly(df, x = ~Date, y = ~Total) %>%
      add_lines(name = ~"Monthly Total")
    p2 = plot_ly(df, x = ~Date, y = ~Weekday) %>%
      add_lines(name = ~"Average Weekday")
    p3 = plot_ly(df, x = ~Date, y = ~Saturday) %>%
      add_lines(name = ~"Average Saturday")
    p4 = plot_ly(df, x = ~Date, y = ~Sunday) %>%
      add_lines(name = ~"Average Sunday")
    p = subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE)%>%
      layout(autosize = T, margin = m,
             title = y_name, 
             xaxis = list(categoryorder = "array",categoryarray = df$Date,
                          rangeslider = list(type = "date")))
  })
  
  # Ferry
  datasetInput_ferry = reactive({
    ferry_all=ferry[, 1:2]
    
    if (input$rep_ferry == "value"){
      selected_data= ferry_all
    }
    else if (input$rep_ferry == "percent"){
      data=ferry_all[,2]
      data_diff=diff(data,12)
      data=data_diff/ferry_all[1:(NROW(ferry_all[,2])-13),2]*100
      selected_data= data.frame(ferry_all[13:NROW(ferry_all[,2]),1], data)
    }
    
  })
  
  output$download_ferry = downloadHandler(
    
    filename = function() { 
      paste('ferry', '.csv', sep='') 
    },
    content = function(file) {
      out=write.csv(datasetInput_ferry(), file, row.names = FALSE, append = FALSE, quote = FALSE, sep = " ",
                    eol = "\n", na = "NA", dec = ".", col.names = TRUE)
    }
  )
  
  output$ferry_chart = renderPlotly({
    df= datasetInput_ferry()
    df[,1]=as.array(df[,1])
    if (input$rep_ferry == "value"){
      setnames(df,1:2,c("Date","Passengers"))
      title_name="Monthly Ferry Passengers"
      y_name = "Passenters"
    }
    else if (input$rep_ferry == "percent"){
      setnames(df,1:2,c("Date","Passengers"))
      title_name="Monthly Ferry Passenger Year Over Year % Change"
      y_name = "Percent Change (%)"
    }
    
    
    
    
    # Done 
    
    p = plot_ly(df, x = ~Date) %>%
      add_lines(y = ~df[,2], name = y_name)%>%
      layout(autosize = T, margin = m,
             title = title_name, 
             xaxis = list(categoryorder = "array",categoryarray = df$Date,
                          rangeslider = list(type = "date")),
             yaxis = list(title = y_name))
    
  })
  
  output$comp_chart = renderPlotly({
    
    avi=avi_all[,1:5]
    avi[,1]=as.array(avi[,1])
    df0= data.frame(as.character("air"), avi[,1], (avi[,2]+avi[,3]+avi[,4]+avi[,5]))
    
    tbt=tbt_all[241:NROW(tbt_all[,2]), 1:4]
    tbt[,1]=as.array(tbt[,1])
    df1= data.frame(as.character("veh"), tbt[,1], (tbt[,2]+tbt[,3]+tbt[,4])*1000)
    
    ferry= ferry[145:NROW(ferry[,2]), 1:2]
    ferry[,1]=as.array(ferry[,1])
    df2=data.frame(as.character("ferry"), ferry[,1], ferry[,2])
    
    path=path_all[133:NROW(path_all[,2]), 1:2]
    path[,1]=as.array(path[,1])
    df3= data.frame(as.character("path"), path[,1], path[,2]*1000)
    
    df = data.frame(df0[,2:3], df1[,3], df3[,3], df2[,3])
    
    
    if (input$rep_comp == "value"){
      title_name="Monthly Volume"
      y_name = "Volume"
    }
    else if (input$rep_comp == "percent"){
      title_name="Monthly Volume Year Over Year % Change"
      y_name = "Percent Change (%)"
      data_1=df[,2]
      data_2=df[,3]
      data_3=df[,4]
      data_4=df[,5]
      data_diff_1=diff(data_1,12)
      data_diff_2=diff(data_2,12)
      data_diff_3=diff(data_3,12)
      data_diff_4=diff(data_4,12)
      data_1=data_diff_1/df[1:(NROW(df[,3])-13),2]*100
      data_2=data_diff_2/df[1:(NROW(df[,3])-13),3]*100
      data_3=data_diff_3/df[1:(NROW(df[,3])-13),4]*100
      data_4=data_diff_4/df[1:(NROW(df[,3])-13),5]*100
      df= data.frame(df[13:NROW(df[,3]),1], data_1, data_2,data_3, data_4)
    }
    setnames(df,1:5,c("Date","air", "veh", "path", "ferry"))
    
    df.long=melt(df[,1:5], id.var=c('Date'))
    df.long[,1]=as.array(df.long[,1])
    plot_ly(df.long, x = ~Date, y=~value, color = ~variable, colors = "Accent") %>%
      filter(as.character(variable) %in% input$vars) %>%
      group_by(variable) %>%
      add_lines()%>%
      layout(autosize = T, margin = m, title = title_name, yaxis = list(title = y_name),
             xaxis = list(categoryorder = "array",categoryarray = df.long$Date,
                          rangeslider = list(type = "date")))
  })
  
  
  output$graph = renderPlot({
    # Make the data frame needed for the transport drop-down 
    econ_month$month = as.Date(econ_month$month, format = "%m/%d/%y") 
    econ_quarter$month = as.Date(econ_quarter$month, format = "%m/%d/%y") 
    names(econ_month) = c("month","Unemployment", "US_CPI", "Gasoline", 
                          "Regional_employment", "Regional_CPI", "Regional_housing_index") 
    names(econ_quarter) = c("month", "US_real_GDP", "Midtown_rent","Downtown_rent")
    
    econ_quarter = read.zoo(econ_quarter) 
    tt = as.yearmon(seq(start(econ_quarter), end(econ_quarter), "month")) # makes months, different format (unsure why needed) 
    tt = as.data.frame(na.spline(econ_quarter, as.yearmon, xout = tt)) 
    tt$month = seq(as.Date("2015/1/1"), as.Date("2018/08/01"), by="month") # add date
    
    econ = merge(econ_month,tt,by="month") 
    econ = econ[complete.cases(econ), ]
    rm(econ_month,econ_quarter,tt) 
    ggplot(econ[!is.na(econ),]) + 
      geom_line(aes_string(x = 'month', y = input$opti), size = 0.25) + 
      geom_point(aes_string(x = 'month', y = input$opti),size = 2, shape = 1) 
  })
  
  
  #  output$graph = renderPlot({
  #    econ_month$month = as.Date(econ_month$month, format = "%m/%d/%y") 
  #    econ_quarter$month = as.Date(econ_quarter$month, format = "%m/%d/%y") 
  #    names(econ_month) = c("month","Unemployment", "US_CPI", "Gasoline", 
  #                          "Regional_employment", "Regional_CPI", "Regional_housing_index") 
  #    names(econ_quarter) = c("month", "US_real_GDP", "Midtown_rent","Downtown_rent")
  #    
  #    econ_quarter = read.zoo(econ_quarter) 
  #    tt = as.yearmon(seq(start(econ_quarter), end(econ_quarter), "month")) # makes months, different format (unsure why needed) 
  #    tt = as.data.frame(na.spline(econ_quarter, as.yearmon, xout = tt)) 
  #    tt$month = seq(as.Date("2015/1/1"), as.Date("2018/08/01"), by="month") # add date
  #    
  #    econ = merge(econ_month,tt,by="month") 
  #    econ = econ[complete.cases(econ), ]
  #    rm(econ_month,econ_quarter,tt) 
  #    ggplot(econ[!is.na(econ),]) + 
  #      geom_line(aes_string(x = 'month', y = input$opti), size = 0.25) + 
  #      geom_point(aes_string(x = 'month', y = input$opti),size = 2, shape = 1) 
  #  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

