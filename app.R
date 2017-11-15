# main file
library(shiny)
library(plotly)
library(leaflet)
library(geojson)
library(geojsonio)


#datos clg
clg_data <- read.csv("clg.csv")
comunas <- geojsonio::geojson_read("comunas.json", what = "sp")

# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel(tags$img(src="chg-logo.png", height="40px"),"Data Analysis"),

    sidebarLayout(
        sidebarPanel(width = 3,
         tabsetPanel( type="tab",
            tabPanel ("Load file",
              tags$small(paste0(
                "Note: Load data in a '.csv' file. Include the ID of the county",
                " "
              )),
              checkboxInput('header', 'First row as header', TRUE),
              radioButtons('sep', 'Separator field character',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
              radioButtons('quote', 'Text quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
              #carga de archivo externo
              fileInput('datafile', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
              )
            ),
            
            tabPanel("Analysis",
                 selectInput(inputId = "xax","X axis",
                        choices = colnames(clg_data[5:11]), selected = "K3_CEU"
                 ),
                 textInput("namex", "Column name x"),
                 selectInput(inputId = "yax","Y Axis",
                             choices = colnames(clg_data[5:11]), selected = "K4_CEU"
                 ),
                 textInput("namey", "Column name y"),
                
                 submitButton("Apply changes")
            )
         ),

          uiOutput("fromCol"),
          uiOutput("toCol")

     ), # sidebarpanel

      # Show a plot of the generated distribution
      mainPanel(width = 9,
     
         tabsetPanel( type="tab",
            tabPanel ( "Chart" ,
                plotlyOutput("xy", height = 500)
            ),
            tabPanel("Ancestry",
                tableOutput(outputId = "table.ancestry")
                ),
            tabPanel("Table",
                tableOutput(outputId = "table.output")
                ),
            tabPanel("Map",
                tags$p(" "),
                leafletOutput("mymap", height = 650)
                )
        )
      )
     
   )

)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selectedData <- reactive({
    clg_data[, c(input$xax, input$yax)]
  })
  
   #output$namex <- input$namex
  # output$namey <- renderText({ input$namey })
  
  # table ancestry
  output$table.ancestry <- renderTable({
    tbl <- clg_data
    return(tbl)
  })
  
  # capture file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  output$fromCol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=colnames(df)
    #names(items)=itemsres
    selectInput("from", "From col:",items)
  })
  
  output$toCol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=colnames(df)
    #names(items)=items
    selectInput("to", "To col:",items)
  })
  
  #table
  output$table.output <- renderTable({
    inFile <- input$datafile
    if (is.null(inFile))
      return(NULL)
    tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep,  dec = input$dec)
    return(tbl)
  })
  
  #chart
  
  output$xy <- renderPlotly({
    xvar <- clg_data[,input$xax]
    yvar <- clg_data[,input$yax]
    x <- list(title = "xx")
    y <- list(title = "yy")
    clg_data %>%
      plot_ly( x = ~xvar, y= ~yvar, type="scatter",  mode="markers", text = ~County) 
  })
  
  
  #mapa
  
  bins <- c(0, 1000, 2000, 4000, 6000,  8000,  10000,  12000, 15000, Inf)
  pal <- colorBin("YlOrRd", domain = comunas$CODIGO, bins = bins)
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  
  output$mymap <- renderLeaflet({
    leaflet(comunas) %>%
      setView(lng = -70.8067798, lat = -33.5107981, zoom = 6) %>%
      addTiles() %>%
      addPolygons(stroke = TRUE, color = "#000", weight = 1, smoothFactor = 0.3,
                  fillColor= ~pal(CODIGO),
                  fillOpacity = 0.5, 
                  dashArray = "3"
      ) %>%
     addLegend(pal = pal, values = ~CODIGO, opacity = 0.7, title = NULL, position = "bottomright")
    
  })
  # fin mapa
}


# Run the application
shinyApp(ui = ui, server = server)
