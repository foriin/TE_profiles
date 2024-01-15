library(shiny)
library(shinydashboard)
library(GenomicRanges)
library(ggbio)
library(dplyr)
library(ggplot2)


server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  myData <- reactiveVal()
  storedPlots <- reactiveValues(all = NULL, individual = list())
  plotGenerationStatus <- reactiveVal(FALSE)
  
  processFile <- function(filePath) {
    gr_data <- tryCatch({
      df <- read.table(filePath,header = F, sep = '\t') %>% 
        setNames(c("chr", "start", "end", "score")) %>% 
        mutate(start = start + 1)
      makeGRangesFromDataFrame(df, keep.extra.columns = T)
    }, error = function(e) {
      NULL
    })
    req(gr_data)
    myData(gr_data)
    updateSelectInput(session, "chromosome_select",
                      choices = c("all", levels(as.factor(seqlevels(gr_data)))))
    
    # Set plot generation status to TRUE
    plotGenerationStatus(TRUE)
    
    # Generate and store plots
    storedPlots$all <- ggplot(gr_data) + 
      geom_bar(stat = 'identity', aes(y = score)) +
      facet_wrap(~seqnames, scales = "free", ncol = 4) + 
      theme_clear()
    
    for(chr in levels(as.factor(seqnames(gr_data)))) {
      filtered_data <- gr_data[seqnames(gr_data) == chr]
      storedPlots$individual[[chr]] <- ggplot(filtered_data) + 
        geom_rect(stat = 'identity', aes(xmin = start, xmax = end, ymin = 0, ymax = score)) +
        theme_clear()
    }
    
    # Set plot generation status to FALSE
    plotGenerationStatus(FALSE)
  }
  observeEvent(input$file_myData, {
    if(input$select_upload == "local") {
      processFile(input$file_myData$datapath)
    }
  })
  
  observeEvent(input$submit_url, {
    if(input$select_upload == "online" && nzchar(input$url_myData)) {
      tempFile <- tempfile(fileext = ".bedgraph")
      download.file(input$url_myData, destfile = tempFile, mode = "wb")
      processFile(tempFile)
    }
  }, ignoreInit = TRUE)
  
  
  output$chromosome_selector <- renderUI({
    req(storedPlots)
    if(is.null(myData())) return(NULL)  # Don't display if myData is NULL
    selectInput(
      inputId = "chromosome_select",
      label = "Select Chromosome",
      choices = c("all", levels(as.factor(seqlevels(myData()))))
    )
  })
  
  output$plotTitle <- renderUI({
    req(input$chromosome_select)
    selected_chromosome <- input$chromosome_select
    defaultTitle <- if(selected_chromosome == "all") {
      "Profile for all TEs"
    } else {
      paste("Profile for", selected_chromosome)
    }
    
    textInput("plotTitle", "Plot Title", value = defaultTitle)
  })
  # Reactive expression for determining plot height
  plotHeight <- reactive({
    req(input$chromosome_select)
    if(input$chromosome_select == "all") {
      return(3000)  # Height for "all" chromosomes
    } else {
      return(500)   # Height for a specific chromosome
    }
  })
  # Reactive expression for determining plot width
  plotWidth <- reactive({
    req(input$chromosome_select)
    if(input$chromosome_select == "all") {
      return(1000)  # Height for "all" chromosomes
    } else {
      return("auto")   # Height for a specific chromosome
    }
  })
  output$ggbioPlot <- renderPlot({
    req(myData(), input$chromosome_select, input$plotTitle, storedPlots,
        !plotGenerationStatus())
    selected_chromosome <- input$chromosome_select
    # Get the plot from stored plots without redrawing
    plot <- if(selected_chromosome == "all") {
      req(storedPlots$all)
      storedPlots$all
    } else {
      req(storedPlots$individual[[selected_chromosome]])
      storedPlots$individual[[selected_chromosome]]
    }
    
    yAxisLabel <- input$yAxisLabel
    plotTitle <- input$plotTitle
    
    plot + ggtitle(plotTitle) + ylab(yAxisLabel)
  }, height = function(){
    plotHeight() # Use the reactive value for plot height
  }, width = function(){
    plotWidth()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      req(myData(), input$chromosome_select)
      selected_chromosome <- input$chromosome_select
      paste("teProfile_", selected_chromosome, if(input$downloadType == "PDF") ".pdf" else ".png", sep = "")
    },
    content = function(file) {
      req(storedPlots, input$chromosome_select)
      selected_chromosome <- input$chromosome_select
      yAxisLabel <- input$yAxisLabel
      plotTitle <- input$plotTitle
      # Set different dimensions based on the chromosome selection
      if(selected_chromosome == "all") {
        width <- 1000
        height <- 3000
        plotus <- storedPlots$all + ggtitle(plotTitle) + ylab(yAxisLabel)
      } else {
        width <- 900
        height <- 500
        plotus <- storedPlots$individual[[selected_chromosome]] + ggtitle(plotTitle) + ylab(yAxisLabel)
      }
      
      if(input$downloadType == "PDF") {
        pdf(file, width = width/100, height = height/100)
        print(plotus)
        dev.off()
      } else {
        png(file, width = width, height = height)
        print(plotus)
        dev.off()
      }
    }
  )
}


ui <- dashboardPage(
  dashboardHeader(title = "Genomic Data Visualization"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Data upload", tabName = "data", icon = icon("upload")),
      conditionalPanel(
        "input.sidebarmenu === 'data'",
        selectInput(
          inputId = "select_upload",
          label = "Please select an option",
          choices = c("local" = "local", "online" = "online"),
          selected = "local"
        ),
        conditionalPanel(
          condition = "input.select_upload === 'local'",
          fileInput(inputId = "file_myData", label = "Choose BedGraph file", accept = c(".bed", ".bedgraph"))
        ),
        conditionalPanel(
          condition = "input.select_upload === 'online'",
          textInput(inputId = "url_myData", label = "Enter URL for BedGraph file", value = ""),
          actionButton("submit_url", "Upload")
        ),
        uiOutput("chromosome_selector")
      )
    ),
    textInput("yAxisLabel", "Y-axis Label", value = "RPM"),
    uiOutput("plotTitle"),
    downloadButton("downloadPlot", "Save Plot", class = "btn-primary", style = "margin: 5px 5px 5px 15px;"),
    radioButtons("downloadType", "Select File Type", choices = c("PNG", "PDF"))
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      #ggbioPlot { 
        height: 900px; 
        overflow-y: auto; 
      }
    "))),
    tabItems(
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "TE Profile", solidHeader = TRUE, collapsible = T,
            width = 12, plotOutput(outputId = "ggbioPlot", height = "900px", width = "100%"))
        )
      )
    )
  )
)

shinyApp(ui, server)