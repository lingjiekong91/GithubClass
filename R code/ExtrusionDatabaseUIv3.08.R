#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#make a test
library(shiny)
library(bootstrap)
library(jpeg)
library(ggplot2)
library(DT)

#Creating variables across all sessions
path <- "C:/Users/correaf/Downloads/PPS_document_contents/ListOfSheets/GTE"
single_pps_file <- "Single PPS Data Filled GTE.csv"
single_tari_file <- "Single Tari Data.csv"
multi_pps_file <- "Multi-Layered PPS Data Filled GTE.csv"
tapered_pps_file <- "Tapered PPS Data Filled GTE.csv"
resin_file <- "Resin Information.csv"
screw_file <- "Screw Properties.csv"

single_pps_pathfile <- paste(path, single_pps_file, sep = "/")
single_tari_pathfile <- paste(path, single_tari_file, sep = "/")
multi_pps_pathfile <- paste(path, multi_pps_file, sep = "/")
tapered_pps_pathfile <- paste(path, tapered_pps_file, sep = "/")
resin_pathfile <- paste(path, resin_file, sep = "/")
screw_pathfile <- paste(path, screw_file, sep = "/")

single_pps_data <- read.csv(single_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                            check.names = FALSE)
single_tari_data <- read.csv(single_tari_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                             check.names = FALSE)
multi_pps_data <- read.csv(multi_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                           check.names = FALSE)
tapered_pps_data <- read.csv(tapered_pps_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                             check.names = FALSE)
resin_data <- read.csv(resin_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)
screw_data <- read.csv(screw_pathfile, header = TRUE, stringsAsFactors = FALSE, 
                       check.names = FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  
  titlePanel("Extrusion Application"),
  
  tabsetPanel(id = "application",
              tabPanel('Part Catalog',
                       sidebarPanel(
                         conditionalPanel(
                           'input.dataset === "Single Extrusion PPS Data"',
                           checkboxGroupInput('show_vars1', 'Columns to Show:',
                                              choices = names(single_pps_data),
                                              selected = c("Part Number", "Part Description", 
                                                           "Resin Number", "Resin Description",
                                                           "Die Size", "Tip Size",
                                                           "Inner Diameter (in)", "Outer Diameter (in)",
                                                           "Wall Thickness (in)", "Length (in)")
                           )
                         ),
                         conditionalPanel(
                           'input.dataset === "Multi-Layered Exutrusion PPS Data"',
                           checkboxGroupInput('show_vars2', 'Columns to Show:',
                                              choices = names(multi_pps_data), 
                                              c("Part Number", "Part Description", 
                                                "Resin Number", "Resin Description",
                                                "Die Size", "Tip Size",
                                                "Inner Diameter (in)", "Outer Diameter (in)",
                                                "Inner Wall Thickness (in)", "Middle Wall Thickness (in)",
                                                "Outer Wall Thickness (in)",
                                                "Total Wall Thickness (in)", "Total Length (in)")
                           )
                         ),
                         conditionalPanel(
                           'input.dataset === "Tapered Extrusion PPS Data"',
                           checkboxGroupInput('show_vars3', 'Columns to Show:',
                                              choices = names(tapered_pps_data), 
                                              c("Part Number", "Part Description", 
                                                "Resin Number", "Resin Description",
                                                "Die Size", "Tip Size",
                                                "Proximal Inner Diameter (in)", "Proximal Inner Diameter (in)",
                                                "Proximal Wall Thickness (in)",
                                                "Distal Inner Diameter (in)", "Distal Outer Diameter (in)",
                                                "Distal Wall Thickness (in)",
                                                "Proximal Length (in)", "Transition Length (in)", 
                                                "Transition Length (in)", "Total Length (in)")
                           )
                         )
                       ),
                       
                       mainPanel(
                         tabsetPanel(
                           id = 'dataset',
                           tabPanel('Single Extrusion PPS Data', DT::dataTableOutput('mytable1')),
                           tabPanel('Multi-Layered Exutrusion PPS Data', DT::dataTableOutput('mytable2')),
                           tabPanel('Tapered Extrusion PPS Data', DT::dataTableOutput('mytable3'))
                         )
                       ) #end mainPanel
              ),#end tabPanel
              
              tabPanel('Output',
                       sidebarPanel(
                         conditionalPanel(
                           'input.output_dataset === "MES Data"',
                           checkboxGroupInput('show_vars4', 'Columns to Show:',
                                              choices = names(single_tari_data),
                                              selected = names(single_tari_data)
                           )
                         )
                       ),#end sidebarPanel
                       
                       mainPanel(
                         tabsetPanel(
                           id = 'output_dataset',
                           tabPanel('MES Data', DT::dataTableOutput('mytable4'))
                         )
                       ) #end mainPanel
              ), #end tabPanel for 'Output'
              
              tabPanel('Extra',
                       sidebarPanel(
                         
                         conditionalPanel(
                           'input.extra_dataset === "Resin Data"',
                           checkboxGroupInput('show_vars5', 'Columns to Show:',
                                              choices = names(resin_data),
                                              selected = names(resin_data)
                           )
                         ),
                         conditionalPanel(
                           'input.extra_dataset === "Screw Data"',
                           checkboxGroupInput('show_vars6', 'Columns to Show:',
                                              choices = names(screw_data),
                                              selected = names(screw_data)
                           )
                         )
                       ),#end sidebarPanel
                       
                       mainPanel(
                         tabsetPanel(
                           id = 'extra_dataset',
                           tabPanel('Resin Data', DT::dataTableOutput('mytable5')),
                           tabPanel('Screw Data', DT::dataTableOutput('mytable6'))
                         )
                       ) #end mainPanel
              ) #end tabPanel for 'Extra'
              
  )#end tabsetPanel for part catalog
  
  
) #end fluidPage




server <- function(input, output, session) {
  
  e1 <- new.env(
    #variables to contain:
    # vectorofAttributes - contains the attributes that were previously selected by the user
  ) #creates a new environment to store instance variables
  
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(single_pps_data[, input$show_vars1], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(multi_pps_data[, input$show_vars2], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(tapered_pps_data[, input$show_vars3], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  output$mytable4 <- DT::renderDataTable({
    DT::datatable(single_tari_data[, input$show_vars4], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  output$mytable5 <- DT::renderDataTable({
    DT::datatable(resin_data[, input$show_vars5], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  output$mytable6 <- DT::renderDataTable({
    DT::datatable(screw_data[, input$show_vars6], 
                  options = list(orderClasses = TRUE, 
                                 columnDefs = list(list(className = 'dt-center', 
                                                        targets = "_all"))), 
                  filter = "top")
  })
  
  
  
} #end server

# Run the application 
shinyApp(ui = ui, server = server)

