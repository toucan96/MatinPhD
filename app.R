# Moudi Science
# 
# Shiny app written by Matin Mahmoudi (Roslin Insitute)
#
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Layout options: https://shiny.posit.co/r/articles/build/layout-guide/

library(shiny)
library(bslib)
library(MatinPhD)


source("functions.R") #functions




# Define UI ----
ui <- fluidPage(
  titlePanel("Quick functions"),
  theme = bs_theme(bootswatch = "flatly"),
  card(
    card_header("Complement"),
    textInput("complement_input", label = "Find complement sequence", value="Input sequence"),
    actionButton("complement_submit", "Submit")
  ),
  uiOutput("complement_output"),
  card(
    card_header("Other function"),
    textInput("orf_input", label = "Find amino acid sequence", value="Input sequence"),
    layout_columns(
      radioButtons("orf_value", label= "Frame", choices = 
                     list("Frame 1" = 1, "Frame 2" = 2, "Frame 3" = 3)),
      radioButtons("orf_style", label= "Output style", choices = 
                     list("One-letter code" = "oneletter", "Three-letter code" = "threeletter"))
    ),
    actionButton("orf_submit", "Submit")
  ),
  uiOutput("peptide_output"),
  
)       



# Define server logic ----
server <- function(input, output) {
  
  comp <- eventReactive(input$complement_submit, {
    rComplement(input$complement_input)
    
  })
  output$complement_output <- renderUI({
    HTML(
      paste("Complement: <br />",
            comp()$complement, "<br /><br />",
            "Reverse complement: <br />",
            comp()$reverse_complement)
    )
  })
  
  pept <- eventReactive(input$orf_submit, {
    aa_translation(sequence=input$orf_input, frame=as.integer(input$orf_value), output_style=input$orf_style)
    
  })
  output$peptide_output <- renderUI({
    HTML(
      paste0("Peptide sequence for frame ", input$orf_value, ": <br />",
             pept()
      )
    )
  })
  
  
}





# Run the application 
shinyApp(ui = ui, server = server)
