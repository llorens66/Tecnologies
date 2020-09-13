
library(tidyverse)
library(xgboost)
library(caret)
library(gridExtra)
library(grid)
library(rsconnect)
library(shiny)
library(shinyWidgets)

cards_dim <- readRDS("./cards_dim1.rds")

xgb <- readRDS("./xgb_final_model.rds")

urls <- cards_dim$iconUrls$medium 

cards <- cards_dim$name

df <- data.frame(cards1 = cards)

df$url <- map2(urls,cards, .f = function(urls, cards){
    sprintf(paste("<img src='", urls, "' width=30px><div class='jhr'>%s</div></img>", sep = ""), cards)
}) %>% unlist()

# Define UI for application 
ui <- fluidPage(titlePanel(title = "Predict Win Clash Royale with AI"),
                fluidRow(
                    column(3,
                
                                           tags$head(tags$style(".jhr{
                                                                display: inline;
                                                                vertical-align: middle;
                                                                padding-left: 10px;
                                                                }")),
                                           
                                           sliderInput("Trophies1", 
                                                       label = "Trophies player 1",
                                                       min = 4000,
                                                       max = 8000,
                                                       value = 6000),
                           
                           
                                           pickerInput(inputId = "cards1",
                                                       label = "card 1", 
                                                       choices = df$cards1,
                                                       multiple = FALSE,
                                                       selected = TRUE,
                                                       choicesOpt = list(content = df$url),
                                                       options = list(`actions-box` = TRUE,
                                                                     size = 10, 
                                                                    `selected-text-format` = "count > 9"
                                                                     )
                                                       ),
                                           pickerInput(inputId = "cards2",
                                                       label = "card 2", 
                                                       choices = df$cards1,
                                                       multiple = FALSE,
                                                       selected = TRUE,
                                                       choicesOpt = list(content = df$url),
                                                       options = list(`actions-box` = TRUE,
                                                                      size = 10, 
                                                                      `selected-text-format` = "count > 9"
                                                                      )
                                                        ),
                                           pickerInput(inputId = "cards3",
                                                       label = "card 3", 
                                                       choices = df$cards1,
                                                       multiple = FALSE,
                                                       selected = TRUE,
                                                       choicesOpt = list(content = df$url),
                                                       options = list(`actions-box` = TRUE,
                                                                      size = 10, 
                                                                      `selected-text-format` = "count > 9"
                                                                     )
                                                       ),
                                           pickerInput(inputId = "cards4",
                                                       label = "card 4", 
                                                       choices = df$cards1,
                                                       multiple = FALSE,
                                                       selected = TRUE,
                                                       choicesOpt = list(content = df$url),
                                                       options = list(`actions-box` = TRUE,
                                                                      size = 10, 
                                                                      `selected-text-format` = "count > 9"
                                                                     )
                                                      ),
                                           pickerInput(inputId = "cards5",
                                                       label = "card 5", 
                                                       choices = df$cards1,
                                                       multiple = FALSE,
                                                       selected = TRUE,
                                                       choicesOpt = list(content = df$url),
                                                       options = list(`actions-box` = TRUE,
                                                                      size = 10, 
                                                                      `selected-text-format` = "count > 9"
                                                                     )
                                                      ),
                                           pickerInput(inputId = "cards6",
                                                       label = "card 6", 
                                                       choices = df$cards1,
                                                       multiple = FALSE,
                                                       selected = TRUE,
                                                       choicesOpt = list(content = df$url),
                                                       options = list(`actions-box` = TRUE,
                                                                      size = 10, 
                                                                      `selected-text-format` = "count > 9"
                                                                     )
                                                      ),
                                           pickerInput(inputId = "cards7",
                                                       label = "card 7", 
                                                       choices = df$cards1,
                                                       multiple = FALSE,
                                                       selected = TRUE,
                                                       choicesOpt = list(content = df$url),
                                                       options = list(`actions-box` = TRUE,
                                                                      size = 10, 
                                                                      `selected-text-format` = "count > 9"
                                                                     )
                                                      ),
                                           pickerInput(inputId = "cards8",
                                                       label = "card 8", 
                                                       choices = df$cards1,
                                                       multiple = FALSE,
                                                       selected = TRUE,
                                                       choicesOpt = list(content = df$url),
                                                       options = list(`actions-box` = TRUE,
                                                                      size = 10, 
                                                                      `selected-text-format` = "count > 9"
                                                                     )
                                                      )
                    ),
                    column(6,
                           fluidRow(
                                           uiOutput(outputId = "Mazo1"
                                                   ),
                                           tableOutput(outputId = "predict"
                                                   ),
                                           uiOutput(outputId = "Mazo2"
                                                  )
                           )
                    ),
                    column(3,
                           sliderInput("Trophies2", 
                                       label = "Trophies player 2",
                                       min = 4000,
                                       max = 8000,
                                       value = 6000),
                           
                           pickerInput(inputId = "cards9",
                                       label = "card 1", 
                                       choices = df$cards1,
                                       multiple = FALSE,
                                       selected = TRUE,
                                       choicesOpt = list(content = df$url),
                                       options = list(`actions-box` = TRUE,
                                                      size = 10, 
                                                      `selected-text-format` = "count > 9"
                                       )
                           ),
                           pickerInput(inputId = "cards10",
                                       label = "card 2", 
                                       choices = df$cards1,
                                       multiple = FALSE,
                                       selected = TRUE,
                                       choicesOpt = list(content = df$url),
                                       options = list(`actions-box` = TRUE,
                                                      size = 10, 
                                                      `selected-text-format` = "count > 9"
                                       )
                           ),
                           pickerInput(inputId = "cards11",
                                       label = "card 3", 
                                       choices = df$cards1,
                                       multiple = FALSE,
                                       selected = TRUE,
                                       choicesOpt = list(content = df$url),
                                       options = list(`actions-box` = TRUE,
                                                      size = 10, 
                                                      `selected-text-format` = "count > 9"
                                       )
                           ),
                           pickerInput(inputId = "cards12",
                                       label = "card 4", 
                                       choices = df$cards1,
                                       multiple = FALSE,
                                       selected = TRUE,
                                       choicesOpt = list(content = df$url),
                                       options = list(`actions-box` = TRUE,
                                                      size = 10, 
                                                      `selected-text-format` = "count > 9"
                                       )
                           ),
                           pickerInput(inputId = "cards13",
                                       label = "card 5", 
                                       choices = df$cards1,
                                       multiple = FALSE,
                                       selected = TRUE,
                                       choicesOpt = list(content = df$url),
                                       options = list(`actions-box` = TRUE,
                                                      size = 10, 
                                                      `selected-text-format` = "count > 9"
                                       )
                           ),
                           pickerInput(inputId = "cards14",
                                       label = "card 6", 
                                       choices = df$cards1,
                                       multiple = FALSE,
                                       selected = TRUE,
                                       choicesOpt = list(content = df$url),
                                       options = list(`actions-box` = TRUE,
                                                      size = 10, 
                                                      `selected-text-format` = "count > 9"
                                       )
                           ),
                           pickerInput(inputId = "cards15",
                                       label = "card 7", 
                                       choices = df$cards1,
                                       multiple = FALSE,
                                       selected = TRUE,
                                       choicesOpt = list(content = df$url),
                                       options = list(`actions-box` = TRUE,
                                                      size = 10, 
                                                      `selected-text-format` = "count > 9"
                                       )
                           ),
                           pickerInput(inputId = "cards16",
                                       label = "card 8", 
                                       choices = df$cards1,
                                       multiple = FALSE,
                                       selected = TRUE,
                                       choicesOpt = list(content = df$url),
                                       options = list(`actions-box` = TRUE,
                                                      size = 10, 
                                                      `selected-text-format` = "count > 9"
                                       )
                           )
                           )
                )
)
                    
 

# Define server logic required 

server <- function(input, output) {
    output$Mazo1 <- renderUI({
        
        list(
        tags$img(src = urls[cards_dim$name == input$cards1], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards2], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards3], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards4], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards5], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards6], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards7], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards8], height = "100px", width = "80px")
            )
        })

    output$Mazo2 <- renderUI({
        
        list(
        tags$img(src = urls[cards_dim$name == input$cards9], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards10], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards11], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards12], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards13], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards14], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards15], height = "100px", width = "80px"),
        tags$img(src = urls[cards_dim$name == input$cards16], height = "100px", width = "80px") 
        
        )    
           
        })
    
    output$predict <- renderTable({
        testing <- matrix(c(((input$Trophies1-4000)/(8000-4000)), 
                            ((input$Trophies2-4000)/(8000-4000)),
                      c((ifelse(grepl(paste("^",input$cards1,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards2,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards3,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards4,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards5,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards6,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards7,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards8,sep = ''), cards),1,0)),
                        (ifelse(grepl(paste("^",input$cards9,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards10,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards11,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards12,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards13,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards14,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards15,sep = ''), cards),1,0)+
                         ifelse(grepl(paste("^",input$cards16,sep = ''), cards),1,0)))), nrow = 1) #%>% as.data.frame()
        
        names(testing) <- readRDS("var_names")
        
        data.frame("Player_1" = 1 - predict(xgb, testing, type = "prob"),
                   "Player_2" = predict(xgb, testing, type = "prob"))
        
              
    })
}    

# Run the application 
shinyApp(ui = ui, server = server)
