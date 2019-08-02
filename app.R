library(shiny)
source("globalarm.R")

ui <- fluidPage(
    titlePanel(NULL),
    sidebarPanel(
        selectInput(inputId = "i_country",
                    label = "Select the country",
                    choices = unique(all$imp)),
        sliderInput(inputId = "slider", label = "Select the years",
                    min = 1992, max = 2010, value = c(1992, 2010)),
        checkboxGroupInput(inputId = "use", label = "Select the use",
                           choices = c("ammo", "mil", "civ")),
        plotOutput(outputId = "outplot"),
        tableOutput(outputId = "outtable"),
        uiOutput("outui"),
        tableOutput("text"),
        actionButton("button2", "showw"),
        actionButton("button", "show")
    )#sidebar
)#fluipage

server <- function(input, output, session) {
    observe({
        year_select <- as.numeric(input$slider)
        cty_select <- input$i_country
        newdata <- all %>%
            filter(
                seq(from = year[1], to = year[2], by = 1) %in%
                year_select & imp == cty_select)
        #output$text <- renderTable({newdata})
        assign("newdata", newdata, envir = globalenv())
    })
    observe({
        output$outui <- renderUI({
            fluidRow(
                checkboxGroupInput(inputId = "e_country",
                                   label = "Select the countries",
                                   choices = unique(newdata$e),
                                   selected = unique(newdata$e)),
                checkboxInput("all", "Select All/None", value = TRUE))
        })
    })
    observeEvent(input$button2, {
        updateCheckboxGroupInput(session, "e_country",
                                 choices = unique(newdata$e),
                                 selected = if (input$all) {unique(newdata$e)})
    })
}

shinyApp(ui, server)
