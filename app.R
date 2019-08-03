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
        tableOutput("table"),
        textOutput("text"), #DEL
        actionButton("all", "Select All/none"),
        actionButton("button", "show")
    )#sidebar
)#fluipage

server <- function(input, output, session) {
    observeEvent(input$button, {
        year_select <- as.numeric(input$slider)
        cty_select <- input$i_country
        newdata <- all %>%
            filter(
                year %in% seq(from = year_select[1], to = year_select[2],
                              by = 1) & imp == cty_select)
        assign("mychoices", unique(newdata$e), envir = globalenv())
        output$text <- renderText({year_select})
        output$outui <- renderUI({
            fluidRow(
                checkboxGroupInput(inputId = "e_country",
                                   label = "Select the countries",
                                   choices = mychoices,
                                   selected = mychoices))
        })
    })
    observeEvent(input$all, {
        updateCheckboxGroupInput(session, "e_country",
                                 choices = mychoices,
                                 selected = mychoices)
    })
}

shinyApp(ui, server)
