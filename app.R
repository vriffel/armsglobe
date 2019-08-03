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
        tableOutput(outputId = "outtable"),
        uiOutput("outui"),
        tableOutput("table"),
                                        #textOutput("text"), #DEL
        actionButton("all", "Select All"),
        actionButton("none", "Select None"),
        actionButton("button", "show")
    ), #sidebar
    mainPanel(plotOutput(outputId = "outplot"))
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
        assign("newdata", newdata, envir = globalenv())
                                        #output$text <- renderText({year_select})
        output$outui <- renderUI({
            fluidRow(
                checkboxGroupInput(inputId = "e_country",
                                   label = "Select the countries",
                                   choices = mychoices,
                                   selected = mychoices))
        })
        output$outplot <- renderPlot({
            ggplot(data = newdata,
                   aes(x = year, y = v, col = e, shape = wc)) +
                geom_point()
        })
    })
    observeEvent(input$all, {
        updateCheckboxGroupInput(session, "e_country",
                                 choices = mychoices,
                                 selected = mychoices)
    })
    observeEvent(input$none, {
        updateCheckboxGroupInput(session, "e_country",
                                 choices = mychoices,
                                 selected = NULL)
    })
}

shinyApp(ui, server)
