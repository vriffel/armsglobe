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
                           choices = c("ammo", "mil", "civ"),
                           selected = c("ammo", "mil", "civ")),
        tableOutput(outputId = "outtable"),
        uiOutput("outui"),
        #textOutput("text"), #DEL
        actionButton("all", "Select All"),
        actionButton("none", "Select None"),
        actionButton("button", "show")
    ), #sidebar
    mainPanel(plotOutput(outputId = "outplot"), tableOutput("table"))
)#fluipage

server <- function(input, output, session) {
    observeEvent(c(input$i_country, input$use, input$slider), {
        year_select <- as.numeric(input$slider)
        cty_select <- input$i_country
        use_select <- input$use
        newdata <- all %>%
            filter(
                year %in% seq(from = year_select[1], to = year_select[2],
                              by = 1) & imp %in% cty_select & wc %in% use_select)
        assign("mychoices", unique(newdata$e), envir = globalenv())
        output$outui <- renderUI({
            fluidRow(
                checkboxGroupInput(inputId = "e_country",
                                   label = "Select the countries",
                                   choices = mychoices,
                                   selected = NULL))
        })
        assign("newdata", newdata, envir = globalenv())
    }) #qualquer att
    observeEvent(input$button, {
        newdata <- newdata %>% filter(e %in% input$e_country)
        output$outplot <- renderPlot({
            ggplot(data = newdata,
                   aes(x = year, y = v, col = e, shape = wc)) +
                geom_point()
        })
        output$table <- renderTable({newdata})
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
