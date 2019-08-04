library(shiny)
library(shinydashboard)
source("globalarm.R")

ui <- dashboardPage(
    dashboardHeader(title = "Small Arms and Ammunition - Imports & Exports"),
    dashboardSidebar(
        hr(),
        sidebarMenu(id = "tabs",
                    menuItem("Plot", tabName = "plot",
                             icon = icon("chart-bar"), selected = TRUE),
                    menuItem("Table", tabName = "table", icon = icon("table")),
                    menuItem("About", tabName = "about", icon = icon("question"))
                    )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "plot",
                    fluidRow(
                        column(width = 4,
                               tabPanel(h5("parameters"),
                                        selectInput(inputId = "i_country",
                                                    label = "Select the country",
                                                    choices =
                                                        sort(unique(all$imp))),
                                        sliderInput(inputId = "slider",
                                                    label = "Select the years",
                                                    min = 1992, max = 2010,
                                                    value = c(1992, 2010)),
                                        checkboxGroupInput(inputId = "use",
                                                           label =
                                                               "Select the use",
                                                           choices = c("ammo",
                                                                       "mil",
                                                                       "civ"),
                                                           selected = c("ammo",
                                                                        "mil",
                                                                        "civ")),
                                        uiOutput("outui"),
                                        actionButton("all", "Select All"),
                                        actionButton("none", "Select None"),
                                        actionButton("button", "Plot")
                                        )
                               ),
                        column(width = 8,
                               box(width = NULL, plotOutput("outplot",
                                                            height = "500px"),
                                   collapsible = TRUE,
                                   title = "plot", status = "primary",
                                   solidHeader = TRUE)
                               )
                    )),
            tabItem(tabName = "table",
                    box(width = NULL, status = "primary", solidHeader = TRUE,
                        title = "Table", br(), br(),
                        tableOutput("table")
                        )
                    )
        )
    )
)

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
            checkboxGroupInput(inputId = "e_country",
                               label = "Select the countries",
                               choices = mychoices,
                               selected = NULL)
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
        output$table <- renderTable({
            names(newdata) <- c("Years", "Importer", "Use", "Exporter",
                                "Total Value ($)")
            newdata
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
