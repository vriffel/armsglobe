library(shiny)
library(shinydashboard)
source("armglobal.R")

ui <- dashboardPage(
    dashboardHeader(title = "Arms Globe"),
    dashboardSidebar(
        hr(),
        sidebarMenu(id = "tabs",
                    menuItem("Plot", tabName = "plot",
                             icon = icon("chart-bar")),
                    menuItem("Table", tabName = "table", icon = icon("table")),
                    menuItem("About", tabName = "about", icon = icon("question"),
                             selected = TRUE)
                    )
    ),
    dashboardBody(
        includeCSS("styles.css"),
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
                                        actionButton("exit", "Exit")
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
                    ),
            tabItem(tabName = "about",
                    tags$body("These data were taken from Google Data Arts Team.
 The data is about trade government-authorized small arms and ammunition from
 1992 to 2010.", br(), "In the plot tab you going to find the plot and its
 parameters.", br(), "In the table tab you going to find the dataset created
 from the parameters.", br(), "You can get these dataset and more information "),
 tags$a("here.", href = "https://github.com/dataarts/armsglobe"),
 tags$body(br(), "You can find the code of this shiny application "),
 tags$a("here.", href = "https://github.com/vriffel/armsglobe"), hr(),
 tags$body("Contact me if you find any problem: viniciusriffel@ufpr.br")
 )))
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
    })
    observeEvent(c(input$i_country, input$use, input$slider, input$e_country), {
        newdata <- newdata %>% filter(e %in% input$e_country)
        output$outplot <- renderPlot({
            ggplot(data = newdata,
                   aes(x = year, y = v, col = e, shape = wc)) +
                geom_point(size = 3) +
                ylab("Value ($)") +
                theme_minimal()
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
    observeEvent(input$exit, {
        stopApp()
    })
}

shinyApp(ui, server)
