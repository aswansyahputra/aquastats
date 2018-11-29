statCorrelationUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        tagList(icon = icon("sliders", "fa-2x")),
        br(),
        radioButtons(
          ns("profile"),
          "Please select profile of interest",
          choices = c(
            "All",
            "Year",
            "Type",
            "Type per year"
          )
        ),
        conditionalPanel(
          condition = "input.profile == 'Year'",
          ns = ns,
          pickerInput(
            ns("year"),
            "Please select Year of interest",
            choices = unique(dataset$year),
            multiple = TRUE,
            options = list(
              "actions-box" = TRUE
            )
          )
        ),
        conditionalPanel(
          condition = "input.profile == 'Type'",
          ns = ns,
          pickerInput(
            ns("type"),
            "Please select water type of interest",
            choices = unique(dataset$type),
            multiple = TRUE,
            options = list(
              "actions-box" = TRUE
            )
          )
        ),
        conditionalPanel(
          condition = "input.profile == 'Type per year'",
          ns = ns,
          pickerInput(
            ns("year_ops"),
            "Please select water type of interest",
            choices = unique(dataset$year)
          ),
          pickerInput(
            ns("type_ops"),
            "Please select type within year to analyse",
            choices = unique(dataset[dataset$year == "1997", "type"]),
            multiple = TRUE,
            options = list(
              "actions-box" = TRUE
            )
          )
        ),
        pickerInput(
          ns("x"),
          "Please select a parameter to investigate",
          choices = colnames(dataset)[-c(1, 2, 7)],
          selected = "Mg",
          options = list(
            "live-search" = TRUE
          )
        ),
        pickerInput(
          ns("y"),
          "Please select another parameter to investigate",
          choices = colnames(dataset)[-c(1, 2, 7)],
          selected = "Na",
          options = list(
            "live-search" = TRUE
          )
        ),
        radioButtons(
          ns("alternative"),
          "Alternative Hypothesis",
          choices = c(
            "Two-sided" = "two.sided",
            "Less than" = "less",
            "Greater than" = "greater"
          )
        ),
        radioButtons(
          ns("method"),
          "Test Method",
          choices = c(
            "Pearson" = "pearson",
            "Kendall" = "kendall",
            "Spearman" = "spearman"
          )
        ),
        numericInput(
          ns("conf.level"),
          "Confidence Level",
          min = 0.8, max = 1, value = 0.95
        ),
        actionButton(ns("apply"), "Apply")
      ),
      mainPanel(
        h3("Summary of Statistic"),
        withSpinner(tableOutput(ns("result")), type = 4, color = "#44ade9"),
        hr(),
        h3("Plot"),
        withSpinner(plotlyOutput(ns("plot")), type = 4, color = "#44ade9")
      )
    )
  )
}

statCorrelation <- function(input, output, session) {
  ns <- session$ns

  observeEvent(input$year_ops, {
    type <- dataset %>%
      filter(year %in% input$year_ops) %>%
      select(type) %>%
      unique() %>%
      pull()
    updatePickerInput(
      session = session,
      inputId = "type_ops",
      choices = type
    )
  },
  ignoreInit = TRUE
  )

  observe({
    toggleState(
      id = "apply",
      condition = !is.null(input$x) & !is.null(input$y)
    )
  })

  df <- eventReactive(input$apply, {
    if (input$profile == "All") {
      dataset %>%
        select(year, type, one_of(input$x, input$y))
    } else if (input$profile == "Year") {
      dataset %>%
        select(year, one_of(input$x, input$y)) %>%
        filter(year %in% input$year)
    } else if (input$profile == "Type") {
      dataset %>%
        select(year, type, one_of(input$x, input$y)) %>%
        filter(type %in% input$type)
    } else if (input$profile == "Type per year") {
      dataset %>%
        select(year, type, one_of(input$x, input$y)) %>%
        filter(year %in% input$year_ops) %>%
        filter(type %in% input$type_ops)
    }
  })

  result <- eventReactive(input$apply, {
    formula <- paste("~", input$x, "+", input$y)
    df() %>%
      ntbt(cor.test, formula = as.formula(formula), alternative = input$alternative, method = input$method, conf.level = input$conf.level) %>%
      tidy() %>% 
      `colnames<-`(tools::toTitleCase(names(.)))
  })

  output$result <- renderTable({
    result()
  })

  scatterplot <- eventReactive(input$apply, {
    df() %>%
      ggplot(aes_string(x = input$x, y = input$y)) +
      geom_point() +
      geom_smooth(method = "lm", na.rm = TRUE) +
      labs(col = "") +
      theme_minimal()
  })

  output$plot <- renderPlotly({
    scatterplot() %>%
      ggplotly()
  })
}
