statRegressionUI <- function(id) {
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
            "Please select year of interest",
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
            "Please select year of interest",
            choices = unique(dataset$year)
          ),
          pickerInput(
            ns("type_ops"),
            "Please select water type of interest",
            choices = unique(dataset[dataset$year == "1997", "type"]),
            multiple = TRUE,
            options = list(
              "actions-box" = TRUE
            )
          )
        ),
        pickerInput(
          ns("dependent"),
          "Please select a dependent variable",
          choices = colnames(dataset)[-c(1, 2, 7)],
          selected = "Mg",
          options = list(
            "live-search" = TRUE
          )
        ),
        pickerInput(
          ns("independent"),
          "Please select one or several independent variable(s)",
          choices = colnames(dataset)[-c(1, 2, 7)],
          selected = c("Na", "Br"),
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE,
            "live-search" = TRUE
          )
        ),
        actionButton(ns("apply"), "Apply")
      ),
      mainPanel(
        h3("Summary of Statistic"),
        withSpinner(tableOutput(ns("result")), type = 4, color = "#44ade9"),
        withSpinner(tableOutput(ns("result2")), type = 4, color = "#44ade9"),
        hr(),
        h3("Plot"),
        withSpinner(plotlyOutput(ns("plot")), type = 4, color = "#44ade9")
      )
    )
  )
}

statRegression <- function(input, output, session) {
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
      condition = !is.null(input$dependent) & !is.null(input$independent)
    )
  })
  
  df <- eventReactive(input$apply, {
    if (input$profile == "All") {
      dataset %>%
        select(year, type, one_of(input$dependent, input$independent))
    } else if (input$profile == "Year") {
      dataset %>%
        select(year, one_of(input$dependent, input$independent)) %>%
        filter(year %in% input$year)
    } else if (input$profile == "Type") {
      dataset %>%
        select(year, type, one_of(input$dependent, input$independent)) %>%
        filter(type %in% input$type)
      rename(Profile = type)
    } else if (input$profile == "Type per year") {
      dataset %>%
        select(year, type, one_of(input$dependent, input$independent)) %>%
        filter(year %in% input$year_ops) %>%
        filter(type %in% input$type_ops)
    }
  })
  
  result <- eventReactive(input$apply, {
    formula = paste(input$dependent, "~", paste(input$independent, collapse = " + "))
    df() %>% 
      ntbt(lm, formula = as.formula(formula)) %>% 
      tidy() %>% 
      `colnames<-`(tools::toTitleCase(names(.)))
  })
  
  output$result <- renderTable({
    result()
  })
  
  result2 <- eventReactive(input$apply, {
    formula = paste(input$dependent, "~", paste(input$independent, collapse = " + "))
    df() %>% 
      ntbt(lm, formula = as.formula(formula)) %>% 
      glance() %>% 
      `colnames<-`(tools::toTitleCase(names(.)))
  })
  
  output$result2 <- renderTable({
    result2()
  })
  
  estimateplot <- eventReactive(input$apply, {
    formula = paste(input$dependent, "~", paste(input$independent, collapse = " + "))
    df() %>% 
      ntbt(lm, formula = as.formula(formula)) %>% 
      tidy(conf.int = TRUE) %>% 
      `colnames<-`(tools::toTitleCase(names(.))) %>%
      mutate(Term = factor(Term, levels = unique(Term))) %>% 
      ggplot(aes(Estimate, Term, color = Term)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = Conf.low, xmax = Conf.high), height = 0.3) +
      labs(col = "") +
      theme_minimal()
  })

  output$plot <- renderPlotly({
    estimateplot() %>%
      ggplotly()
  })
}
