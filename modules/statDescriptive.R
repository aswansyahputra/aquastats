statDescriptiveUI <- function(id) {
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
            "Please select year of interest",
            choices = unique(dataset$year)
          ),
          pickerInput(
            ns("type_ops"),
            "Please select water type of interest",
            choices = unique(dataset$type),
            multiple = TRUE,
            options = list(
              "actions-box" = TRUE
            )
          )
        ),
        pickerInput(
          ns("parameter"),
          "Please select parameter to investigate",
          choices = colnames(dataset)[-c(1, 2, 7)],
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE
          )
        ),
        actionButton(ns("apply"), "Apply")
      ),
      mainPanel(
        h3("Summary Table"),
        withSpinner(htmlOutput(ns("summary")), type = 4, color = "#44ade9")
      )
    )
  )
}

statDescriptive <- function(input, output, session) {
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
      condition = !is.null(input$parameter)
    )
  })

  df <- eventReactive(input$apply, {
    if (input$profile == "All") {
      dataset %>%
        select(year, type, one_of(input$parameter))
    } else if (input$profile == "Year") {
      dataset %>%
        select(year, one_of(input$parameter)) %>%
        filter(year %in% input$year)
    } else if (input$profile == "Type") {
      dataset %>%
        select(year, type, one_of(input$parameter)) %>%
        filter(type %in% input$type)
    } else if (input$profile == "Type per year") {
      dataset %>%
        select(year, type, one_of(input$parameter)) %>%
        filter(year %in% input$year_ops) %>%
        filter(type %in% input$type_ops)
    }
  })

  output$summary <- renderUI({
    print(dfSummary(df(), varnumbers = FALSE, graph.magnif = 0.8, style = "multiline"),
      method = "render",
      omit.headings = TRUE,
      bootstrap.css = FALSE,
      custom.css = "./www/custom-summarytools.css",
      footnote = NA
    )
  })
}
