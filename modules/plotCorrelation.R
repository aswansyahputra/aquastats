plotCorrelationUI <- function(id) {
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
          "Please select several parameters to investigate",
          choices = colnames(dataset)[-c(1, 2, 7)],
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE
          )
        ),
        materialSwitch(
          ns("corrvalue"),
          "Show correlation values",
          status = "primary",
          right = TRUE
        ),
        materialSwitch(
          ns("significant"),
          "Mark insignificant values",
          status = "primary",
          right = TRUE
        ),
        actionButton(ns("apply"), "Apply")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Plot",
            icon = icon("image"),
            h3("Correlation Plot"),
            withSpinner(plotOutput(ns("plot")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Data",
            icon = icon("table"),
            h3("Data on Correlation Plot"),
            withSpinner(DT::dataTableOutput(ns("data")), type = 4, color = "#44ade9")
          )
        )
      )
    )
  )
}

plotCorrelation <- function(input, output, session) {
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
  
  # Data ---
  
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

  # Plot ----

  corrplot <- eventReactive(input$apply, {
    if (input$significant) {
      p_mat <- df() %>%
        # filter(year %in% input$year) %>%
        select(input$parameter) %>%
        ggcorrplot::cor_pmat()
      res_plot <- df() %>%
        # filter(year %in% input$year) %>%
        select(input$parameter) %>%
        cor() %>%
        ggcorrplot::ggcorrplot(
          method = "square",
          ggtheme = theme_minimal,
          outline.color = "white",
          lab = input$corrvalue,
          p.mat = p_mat
        )
    } else {
      res_plot <- df() %>%
        # filter(year %in% input$year) %>%
        select(input$parameter) %>%
        cor() %>%
        ggcorrplot::ggcorrplot(
          method = "square",
          ggtheme = theme_minimal,
          outline.color = "white",
          lab = input$corrvalue,
          p.mat = NULL
        )
    }
    return(res_plot)
  })

  output$plot <- renderPlot({
    corrplot()
  })

  # Data ----
  plot_data <- eventReactive(input$apply, {
    dataset %>%
      filter(year %in% input$year) %>%
      select(input$parameter)
  })

  output$data <- DT::renderDataTable({
    plot_data() %>%
      datatable(
        rownames = FALSE,
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 300,
          scroller = TRUE,
          buttons =
            list(
              list(
                extend = "copy"
              ),
              list(
                extend = "collection",
                buttons = c("csv", "excel"),
                text = "Download"
              )
            )
          ,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#44ade9', 'color': '#fff'});",
            "}"
          )
        )
      )
  },
  server = FALSE
  )
}
