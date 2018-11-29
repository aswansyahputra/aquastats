plotScatterplotUI <- function(id) {
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
          ns("x_axis"),
          "Please select one parameter as x axis:",
          choices = colnames(dataset)[-c(1, 2, 7)],
          selected = "Ca"
        ),

        pickerInput(
          ns("y_axis"),
          "Please select one parameter as y axis:",
          choices = colnames(dataset)[-c(1, 2, 7)],
          selected = "pH"
        ),
        radioButtons(
          ns("pointscolour"),
          "Colour points by:",
          choices = c(
            "Type",
            "Year"
          )
        ),
        materialSwitch(
          ns("regressionline"),
          "Show regression line",
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
            h3("Scatterplot"),
            withSpinner(plotlyOutput(ns("plot")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Data",
            icon = icon("table"),
            h3("Data on Scatterplot"),
            withSpinner(DT::dataTableOutput(ns("data")), type = 4, color = "#44ade9")
          )
        )
      )
    )
  )
}

plotScatterplot <- function(input, output, session) {
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
      condition = !is.null(input$x_axis) && !is.null(input$y_axis)
    )
  })
  
  # Data ---
  
  df <- eventReactive(input$apply, {
    if (input$profile == "All") {
      dataset %>%
        select(year, type, one_of(input$x_axis, input$y_axis))
    } else if (input$profile == "Year") {
      dataset %>%
        select(year, one_of(input$x_axis, input$y_axis)) %>%
        filter(year %in% input$year)
    } else if (input$profile == "Type") {
      dataset %>%
        select(year, type, one_of(input$x_axis, input$y_axis)) %>%
        filter(type %in% input$type)
    } else if (input$profile == "Type per year") {
      dataset %>%
        select(year, type, one_of(input$x_axis, input$y_axis)) %>%
        filter(year %in% input$year_ops) %>%
        filter(type %in% input$type_ops)
    }
  })
  
  # Plot ----
  
  scatterplot <- eventReactive(input$apply, {
    res_plot <- df() %>%
      ggplot(aes_string(x = input$x_axis, y = input$y_axis)) +
      geom_point(aes_string(col = switch (input$pointscolour,
        "Year" = "year",
        "Type" = "type"
      ))) +
      labs(col = "") +
      theme_minimal()

    if (input$regressionline) {
      res_plot <- res_plot + geom_smooth(method = "lm", na.rm = TRUE)
    } else {
      res_plot
    }
    return(res_plot)
  })

  output$plot <- renderPlotly({
    scatterplot() %>%
      ggplotly()
  })

  # Data ----
  plot_data <- eventReactive(input$apply, {
    df() %>%
      select(input$x_axis, input$y_axis)
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
