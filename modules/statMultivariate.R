statMultivariateUI <- function(id) {
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
            choices = unique(dataset2$year),
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
            choices = unique(dataset2$type),
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
            choices = unique(dataset2$year)
          ),
          pickerInput(
            ns("type_ops"),
            "Please select water type of interest",
            choices = unique(dataset2[dataset2$year == "1997", "type"]),
            multiple = TRUE,
            options = list(
              "actions-box" = TRUE
            )
          )
        ),
        pickerInput(
          ns("parameter"),
          "Please select parameter to investigate",
          choices = colnames(dataset2)[-c(1, 2, 7)],
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE
          )
        ),
        pickerInput(
          ns("parameter_supp"),
          "Please select supplementary parameter to investigate",
          choices = colnames(dataset2)[-c(1, 2, 7)],
          multiple = TRUE,
          options = list(
            "actions-box" = TRUE
          )
        ),
        actionButton(ns("apply"), "Apply")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Overview",
            icon = icon("briefcase"),
            h3("Overview"),
            withSpinner(tableOutput(ns("overview")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Data",
            icon = icon("table"),
            h3("Agggregated dataset2"),
            withSpinner(DT::dataTableOutput(ns("dataset2")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Dimension",
            icon = icon("plus"),
            h3("Description of dimension"),
            withSpinner(plotOutput(ns("screeplot")), type = 4, color = "#44ade9"),
            withSpinner(verbatimTextOutput(ns("dimension")), type = 4, color = "#44ade9")
          ),
          tabPanel(
            "Plot",
            icon = icon("image"),
            h3("PCA Plot"),
            fluidRow(
              column(
                1,
                dropdownButton(
                  circle = TRUE,
                  status = "primary",
                  icon = icon("gear"),
                  width = "350px",
                  tooltip = tooltipOptions(title = "Options"),
                  selectInput(
                    ns("options"),
                    "Plot Options",
                    choices = c("Profile", "Parameter")
                  ),
                  numericInput(
                    ns("x_axis"),
                    "Dimension on x axis",
                    min = 1,
                    max = 4,
                    value = 1
                  ),
                  numericInput(
                    ns("y_axis"),
                    "Dimension on y axis",
                    min = 2,
                    max = 5,
                    value = 2
                  ),
                  materialSwitch(
                    ns("repel"),
                    "Repel text",
                    value = TRUE,
                    status = "primary",
                    right = TRUE
                  ),
                  sliderInput(
                    ns("labsize"),
                    "Label size",
                    min = 2,
                    max = 8,
                    value = 3,
                    step = 1
                  )
                )
              ),
              column(11,
                align = "center",
                withSpinner(plotOutput(ns("plot"), width = "600px", height = "400px"), type = 4, color = "#44ade9")
              )
            )
          )
        )
      )
    )
  )
}

statMultivariate <- function(input, output, session) {
  ns <- session$ns

  observeEvent(input$year_ops, {
    type <- dataset2 %>%
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
    if (input$profile == "Year") {
      dataset2 %>%
        select(year, one_of(input$parameter, input$parameter_supp)) %>%
        filter(year %in% input$year) %>%
        group_by(year) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        rename(Profile = year)
    } else if (input$profile == "Type") {
      dataset2 %>%
        select(year, type, one_of(input$parameter, input$parameter_supp)) %>%
        filter(type %in% input$type) %>%
        group_by(type) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        rename(Profile = type)
    } else if (input$profile == "Type per year") {
      dataset2 %>%
        select(year, type, one_of(input$parameter, input$parameter_supp)) %>%
        filter(year %in% input$year_ops) %>%
        filter(type %in% input$type_ops) %>%
        group_by(type) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE) %>%
        rename(Profile = type)
    }
  })

  overview <- eventReactive(input$apply, {
    df() %>%
      {
        tibble(
          "Method" = "Principal Component Analysis",
          "Profile" = .[, "Profile"] %>% pull() %>% unique() %>% paste(collapse = ", "),
          "Parameter" = paste(input$parameter, collapse = ", "),
          "Paramater Supplementary" = paste(input$parameter_supp, collapse = ", ")
        )
      } %>%
      t() %>%
      as_tibble(rownames = "Parameter")
  })

  output$overview <- renderTable({
    overview()
  }, colnames = FALSE)

  output$dataset2 <- DT::renderDataTable({
    df() %>%
      mutate_if(is.numeric, round, 2) %>%
      datatable(
        rownames = FALSE,
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 250,
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

  global <- reactive({
    req(df())
    if (is.null(input$parameter_supp)) {
      dat <- df() %>%
        as.data.frame() %>%
        `rownames<-`(.[, "Profile"]) %>%
        select_if(is.numeric)
      quanti_supp <- NULL
    } else {
      dat <- df() %>%
        as.data.frame() %>%
        `rownames<-`(.[, "Profile"]) %>%
        select_if(is.numeric)
      quanti_supp <- which(names(dat) %in% input$parameter_supp)
    }
    res <- PCA(dat, quanti.sup = quanti_supp, graph = FALSE)
    return(res)
  })

  output$screeplot <- renderPlot({
    plot_eigen(global())
  })

  output$dimension <- renderPrint({
    dimdesc(global(), proba = 1)
  })

  plot <- reactive({
    if (input$options == "Profile") {
      plot_profile(global(),
        axes = c(input$x_axis, input$y_axis),
        repel = input$repel,
        lab.size = input$labsize
      )
    } else if (input$options == "Parameter") {
      plot_parameter(
        res = global(),
        axes = c(input$x_axis, input$y_axis),
        lab.size = input$labsize
      )
    }
  })

  output$plot <- renderPlot({
    plot()
  })
}
