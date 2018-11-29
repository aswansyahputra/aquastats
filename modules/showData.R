showDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Dataset",
        icon = icon("table"),
        br(),
        withSpinner(DT::dataTableOutput(ns("dataset")), type = 4, color = "#44ade9")
      )
      # tabPanel(
      #   "Parameter Description",
      #   icon = icon("bookmark"),
      #   br(),
      #   withSpinner(DT::dataTableOutput(ns("description")), type = 4, color = "#44ade9")
      # )
    )
  )
}

showData <- function(input, output, session) {
  ns <- session$ns

  # Dataset ----
  output$dataset <- DT::renderDataTable({
    dataset %>%
      datatable(
        rownames = FALSE,
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brfti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 400,
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

  # Data Description ----
  output$description <- DT::renderDataTable({
    description %>%
      datatable(
        rownames = FALSE,
        extensions = c("Scroller", "Buttons"),
        options = list(
          dom = "Brfti",
          autoWidth = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scrollY = 400,
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
