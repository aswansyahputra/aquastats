server <- function(input, output) {
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")
  # Data ----
  callModule(showData, "data")

  # Plot ----
  ## Scatterplot ----
  callModule(plotScatterplot, "plot_scatterplot")

  ## Correlation Plot ----
  callModule(plotCorrelation, "plot_correlation")

  # Statistic ----
  ## Descriptive ----
  callModule(statDescriptive, "stat_descriptive")
  
  ## Correlation ----
  callModule(statCorrelation, "stat_correlation")
  
  ## Regression ----
  callModule(statRegression, "stat_regression")

  ## Multivariate
  callModule(statMultivariate, "stat_multivariate")
}
