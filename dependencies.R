getDependencies <- function() {
  d3 <- htmltools::htmlDependency(
    name="d3",
    version="5.16.0",
    src=c(href="https://unpkg.com/d3@5/dist/"),
    script="d3.min.js"
  )
  
  sszvis <- htmltools::htmlDependency(
    name="sszvis",
    version="2.1.1",
    src=c(href="https://unpkg.com/sszvis@2/build/"),
    script="sszvis.min.js",
    stylesheet="sszvis.css"
  )

  # Local script with logic to render the chart.
  chart_script <- tags$script(src="script.js")
  
  dependencies <- list(
    d3,
    sszvis,
    chart_script
  )
  
  return(dependencies)
}
