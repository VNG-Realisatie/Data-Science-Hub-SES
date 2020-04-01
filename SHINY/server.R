library(shiny)
library(plotly)

shinyServer(function(input, output) {
  # Load the data
  Y <- readRDS("tsne_3d_shiny.rds")
  label = Y$cluster
  Y$label = NULL
  # Layout configuration
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 0
  )
  # Axis configuration
  ax <- list(
         title = "",
         zeroline = FALSE,
         showline = FALSE,
         showticklabels = FALSE,
         showgrid = FALSE,
         showspikes = FALSE
  )
  # Legend configuration
  l <- list(
        x = 1,
        y = 0.5,
        font = list(
                family = "sans-serif",
                size = 14,
                color = "#FFFFFF"
        ),
        bgcolor = "#060606",
        bordercolor = "#060606"
  )
 
  output$plot <- renderPlotly({
    plot_ly(type="scatter3d", x = Y[,1], y = Y[,2], z = Y[,3], color = as.factor(label),
            marker = list(size = 3, opacity=0.8), hoverinfo="none",
            width=800,  height = 800, mode="markers") %>%
    config(displayModeBar = F)  %>%
    layout(scene = list(xaxis=ax,yaxis=ax,zaxis=ax),
           paper_bgcolor="#060606",
           plot_bgcolor= "#060606",
           legend = l
    )
  })
  
})
