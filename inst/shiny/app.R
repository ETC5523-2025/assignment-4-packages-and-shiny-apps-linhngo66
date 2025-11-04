library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(yarraView)

# Load the dataset
data(yarra_water)

ui <- page_navbar(
  title = "Yarra Water Quality Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel(
    title = "Time Series",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        
        selectInput(
          "year_select",
          "Select Year:",
          choices = c("All years", "2020", "2021", "2022", "2023", "2024"),
          selected = "All years"
        ),
        
        selectInput(
          "parameter_select",
          "Select Parameter:",
          choices = c("Water Temperature", "Turbidity", "Salinity as EC@25", "pH"),
          selected = "Water Temperature"
        )
      ),
      
      # Main content area with custom layout
      layout_columns(
        col_widths = c(6, 6, 12),
        row_heights = c(1, 2),
        
        # Top row: Score cards
        value_box(
          title = "Average Value",
          value = textOutput("avg_value"),
          showcase = bsicons::bs_icon("graph-up"),
          theme = "primary",
          height = "100%"
        ),
        
        value_box(
          title = "Number of Records",
          value = textOutput("record_count"),
          showcase = bsicons::bs_icon("clipboard-data"),
          theme = "info",
          height = "100%"
        ),
        
        # Bottom row: Time series plot
        card(
          card_header("Time Series Plot"),
          card_body(
            plotlyOutput("timeseries_plot", height = "100%")
          ),
          full_screen = TRUE,
          height = "100%"
        )
      )
    )
  ),
  
  nav_panel(
    title = "Comparison",
    p("Comparison tab content coming soon...")
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered data
  tab1_data <- reactive({
    data <- yarra_water |> 
      filter(period == "2020s")
    
    # Filter by year
    if (input$year_select != "All years") {
      data <- data |> filter(year == as.numeric(input$year_select))
    }
    
    # Filter by parameter
    data <- data |> filter(parameter == input$parameter_select)
    
    data
  })
  
  # Average value score card
  output$avg_value <- renderText({
    req(nrow(tab1_data()) > 0)
    
    avg <- mean(tab1_data()$value, na.rm = TRUE)
    unit <- unique(tab1_data()$unit_of_measurement)[1]
    
    paste0(round(avg, 2), " ", unit)
  })
  
  # Record count score card
  output$record_count <- renderText({
    req(nrow(tab1_data()) > 0)
    
    format(nrow(tab1_data()), big.mark = ",")
  })
  
  # Time series plot with Plotly
  output$timeseries_plot <- renderPlotly({
    req(nrow(tab1_data()) > 0)
    
    # Get unit for tooltip
    unit <- unique(tab1_data()$unit_of_measurement)[1]
    
    # Create custom tooltip text
    data_with_tooltip <- tab1_data() |>
      mutate(
        tooltip_text = paste0(
          "Datetime: ", format(datetime, "%Y-%m-%d %H:%M"), "<br>",
          "Value: ", round(value, 2), " ", unit
        )
      )
    
    # Create ggplot
    p <- data_with_tooltip |>
      ggplot(aes(x = datetime, y = value)) +
      geom_line(color = "#2E86AB", linewidth = 0.8, alpha = 0.9) +
      geom_point(aes(text = tooltip_text), color = "#2E86AB", size = 1.5, alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "#A23B72", 
                  fill = "#A23B72", alpha = 0.2, linewidth = 1,
                  aes(text = NULL)) +
      scale_y_continuous(
        labels = scales::number_format(
          suffix = ifelse(input$parameter_select == "Water Temperature", "°C", "")
        ),
        expand = expansion(mult = c(0.05, 0.1))
      ) +
      scale_x_datetime(
        date_breaks = ifelse(input$year_select == "All years", "3 months", "1 month"),
        date_labels = "%b\n%Y",
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      labs(
        title = paste("Yarra River", input$parameter_select, "(", input$year_select, ")"),
        x = NULL,
        y = paste(input$parameter_select, 
                  ifelse(input$parameter_select == "Water Temperature", "(°C)", 
                        paste0("(", unit, ")")))
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
        plot.subtitle = element_text(color = "grey40", size = 11, margin = margin(b = 15)),
        plot.caption = element_text(color = "grey50", size = 9, hjust = 0, margin = margin(t = 15)),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        axis.text = element_text(color = "grey30"),
        axis.title.y = element_text(margin = margin(r = 10), size = 11),
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Convert to plotly with custom tooltip
    ggplotly(p, tooltip = "text") |>
      layout(
        hovermode = "closest",
        hoverlabel = list(
          bgcolor = "white",
          font = list(size = 12, color = "black"),
          bordercolor = "#2E86AB"
        )
      ) |>
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
}

shinyApp(ui, server)