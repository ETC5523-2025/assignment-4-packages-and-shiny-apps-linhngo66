library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggbeeswarm)
library(yarraView)

# Load the dataset
data(yarra_water)

# Parameter descriptions
parameter_descriptions <- list(
  "Water Temperature" = "Water temperature affects aquatic life metabolism, dissolved oxygen levels, and chemical processes. Measured in degrees Celsius (°C).",
  "Turbidity" = "Turbidity measures water clarity by detecting suspended particles. High turbidity can indicate pollution or erosion. Measured in Nephelometric Turbidity Units (NTU).",
  "Salinity as EC@25" = "Electrical conductivity (EC) at 25°C indicates the concentration of dissolved salts in water. High salinity can affect freshwater ecosystems. Measured in microsiemens per centimeter (µS/cm).",
  "pH" = "pH measures water acidity or alkalinity on a scale of 0-14. Most aquatic life thrives in pH range of 6.5-8.5. A pH of 7 is neutral."
)

ui <- page_navbar(
  title = "Yarra Water Quality Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Tab 1: Time Series
  nav_panel(
    title = "Time Series",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 350,
        
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
      
      layout_columns(
        col_widths = c(6, 6, 12, 12),
        row_heights = c(1, 5, 1),

        value_box(
          title = tags$h5("Average Value", style = "font-size: 0.9rem; margin: 0;"),
          value = tags$div(
            textOutput("avg_value"),
            style = "font-size: 2rem; font-weight: 600;"
          ),
          showcase = bsicons::bs_icon("graph-up", size = "0.9em"),
          theme = "primary",
          height = "100%"
        ),
        
        value_box(
          title = tags$h5("Number of Records", style = "font-size: 0.9rem; margin: 0;"),
          value = tags$div(
            textOutput("record_count"),
            style = "font-size: 2rem; font-weight: 600;"
          ),
          showcase = bsicons::bs_icon("clipboard-data", size = "0.9em"),
          theme = "info",
          height = "100%"
        ),
        
        card(
          card_header("Time Series Plot"),
          card_body(
            plotlyOutput("timeseries_plot", height = "100%")
          ),
          full_screen = TRUE,
          height = "100%"
        ),
        
        card(
          card_header("Parameter Information"),
          card_body(
            textOutput("parameter_info_ts")
          ),
          height = "100%"
        )
      )
    )
  ),
  
  # Tab 2: Comparison
  nav_panel(
    title = "Comparison",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 350,
        
        selectInput(
          "parameter_select_comp",
          "Select Parameter:",
          choices = c("Water Temperature", "Turbidity", "Salinity as EC@25", "pH"),
          selected = "Water Temperature"
        )
      ),
      
      layout_columns(
        col_widths = 12,
        row_heights = c(4, 1),
        
        card(
          card_header("Period Comparison: 1990s vs 2020s"),
          card_body(
            plotlyOutput("comparison_plot", height = "100%")
          ),
          full_screen = TRUE,
          height = "100%"
        ),
        
        card(
          card_header("Parameter Information"),
          card_body(
            textOutput("parameter_info_comp")
          ),
          height = "100%"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ===== TIME SERIES TAB =====
  tab1_data <- reactive({
    data <- yarra_water |> 
      filter(period == "2020s")
    
    if (input$year_select != "All years") {
      data <- data |> filter(year == as.numeric(input$year_select))
    }
    
    data <- data |> filter(parameter == input$parameter_select)
    
    data
  })
  
  unit_display <- reactive({
    if(
      input$parameter_select == "Water Temperature" |
      input$parameter_select_comp == "Water Temperature"
    ) "\u00B0C" else unit
  })

  output$avg_value <- renderText({
    req(nrow(tab1_data()) > 0)
    
    avg <- mean(tab1_data()$value, na.rm = TRUE)
    unit <- unique(tab1_data()$unit_of_measurement)[1]
    paste0(round(avg, 2), " ", unit_display())
  })
  
  output$record_count <- renderText({
    req(nrow(tab1_data()) > 0)
    
    format(nrow(tab1_data()), big.mark = ",")
  })
  
  output$parameter_info_ts <- renderText({
    parameter_descriptions[[input$parameter_select]]
  })
  
  output$timeseries_plot <- renderPlotly({
    req(nrow(tab1_data()) > 0)
    
    unit <- unique(tab1_data()$unit_of_measurement)[1]
    
    p <- tab1_data() |>
      ggplot(aes(x = datetime, y = value)) +
      geom_line(color = "#2E86AB", linewidth = 0.8, alpha = 0.9) +
      geom_point(aes(text = paste0(
        "Datetime: ", format(datetime, "%Y-%m-%d %H:%M"), "<br>",
        "Value: ", round(value, 2), " ", unit_display()
      )), color = "#2E86AB", size = 1.5, alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "#A23B72", 
                  fill = "#A23B72", alpha = 0.2, linewidth = 1) +
      scale_y_continuous(
        labels = scales::label_number(suffix = paste0(" ", unit_display()))
      ) +
      scale_x_datetime(
        date_breaks = ifelse(input$year_select == "All years", "3 months", "1 month"),
        date_labels = "%b\n%Y"
      ) +
      labs(
        title = paste("Yarra River", input$parameter_select, "(", input$year_select, ")"),
        x = NULL,
        y = paste(input$parameter_select, paste0("(", unit_display(), ")"))
      ) +
      theme_bw(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
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
  
  # ===== COMPARISON TAB =====
  
  tab2_data <- reactive({
    yarra_water |>
      filter(parameter == input$parameter_select_comp) |>
      filter(period %in% c("1990s", "2020s"))
  })
  
  output$parameter_info_comp <- renderText({
    parameter_descriptions[[input$parameter_select_comp]]
  })
  
  output$comparison_plot <- renderPlotly({
    req(nrow(tab2_data()) > 0)
    
    # Calculate medians for tooltip
    medians <- tab2_data() |>
      group_by(month, period) |>
      summarise(median_value = median(value, na.rm = TRUE), .groups = "drop")
    
    # Get unit
    unit <- unique(tab2_data()$unit_of_measurement)[1]
    
    # Create base plot with beeswarm
    p_base <- tab2_data() |>
      ggplot(aes(x = factor(month), y = value, color = period, group = period)) +
      geom_quasirandom(
        alpha = 0.1,
        size = 1.5
      ) +
      scale_color_manual(
        values = c("1990s" = "#E63946", "2020s" = "#2E86AB"),
        name = "Period"
      ) +
      scale_x_discrete(
        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
      ) +
      scale_y_continuous(
        labels = scales::label_number(suffix = paste0(" ", unit_display()))
      ) +
      labs(
        title = paste(input$parameter_select_comp, ": 1990s vs 2020s"),
        subtitle = "Points show median values connected by lines for each period",
        x = "Month",
        y = paste(input$parameter_select_comp, paste0("(", unit_display(), ")"))
      ) +
      theme_bw(base_size = 13) +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    # Add median lines
    p_with_lines <- p_base +
      geom_line(
        data = medians,
        aes(x = factor(month), y = median_value, group = period, color = period),
        linewidth = 1
      ) +
      geom_point(
        data = medians,
        aes(x = factor(month), y = median_value, group = period, color = period,
            text = paste0(
              "Month: ", month.name[month], "<br>",
              "Period: ", period, "<br>",
              "Median: ", round(median_value, 2), " ", unit_display()
            )),
        size = 3
      )
    
    # Convert to plotly
    ggplotly(p_with_lines, tooltip = "text") |>
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