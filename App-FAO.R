# FAOSTAT classroom dashboard
# Let's build our app in four steps: read the data, design the page,
# connect the filters to the outputs, and start Shiny.
# Run once if needed:
# install.packages(c("shiny", "dplyr", "ggplot2", "leaflet", "DT", "bslib"))

library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(bslib)

# STEP 1: Read and prepare the data ------------------------------------------
# Keep this CSV beside App.R. Shiny uses the app folder as its working folder.
fao <- read.csv("FAOSTAT_Data.csv", stringsAsFactors = FALSE,
                fileEncoding = "UTF-8-BOM")
required <- c("Area", "Item", "Element", "Year", "Unit", "Value")
stopifnot(all(required %in% names(fao)))
fao$Year <- as.integer(fao$Year)
fao$Value <- as.numeric(fao$Value)
stopifnot(all(is.finite(fao$Year)), all(is.finite(fao$Value)))
fao <- filter(fao, (Element == "Yield" & Unit == "kg/ha") |
                (Element == "Area harvested" & Unit == "ha"))
stopifnot(nrow(fao) > 0)
countries <- sort(unique(fao$Area))
crops <- sort(unique(fao$Item))
years <- range(fao$Year)

# The CSV has no coordinates. These approximate country centers are added
# only to position the bubbles; they do not represent individual farms.
locations <- data.frame(
  Area = c("Afghanistan", "Albania", "Algeria", "Angola"),
  longitude = c(67.71, 20.17, 2.63, 17.87),
  latitude = c(33.94, 41.15, 28.03, -11.20)
)
country_colors <- c(Afghanistan = "#168bff", Albania = "#2022aa",
                    Algeria = "#f26b38", Angola = "#800080")
number <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")

# STEP 2: Design the page ---------------------------------------------------
# Let's keep the reference colors using bslib's native value boxes.
# The theme argument sets each color directly, without writing HTML or CSS.
ui <- fluidPage(
  theme = bs_theme(version = 5, bg = "#eef1f5", fg = "#263446",
                   primary = "#2563eb"),
  titlePanel("FAOSTAT | Crop dashboard"),
  helpText("Explore crop yield and harvested area across countries and years."),
  layout_columns(
    col_widths = c(3, 3, 3, 3),
    value_box("Average yield | kg/ha", textOutput("average_yield"),
              theme = value_box_theme(bg = "#2563eb", fg = "white")),
    value_box("Total harvested area | ha", textOutput("total_area"),
              theme = value_box_theme(bg = "#10a34a", fg = "white")),
    value_box("Peak observed yield | kg/ha", textOutput("peak_yield"),
              theme = value_box_theme(bg = "#ef5809", fg = "white")),
    value_box("Selected period", textOutput("selected_period"),
              theme = value_box_theme(bg = "#9333ea", fg = "white"))
  ),
  fluidRow(
    column(8,
           h4(textOutput("map_title")),
           leafletOutput("map", height = 370),
           helpText("Bubbles show country-level summaries for the selected period. Locations are approximate country centers.")
    ),
    column(4, wellPanel(
      radioButtons("metric", "Metric view",
                   choices = c("Yield" = "Yield", "Area" = "Area harvested"),
                   selected = "Yield", inline = TRUE),
      sliderInput("years", "Year", min = years[1], max = years[2],
                  value = years, step = 1, sep = ""),
      checkboxGroupInput("countries", "Countries", countries, selected = countries),
      checkboxGroupInput("crops", "Crops", crops, selected = crops),
      actionButton("reset", "Reset filters")
    ))
  ),
  fluidRow(
    column(7,
           h4(textOutput("trend_title")),
           plotOutput("trend", height = 285)
    ),
    column(5,
           h4("Data table"),
           DTOutput("table"),
           downloadButton("download", "Download filtered records")
    )
  ),
  helpText("Source: supplied FAOSTAT CSV. Yield is an unweighted mean of available records; area is summed across selected years and crops. Missing observations are not treated as zero. No records are available for 2014.")
)

# STEP 3: Connect inputs and outputs ---------------------------------------
server <- function(input, output, session) {
  # A reactive expression is recalculated whenever one of its inputs changes.
  # We filter once here so every output uses the same selection.
  filtered <- reactive({
    filter(fao, Year >= input$years[1], Year <= input$years[2],
           Area %in% input$countries, Item %in% input$crops)
  })
  
  selected_metric <- reactive({
    filter(filtered(), Element == input$metric)
  })
  metric_unit <- reactive(if (input$metric == "Yield") "kg/ha" else "ha")
  summary_label <- reactive(if (input$metric == "Yield") "Average yield" else "Total harvested area")
  
  # Yield is a rate, so we average it. Harvested area is additive, so we sum it.
  # This is an unweighted average: we do not have a production-based weight.
  summarize_value <- function(x) {
    if (input$metric == "Yield") mean(x) else sum(x)
  }
  
  # Each renderText() supplies one plain-text value to the page.
  # Keeping these outputs separate makes the UI/server connection easy to follow.
  output$average_yield <- renderText({
    values <- filtered()$Value[filtered()$Element == "Yield"]
    if (length(values)) number(mean(values)) else "No data"
  })
  output$total_area <- renderText({
    values <- filtered()$Value[filtered()$Element == "Area harvested"]
    if (length(values)) number(sum(values)) else "No data"
  })
  output$peak_yield <- renderText({
    values <- filtered()$Value[filtered()$Element == "Yield"]
    if (length(values)) number(max(values)) else "No data"
  })
  output$selected_period <- renderText({
    paste(input$years, collapse = " – ")
  })
  
  output$map_title <- renderText({
    paste("Geographic distribution —", summary_label(), paste0("(", metric_unit(), ")"))
  })
  output$map <- renderLeaflet({
    data <- selected_metric()
    validate(need(nrow(data) > 0, "No data. Select countries, crops, or a wider year range."))
    bubbles <- data %>% group_by(Area) %>%
      summarise(Value = summarize_value(Value), .groups = "drop") %>%
      left_join(locations, by = "Area")
    # Taking the square root makes bubble area proportional to the value.
    bubbles$radius <- 30 * sqrt(bubbles$Value / max(bubbles$Value))
    bubbles$label <- paste(bubbles$Area, "—", number(bubbles$Value), metric_unit())
    # Use the default OpenStreetMap tiles: no account or API key is needed.
    leaflet(bubbles) %>%
      addTiles() %>%
      setView(lng = 32, lat = 17, zoom = 2) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = ~radius,
                       color = "#15518c", weight = 1.5, fillColor = "#168bff",
                       fillOpacity = 0.8, label = ~label)
  })
  
  output$trend_title <- renderText({
    paste(summary_label(), "by year and country")
  })
  output$trend <- renderPlot({
    data <- selected_metric()
    validate(need(nrow(data) > 0, "No data. Adjust the filters to display the chart."))
    trend <- data %>% group_by(Year, Area) %>%
      summarise(Value = summarize_value(Value), .groups = "drop")
    # Insert missing years as NA so the line does not bridge a data gap.
    grid <- expand.grid(Year = seq(input$years[1], input$years[2]),
                        Area = unique(trend$Area))
    trend <- merge(grid, trend, by = c("Year", "Area"), all.x = TRUE)
    ggplot(trend, aes(Year, Value, color = Area, group = Area)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 1.6, na.rm = TRUE) +
      scale_color_manual(values = country_colors) +
      scale_y_continuous(labels = function(x) format(x, big.mark = ",", trim = TRUE)) +
      labs(x = "Year", y = paste(summary_label(), paste0("(", metric_unit(), ")")),
           color = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top", panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
  })
  
  output$table <- renderDT({
    data <- selected_metric()
    validate(need(nrow(data) > 0, "No records match these filters."))
    # Keep crops separate here so students can inspect each country/crop pair.
    table_data <- data %>% group_by(Area, Item, Unit) %>%
      summarise(Value = summarize_value(Value), .groups = "drop") %>%
      rename(Country = Area, Crop = Item) %>% select(Country, Crop, Value, Unit)
    names(table_data)[3] <- summary_label()
    datatable(table_data, rownames = FALSE,
              options = list(pageLength = 5, lengthChange = FALSE,
                             searching = FALSE, scrollX = TRUE)) %>%
      formatRound(columns = 3, digits = 1, mark = ",")
  })
  
  # Download the original records for the active metric, not the summaries.
  output$download <- downloadHandler(
    filename = function() "FAOSTAT_filtered.csv",
    content = function(file) write.csv(selected_metric(), file, row.names = FALSE)
  )
  observeEvent(input$reset, {
    updateSliderInput(session, "years", value = years)
    updateCheckboxGroupInput(session, "countries", selected = countries)
    updateCheckboxGroupInput(session, "crops", selected = crops)
    updateRadioButtons(session, "metric", selected = "Yield")
  })
}

# STEP 4: Start the app ----------------------------------------------------
# In RStudio, open App.R and click Run App, or run:
# shiny::runApp("path/to/fao-shiny/App.R")
shinyApp(ui, server)
