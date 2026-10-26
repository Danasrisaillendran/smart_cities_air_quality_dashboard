library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

# --------------------------------------------------
# SMART CITIES: URBAN AIR QUALITY DASHBOARD
# --------------------------------------------------

air <- read.csv(
  "data/city_day.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "NaN")
)

required_cols <- c("City", "Date", "AQI")
missing_cols <- setdiff(required_cols, names(air))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ",
       paste(missing_cols, collapse = ", "))
}

air$Date <- as.Date(air$Date)

numeric_cols <- intersect(
  c("AQI", "PM2.5", "PM10", "NO2", "SO2",
    "CO", "O3", "NH3"),
  names(air)
)

for (nm in numeric_cols) {
  air[[nm]] <- suppressWarnings(as.numeric(air[[nm]]))
}

air$City <- as.character(air$City)
air <- air %>% filter(!is.na(Date), !is.na(City))

if (nrow(air) == 0) {
  stop("No valid records were found in data/city_day.csv.")
}

city_choices <- sort(unique(air$City))
date_min <- min(air$Date)
date_max <- max(air$Date)

pollutant_cols <- intersect(
  c("PM2.5", "PM10", "NO2", "SO2", "CO", "O3", "NH3"),
  names(air)
)

aqi_colours <- c(
  "Good" = "#249B62",
  "Satisfactory" = "#A6C94A",
  "Moderate" = "#F2C744",
  "Poor" = "#F28E2B",
  "Very Poor" = "#D9534F",
  "Severe" = "#7D2635"
)

# --------------------------------------------------
# USER INTERFACE
# --------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: #f3f6fb;
        color: #182235;
        font-family: Arial, sans-serif;
      }
      .dashboard-header {
        background: linear-gradient(120deg,#142b4a,#176b87);
        color: white;
        padding: 25px;
        border-radius: 13px;
        margin: 15px 0 22px;
      }
      .well {
        background: white;
        border: 1px solid #dfe7f1;
        border-radius: 12px;
      }
      .kpi {
        background: white;
        border: 1px solid #dfe7f1;
        border-radius: 12px;
        padding: 15px;
        margin-bottom: 15px;
        min-height: 112px;
        box-shadow: 0 2px 8px rgba(20,40,70,.05);
      }
      .kpi h5 { color: #65748b; margin-top: 0; }
      .kpi h3 {
        color: #176b87;
        font-weight: bold;
        overflow-wrap: anywhere;
      }
      .plot-panel {
        background: white;
        border: 1px solid #dfe7f1;
        border-radius: 12px;
        padding: 14px;
        margin-bottom: 18px;
      }
      .nav-tabs > li > a { font-weight: bold; }
      .download-area { margin: 12px 0 20px; }
    "))
  ),

  div(
    class = "dashboard-header",
    h2("Smart Cities: Urban Air Quality Dashboard"),
    h4("CSI3005 | Advanced Data Visualization"),
    p("Explore air quality patterns across Indian cities."),
    p("Analyze AQI, compare seasons, and investigate pollutant relationships.")
  ),

  sidebarLayout(
    sidebarPanel(
      h4("Dashboard Filters"),

      selectInput(
        "city", "Select city",
        choices = c("All Cities", city_choices),
        selected = "All Cities"
      ),

      dateRangeInput(
        "dates", "Date range",
        start = date_min,
        end = date_max,
        min = date_min,
        max = date_max,
        format = "yyyy-mm-dd"
      ),

      checkboxInput(
        "valid_aqi", "Exclude records without AQI", TRUE
      ),

      hr(),

      downloadButton(
        "download_data", "Download filtered CSV",
        class = "btn-primary"
      ),

      br(), br(),
      p("All charts and summary metrics respond to the selected filters.")
    ),

    mainPanel(
      fluidRow(
        column(4, div(class = "kpi",
          h5("Records"),
          h3(textOutput("record_count", inline = TRUE))
        )),
        column(4, div(class = "kpi",
          h5("Average AQI"),
          h3(textOutput("average_aqi", inline = TRUE))
        )),
        column(4, div(class = "kpi",
          h5("Highest AQI"),
          h3(textOutput("highest_aqi", inline = TRUE))
        ))
      ),

      fluidRow(
        column(6, div(class = "kpi",
          h5("Cleanest city in selection"),
          h3(textOutput("cleanest_city", inline = TRUE))
        )),
        column(6, div(class = "kpi",
          h5("Most polluted city in selection"),
          h3(textOutput("polluted_city", inline = TRUE))
        ))
      ),

      tabsetPanel(
        tabPanel("Overview",
          br(),
          fluidRow(
            column(6, div(class = "plot-panel",
              h4("AQI Category Distribution"),
              plotOutput("category_plot", height = "320px")
            )),
            column(6, div(class = "plot-panel",
              h4("Monthly AQI Trend"),
              plotOutput("trend_plot", height = "320px")
            ))
          ),
          div(class = "plot-panel",
            h4("Average AQI by City"),
            plotOutput("city_plot", height = "420px")
          )
        ),

        tabPanel("Time & Seasons",
          br(),
          div(class = "plot-panel",
            h4("Daily AQI Calendar-Style Heatmap"),
            p("Each tile represents a calendar day. Darker colors indicate higher AQI."),
            plotOutput("calendar_plot", height = "480px")
          ),
          div(class = "plot-panel",
            h4("Seasonal AQI Comparison"),
            p("Winter: Dec-Feb; Pre-monsoon: Mar-May; Monsoon: Jun-Sep; Post-monsoon: Oct-Nov."),
            plotOutput("season_plot", height = "360px")
          )
        ),

        tabPanel("City Comparison",
          br(),
          div(class = "plot-panel",
            h4("AQI Distribution by City"),
            plotOutput("box_plot", height = "520px")
          )
        ),

        tabPanel("Pollutants",
          br(),
          fluidRow(
            column(6, div(class = "plot-panel",
              h4("PM2.5 vs PM10"),
              plotOutput("scatter_plot", height = "350px")
            )),
            column(6, div(class = "plot-panel",
              h4("Pollutant Correlation Heatmap"),
              plotOutput("correlation_plot", height = "350px")
            ))
          ),
          p("Correlation indicates statistical association, not causation.")
        )
      ),

      br(),
      p("Data source: India city-day air quality dataset supplied for this project.")
    )
  )
)

# --------------------------------------------------
# SERVER
# --------------------------------------------------

server <- function(input, output, session) {

  filtered_data <- reactive({
    req(input$dates)

    d <- air %>%
      filter(
        Date >= input$dates[1],
        Date <= input$dates[2]
      )

    if (input$city != "All Cities") {
      d <- d %>% filter(City == input$city)
    }

    if (isTRUE(input$valid_aqi)) {
      d <- d %>% filter(!is.na(AQI))
    }

    d
  })

  aqi_data <- reactive({
    filtered_data() %>% filter(!is.na(AQI))
  })

  output$record_count <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })

  output$average_aqi <- renderText({
    d <- aqi_data()
    if (nrow(d) == 0) return("N/A")
    round(mean(d$AQI), 1)
  })

  output$highest_aqi <- renderText({
    d <- aqi_data()
    if (nrow(d) == 0) return("N/A")
    round(max(d$AQI), 1)
  })

  city_summary <- reactive({
    d <- aqi_data()
    validate(need(nrow(d) > 0, "No valid AQI data for these filters."))

    d %>%
      group_by(City) %>%
      summarise(
        Mean_AQI = mean(AQI),
        Records = n(),
        .groups = "drop"
      )
  })

  output$cleanest_city <- renderText({
    s <- city_summary()
    s$City[which.min(s$Mean_AQI)]
  })

  output$polluted_city <- renderText({
    s <- city_summary()
    s$City[which.max(s$Mean_AQI)]
  })

  output$category_plot <- renderPlot({
    d <- aqi_data()

    validate(need(nrow(d) > 0, "No AQI data for these filters."))

    if ("AQI_Bucket" %in% names(d) &&
        any(!is.na(d$AQI_Bucket))) {

      counts <- d %>%
        filter(!is.na(AQI_Bucket)) %>%
        count(AQI_Bucket, name = "Records")

      ggplot(counts,
        aes(x = reorder(AQI_Bucket, Records),
            y = Records, fill = AQI_Bucket)
      ) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_fill_manual(
          values = aqi_colours,
          na.value = "grey70"
        ) +
        labs(x = NULL, y = "Number of records") +
        theme_minimal(base_size = 12)
    } else {
      ggplot(d, aes(x = AQI)) +
        geom_histogram(
          bins = 25, fill = "#2389a8", color = "white"
        ) +
        labs(x = "AQI", y = "Number of records") +
        theme_minimal(base_size = 12)
    }
  })

  output$trend_plot <- renderPlot({
    d <- aqi_data()
    validate(need(nrow(d) > 0, "No AQI data for these filters."))

    monthly <- d %>%
      mutate(Month = floor_date(Date, "month")) %>%
      group_by(Month) %>%
      summarise(Mean_AQI = mean(AQI), .groups = "drop")

    ggplot(monthly, aes(Month, Mean_AQI)) +
      geom_line(color = "#176b87", linewidth = 1) +
      geom_point(color = "#F28E2B", size = 1.8) +
      labs(x = "Month", y = "Average AQI") +
      theme_minimal(base_size = 12)
  })

  output$city_plot <- renderPlot({
    s <- city_summary()

    ggplot(s, aes(x = reorder(City, Mean_AQI), y = Mean_AQI)) +
      geom_col(fill = "#2389a8") +
      coord_flip() +
      labs(x = NULL, y = "Average AQI") +
      theme_minimal(base_size = 11)
  })

  output$box_plot <- renderPlot({
    d <- aqi_data()
    validate(need(nrow(d) > 0, "No AQI data for these filters."))

    ggplot(d, aes(x = reorder(City, AQI, FUN = median),
                  y = AQI, fill = City)) +
      geom_boxplot(show.legend = FALSE, outlier.alpha = 0.2) +
      coord_flip() +
      labs(x = NULL, y = "AQI") +
      theme_minimal(base_size = 10)
  })

  output$calendar_plot <- renderPlot({
    d <- aqi_data()
    validate(need(nrow(d) > 0, "No AQI data for these filters."))

    daily <- d %>%
      mutate(
        Year = format(Date, "%Y"),
        Month = factor(
          month(Date, label = TRUE, abbr = TRUE),
          levels = month.abb
        ),
        Day = day(Date)
      ) %>%
      group_by(Year, Month, Day) %>%
      summarise(Mean_AQI = mean(AQI), .groups = "drop")

    ggplot(daily, aes(x = Day, y = Month, fill = Mean_AQI)) +
      geom_tile(color = "white", linewidth = 0.2) +
      facet_wrap(~Year) +
      scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 31)) +
      scale_fill_gradient(
        low = "#D9F0E3", high = "#B52B36",
        name = "Average AQI"
      ) +
      labs(x = "Day of month", y = "Month") +
      theme_minimal(base_size = 10) +
      theme(panel.grid = element_blank())
  })

  output$season_plot <- renderPlot({
    d <- aqi_data()
    validate(need(nrow(d) > 0, "No AQI data for these filters."))

    seasonal <- d %>%
      mutate(
        MonthNumber = month(Date),
        Season = case_when(
          MonthNumber %in% c(12, 1, 2) ~ "Winter",
          MonthNumber %in% 3:5 ~ "Pre-monsoon",
          MonthNumber %in% 6:9 ~ "Monsoon",
          TRUE ~ "Post-monsoon"
        ),
        Season = factor(
          Season,
          levels = c(
            "Winter", "Pre-monsoon",
            "Monsoon", "Post-monsoon"
          )
        )
      ) %>%
      group_by(Season) %>%
      summarise(
        Mean_AQI = mean(AQI),
        Records = n(),
        .groups = "drop"
      )

    ggplot(seasonal, aes(Season, Mean_AQI, fill = Season)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = round(Mean_AQI, 1)),
        vjust = -0.4, size = 4
      ) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = NULL, y = "Average AQI") +
      theme_minimal(base_size = 12)
  })

  output$scatter_plot <- renderPlot({
    d <- filtered_data()

    validate(need(
      all(c("PM2.5", "PM10") %in% names(d)),
      "PM2.5 or PM10 is unavailable."
    ))

    d <- d %>% filter(!is.na(`PM2.5`), !is.na(PM10))
    validate(need(nrow(d) > 0, "No paired pollutant records."))

    p <- ggplot(d, aes(x = `PM2.5`, y = PM10)) +
      geom_point(alpha = 0.3, color = "#176b87") +
      labs(x = "PM2.5", y = "PM10") +
      theme_minimal(base_size = 12)

    if (nrow(d) >= 3 &&
        sd(d$`PM2.5`) > 0 &&
        sd(d$PM10) > 0) {
      p <- p + geom_smooth(
        method = "lm", se = FALSE, color = "#D9534F"
      )
    }

    p
  })

  output$correlation_plot <- renderPlot({
    d <- filtered_data()

    validate(need(
      length(pollutant_cols) >= 2,
      "At least two pollutant columns are required."
    ))

    x <- d[, pollutant_cols, drop = FALSE]

    keep <- vapply(x, function(v) {
      sum(!is.na(v)) >= 2 &&
        is.finite(sd(v, na.rm = TRUE)) &&
        sd(v, na.rm = TRUE) > 0
    }, logical(1))

    x <- x[, keep, drop = FALSE]

    validate(need(
      ncol(x) >= 2,
      "Not enough variable pollutant measurements."
    ))

    corr <- cor(x, use = "pairwise.complete.obs")
    corr[!is.finite(corr)] <- NA_real_

    corr_long <- as.data.frame(as.table(corr))
    names(corr_long) <- c("Pollutant1", "Pollutant2", "Correlation")
    corr_long <- corr_long %>% filter(!is.na(Correlation))

    validate(need(
      nrow(corr_long) > 0,
      "Unable to calculate correlations for these filters."
    ))

    ggplot(corr_long,
      aes(Pollutant1, Pollutant2, fill = Correlation)
    ) +
      geom_tile(color = "white") +
      geom_text(
        aes(label = sprintf("%.2f", Correlation)),
        size = 3
      ) +
      scale_fill_gradient2(
        low = "#D9534F", mid = "white",
        high = "#2389a8", midpoint = 0,
        limits = c(-1, 1), name = "Correlation"
      ) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("air_quality_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui = ui, server = server)
