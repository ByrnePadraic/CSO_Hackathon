
# ============================================================
# Step 3: Styled Shiny app
# Same functionality, cleaner look
# ============================================================

library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(geosphere)
library(bslib)

# --- Theme ---
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1d4ed8",
  secondary = "#64748b",
  success = "#059669",
  base_font = font_google("Inter"),
  heading_font = font_google("Manrope")
)

# --- Load pre-prepared data ---
snapshot      <- readRDS("data/snapshot.rds")
wait_latest   <- readRDS("data/wait_latest.rds")
centre_coords <- read.csv("data/centre_coords.csv", stringsAsFactors = FALSE)
town_coords   <- read.csv("data/town_coords.csv",   stringsAsFactors = FALSE)

snapshot_geo <- snapshot %>%
  left_join(centre_coords, by = c("driving_test_centre" = "centre_name")) %>%
  left_join(wait_latest,   by = "driving_test_centre")

message("App loaded with ", nrow(snapshot_geo), " centre-category rows")
message("Centres with coordinates: ",
        length(unique(snapshot_geo$driving_test_centre[!is.na(snapshot_geo$latitude)])))
message("Centres without coordinates: ",
        length(unique(snapshot_geo$driving_test_centre[is.na(snapshot_geo$latitude)])))

# --- UI ---
ui <- fluidPage(
  theme = app_theme,
  
  tags$head(
    tags$style(HTML("
      body {
        background: #f6f8fc;
      }

      .app-shell {
        max-width: 1450px;
        margin: 0 auto;
      }

      .hero {
        background: linear-gradient(135deg, #0f172a 0%, #1d4ed8 55%, #0ea5e9 100%);
        color: white;
        padding: 28px 30px;
        border-radius: 24px;
        margin: 22px 0 20px 0;
        box-shadow: 0 18px 40px rgba(15, 23, 42, 0.22);
      }

      .hero-title {
        font-size: 2rem;
        font-weight: 800;
        margin-bottom: 6px;
        line-height: 1.1;
      }

      .hero-subtitle {
        font-size: 0.98rem;
        color: rgba(255,255,255,0.88);
        margin: 0;
      }

      .chip {
        display: inline-block;
        background: rgba(255,255,255,0.14);
        border: 1px solid rgba(255,255,255,0.18);
        color: white;
        border-radius: 999px;
        padding: 6px 12px;
        font-size: 0.82rem;
        font-weight: 600;
        margin-top: 10px;
      }

      .panel-card {
        background: white;
        border: 1px solid #e9eef6;
        border-radius: 22px;
        box-shadow: 0 10px 28px rgba(15, 23, 42, 0.06);
        padding: 18px 18px 16px 18px;
        margin-bottom: 18px;
      }

      .panel-title {
        font-weight: 800;
        font-size: 1.05rem;
        color: #0f172a;
        margin-bottom: 4px;
      }

      .panel-subtitle {
        color: #64748b;
        font-size: 0.92rem;
        margin-bottom: 14px;
      }

      .sidebar-card .shiny-input-container {
        margin-bottom: 14px;
      }

      .soft-rule {
        border-top: 1px solid #e9eef6;
        margin: 16px 0;
      }

      .section-label {
        font-size: 0.84rem;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        color: #64748b;
        font-weight: 800;
        margin-bottom: 10px;
      }

      .btn-primary {
        background: linear-gradient(135deg, #1d4ed8 0%, #0ea5e9 100%);
        border: 0;
        border-radius: 14px;
        font-weight: 700;
        padding: 10px 14px;
        box-shadow: 0 10px 20px rgba(29, 78, 216, 0.18);
      }

      .btn-primary:hover {
        filter: brightness(1.03);
      }

      .metric-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 14px;
        margin-bottom: 18px;
      }

      .metric-card {
        background: white;
        border: 1px solid #e9eef6;
        border-radius: 18px;
        box-shadow: 0 8px 22px rgba(15, 23, 42, 0.05);
        padding: 16px 18px;
      }

      .metric-label {
        color: #64748b;
        font-size: 0.82rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        margin-bottom: 6px;
      }

      .metric-value {
        color: #0f172a;
        font-size: 1.55rem;
        font-weight: 800;
        line-height: 1;
      }

      .metric-note {
        color: #64748b;
        font-size: 0.85rem;
        margin-top: 6px;
      }

      .recommend-card {
        background: #f8fafc;
        border: 1px solid #e5e7eb;
        border-left: 4px solid var(--accent);
        border-radius: 14px;
        padding: 12px;
        margin-bottom: 10px;
      }

      .recommend-rank {
        background: var(--accent);
        color: white;
        width: 24px;
        height: 24px;
        border-radius: 50%;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        font-weight: 800;
        font-size: 0.85rem;
        margin-right: 8px;
      }

      .recommend-title {
        font-weight: 700;
        color: #0f172a;
      }

      .recommend-meta {
        font-size: 0.88rem;
        color: #64748b;
        margin-top: 4px;
      }

      .empty-state {
        background: #fff7ed;
        border: 1px solid #fed7aa;
        color: #9a3412;
        border-radius: 14px;
        padding: 12px;
        font-weight: 600;
      }

      .leaflet-container {
        border-radius: 18px;
      }

      .nav-pills > li > a {
        border-radius: 12px !important;
        font-weight: 700;
      }

      .nav-pills > li.active > a,
      .nav-pills > li.active > a:hover,
      .nav-pills > li.active > a:focus {
        background-color: #1d4ed8 !important;
      }

      table.dataTable tbody tr:hover {
        background-color: #f8fafc !important;
      }

      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        border-radius: 12px;
        border: 1px solid #dbe3ef;
        padding: 6px 10px;
      }

      @media (max-width: 991px) {
        .metric-grid {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  
  div(
    class = "app-shell",
    
    div(
      class = "hero",
      div(class = "hero-title", "RSA Driving Test Centre Analysis"),
      p(class = "hero-subtitle",
        "Explore pass rates, current wait times, and test centre recommendations."),
      div(class = "chip", "Sourced from CSO PxStat data")
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(
          class = "panel-card sidebar-card",
          div(class = "section-label", "Filters"),
          selectInput(
            "category", "Test Category:",
            choices = sort(unique(snapshot_geo$driving_test_categories))[c(
              2:length(unique(snapshot_geo$driving_test_categories)), 1
            )]
          ),
          
          tags$hr(class = "soft-rule"),
          
          div(class = "section-label", "Find Your Best Centre"),
          selectInput(
            "county", "Your County:",
            choices = c("Select..." = "", sort(unique(town_coords$county)))
          ),
          selectInput(
            "town", "Your Town:",
            choices = c("Select..." = "")
          ),
          sliderInput(
            "max_km", "Max Distance (km):",
            min = 10, max = 200, value = 50, step = 10
          ),
          actionButton(
            "find", "Find Best Centre",
            class = "btn-primary", width = "100%"
          ),
          br(), br(),
          uiOutput("recommendation")
        )
      ),
      
      mainPanel(
        width = 8,
        
        uiOutput("summary_cards"),
        
        div(
          class = "panel-card",
          div(class = "panel-title", "Test Centre Map"),
          div(class = "panel-subtitle", "Colour-coded by pass rate or wait time."),
          leafletOutput("map", height = "520px")
        ),
        
        div(
          class = "panel-card",
          div(class = "panel-title", "Centre Tables"),
          div(class = "panel-subtitle", "Inspect the underlying centre-level numbers."),
          tabsetPanel(
            id = "main_tabs",
            type = "pills",
            tabPanel("Pass Rate", DTOutput("table")),
            tabPanel("Wait Times", DTOutput("wait_table"))
          )
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  filtered <- reactive({
    snapshot_geo %>% filter(driving_test_categories == input$category)
  })
  
  output$summary_cards <- renderUI({
    df <- filtered()
    
    mapped_centres <- df %>%
      filter(!is.na(latitude), !is.na(longitude)) %>%
      distinct(driving_test_centre) %>%
      nrow()
    
    avg_pass <- round(mean(df$pass_rate, na.rm = TRUE), 1)
    avg_wait <- round(mean(df$wait_weeks, na.rm = TRUE), 1)
    
    tags$div(
      class = "metric-grid",
      tags$div(
        class = "metric-card",
        tags$div(class = "metric-label", "Mapped Centres"),
        tags$div(class = "metric-value", mapped_centres),
        tags$div(class = "metric-note", "Visible on the map for this category")
      ),
      tags$div(
        class = "metric-card",
        tags$div(class = "metric-label", "Average Pass Rate"),
        tags$div(class = "metric-value", paste0(avg_pass, "%")),
        tags$div(class = "metric-note", "12-month average across centres")
      ),
      tags$div(
        class = "metric-card",
        tags$div(class = "metric-label", "Average Wait"),
        tags$div(class = "metric-value", paste0(avg_wait, " wks")),
        tags$div(class = "metric-note", "Latest available wait-time view")
      )
    )
  })
  
  observeEvent(input$county, {
    if (input$county == "") {
      updateSelectInput(session, "town", choices = c("Select..." = ""))
      return()
    }
    
    towns <- town_coords %>%
      filter(county == input$county) %>%
      pull(town) %>%
      sort()
    
    updateSelectInput(session, "town", choices = c("Select..." = "", towns))
  })
  
  user_loc_rv    <- reactiveVal(NULL)
  top_centres_rv <- reactiveVal(NULL)
  
  observeEvent(input$find, {
    if (is.null(input$county) || is.null(input$town) ||
        input$county == "" || input$town == "") {
      top_centres_rv(NULL)
      return()
    }
    
    loc <- town_coords %>%
      filter(county == input$county, town == input$town)
    
    if (nrow(loc) != 1) {
      top_centres_rv(NULL)
      return()
    }
    
    user_loc_rv(loc)
    
    candidates <- filtered() %>% filter(!is.na(latitude), !is.na(longitude))
    candidates$distance_km <- distHaversine(
      cbind(candidates$longitude, candidates$latitude),
      c(loc$longitude[1], loc$latitude[1])
    ) / 1000
    
    candidates <- candidates %>% filter(distance_km <= input$max_km)
    
    if (nrow(candidates) == 0) {
      top_centres_rv(NULL)
      return()
    }
    
    avg_wait <- mean(candidates$wait_weeks, na.rm = TRUE)
    
    candidates <- candidates %>%
      mutate(wait_for_calc = ifelse(is.na(wait_weeks), avg_wait, wait_weeks))
    
    top <- candidates %>%
      mutate(
        pass_score = (pass_rate - min(pass_rate, na.rm = TRUE)) /
          (max(pass_rate, na.rm = TRUE) - min(pass_rate, na.rm = TRUE) + 1e-9),
        wait_score = 1 - (wait_for_calc - min(wait_for_calc, na.rm = TRUE)) /
          (max(wait_for_calc, na.rm = TRUE) - min(wait_for_calc, na.rm = TRUE) + 1e-9),
        score = (0.3 * pass_score) + (0.5 * wait_score) * (0.2 * (1 - distance_km))
      ) %>%
      arrange(desc(score)) %>%
      head(3)
    
    top_centres_rv(top)
  }, ignoreInit = TRUE)
  
  output$recommendation <- renderUI({
    top <- top_centres_rv()
    
    if (is.null(top) || nrow(top) == 0) {
      if (input$find > 0) {
        return(
          tags$div(
            class = "empty-state",
            paste0("No centres within ", input$max_km, " km.")
          )
        )
      }
      return(NULL)
    }
    
    rank_colors <- c("#d4af37", "#94a3b8", "#cd7f32")
    
    cards <- lapply(seq_len(nrow(top)), function(i) {
      row <- top[i, ]
      wait_display <- if (is.na(row$wait_weeks)) "N/A" else paste0(round(row$wait_weeks, 1), " wks")
      col <- rank_colors[i]
      
      tags$div(
        class = "recommend-card",
        style = sprintf("--accent:%s;", col),
        tags$div(
          style = "display:flex; align-items:center;",
          tags$span(class = "recommend-rank", i),
          tags$span(class = "recommend-title", row$driving_test_centre)
        ),
        tags$div(
          class = "recommend-meta",
          sprintf("Pass %.1f%% | Wait %s | %.1f km",
                  row$pass_rate, wait_display, row$distance_km)
        )
      )
    })
    
    do.call(tagList, cards)
  })
  
  output$map <- renderLeaflet({
    df <- filtered() %>% filter(!is.na(latitude), !is.na(longitude))
    
    if (isTRUE(input$main_tabs == "Wait Times")) {
      pal <- colorNumeric(
        "viridis",
        domain = df$wait_weeks,
        reverse = TRUE,
        na.color = "#94a3b8"
      )
      df$color_value <- df$wait_weeks
      legend_title <- "Wait (weeks)"
      popup_text <- ifelse(
        is.na(df$wait_weeks),
        paste0("<b>", df$driving_test_centre, "</b><br><i>No wait time data available</i>"),
        paste0("<b>", df$driving_test_centre, "</b><br>Wait: ", round(df$wait_weeks, 1), " weeks")
      )
    } else {
      pal <- colorNumeric("viridis", domain = df$pass_rate)
      df$color_value <- df$pass_rate
      legend_title <- "Pass Rate (%)"
      popup_text <- paste0(
        "<b>", df$driving_test_centre, "</b><br>",
        "Pass rate: ", round(df$pass_rate, 1), "%"
      )
    }
    
    m <- leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -7.9, lat = 53.4, zoom = 7) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 9,
        stroke = TRUE,
        weight = 2,
        color = "white",
        fillColor = ~pal(color_value),
        fillOpacity = 0.92,
        popup = popup_text
      ) %>%
      addLegend(
        pal = pal,
        values = ~color_value,
        title = legend_title,
        position = "bottomright",
        opacity = 0.95
      )
    
    user_loc <- user_loc_rv()
    if (!is.null(user_loc) && nrow(user_loc) == 1) {
      m <- m %>%
        addMarkers(
          lng = user_loc$longitude[1],
          lat = user_loc$latitude[1],
          label = paste0("You: ", user_loc$town[1]),
          popup = paste0("<b>Your location</b><br>", user_loc$town[1])
        )
    }
    
    top <- top_centres_rv()
    if (!is.null(top) && nrow(top) > 0) {
      top <- top %>% filter(!is.na(latitude), !is.na(longitude))
      rank_colors <- c("#d4af37", "#94a3b8", "#cd7f32")[seq_len(nrow(top))]
      
      m <- m %>%
        addCircleMarkers(
          data = top,
          lng = ~longitude,
          lat = ~latitude,
          radius = 16,
          color = rank_colors,
          weight = 4,
          opacity = 1,
          fillOpacity = 0,
          label = paste0("#", seq_len(nrow(top)), " ", top$driving_test_centre)
        )
    }
    
    m
  })
  
  output$table <- renderDT({
    filtered() %>%
      arrange(desc(pass_rate)) %>%
      transmute(
        `Test Centre` = driving_test_centre,
        `Pass Rate (%)` = pass_rate,
        `Tests Delivered` = total_delivered
      ) %>%
      datatable(
        rownames = FALSE,
        class = "compact stripe hover",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          dom = "ftp"
        )
      ) %>%
      formatRound(columns = "Pass Rate (%)", digits = 1) %>%
      formatCurrency(
        columns = "Tests Delivered",
        currency = "",
        interval = 3,
        mark = ",",
        digits = 0
      )
  })
  
  output$wait_table <- renderDT({
    wait_latest %>%
      arrange(desc(wait_weeks)) %>%
      transmute(
        `Test Centre` = driving_test_centre,
        `Wait (weeks)` = wait_weeks
      ) %>%
      datatable(
        rownames = FALSE,
        class = "compact stripe hover",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          dom = "ftp"
        )
      ) %>%
      formatRound(columns = "Wait (weeks)", digits = 1)
  })
}

shinyApp(ui, server)

