# ============================================================
# Step 3: Minimal Shiny app
# Just: a filter, a map, and a table.
# ============================================================

library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(geosphere)

# --- Load pre-prepared data ---
snapshot      <- readRDS("data/snapshot.rds")
wait_latest   <- readRDS("data/wait_latest.rds")
centre_coords <- read.csv("data/centre_coords.csv", stringsAsFactors = FALSE)
town_coords   <- read.csv("data/town_coords.csv",   stringsAsFactors = FALSE)

# Join coordinates AND latest wait time onto snapshot
# Note: we DON'T filter out centres without coords here - tables should show
# everything. The map filters for coords at render time.
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
  
  titlePanel("Irish Driving Test Centres"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput("category", "Test Category:",
                  choices = sort(unique(snapshot_geo$driving_test_categories))[c(2:length(unique(snapshot_geo$driving_test_categories)),1)]),
      hr(),
      h4("Find Best Centre"),
      selectInput("county", "Your County:",
                  choices = c("Select..." = "",
                              sort(unique(town_coords$county)))),
      selectInput("town", "Your Town:",
                  choices = c("Select..." = "")),
      sliderInput("max_km", "Max Distance (km):",
                  min = 10, max = 200, value = 50, step = 10),
      actionButton("find", "Find Best Centre",
                   class = "btn-primary", width = "100%"),
      br(), br(),
      uiOutput("recommendation")
    ),
    mainPanel(
      width = 8,
      leafletOutput("map", height = "500px"),
      br(),
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Pass Rate", DTOutput("table")),
        tabPanel("Wait Times", DTOutput("wait_table"))
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  filtered <- reactive({
    snapshot_geo %>% filter(driving_test_categories == input$category)
  })

  # --- Cascade town options when county is selected ---
  observeEvent(input$county, {
    if (input$county == "") {
      updateSelectInput(session, "town", choices = c("Select..." = ""))
      return()
    }
    towns <- town_coords %>%
      filter(county == input$county) %>%
      pull(town) %>% sort()
    updateSelectInput(session, "town",
                      choices = c("Select..." = "", towns))
  })

  # --- Store user location + top 3 centres when Find is clicked ---
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

    candidates <- filtered() %>% filter(!is.na(latitude))
    candidates$distance_km <- distHaversine(
      cbind(candidates$longitude, candidates$latitude),
      c(loc$longitude[1], loc$latitude[1])
    ) / 1000
    candidates <- candidates %>% filter(distance_km <= input$max_km)

    if (nrow(candidates) == 0) {
      top_centres_rv(NULL)
      return()
    }

    # Missing wait times: use average for scoring, preserve NA for display
    avg_wait <- mean(candidates$wait_weeks, na.rm = TRUE)
    candidates <- candidates %>%
      mutate(wait_for_calc = ifelse(is.na(wait_weeks), avg_wait, wait_weeks))

    top <- candidates %>%
      mutate(
        pass_score = (pass_rate - min(pass_rate, na.rm = TRUE)) /
          (max(pass_rate, na.rm = TRUE) -
             min(pass_rate, na.rm = TRUE) + 1e-9),
        wait_score = 1 - (wait_for_calc - min(wait_for_calc, na.rm = TRUE)) /
          (max(wait_for_calc, na.rm = TRUE) -
             min(wait_for_calc, na.rm = TRUE) + 1e-9),
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
        return(tags$div(
          style = "color: #b00; padding: 8px;",
          "No centres within ", input$max_km, " km."
        ))
      }
      return(NULL)
    }

    # Gold / silver / bronze
    rank_colors <- c("#d4af37", "#9ca3af", "#cd7f32")

    cards <- lapply(seq_len(nrow(top)), function(i) {
      row <- top[i, ]
      wait_display <- if (is.na(row$wait_weeks)) {
        "N/A"
      } else {
        paste0(round(row$wait_weeks, 1), " wks")
      }
      col <- rank_colors[i]

      tags$div(
        style = sprintf(
          "background: #f8f9fa; padding: 10px; border-radius: 5px;
           border-left: 4px solid %s; margin-bottom: 8px;", col),
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 4px;",
          tags$span(
            style = sprintf(
              "background: %s; color: white; width: 22px; height: 22px;
               border-radius: 50%%; display: inline-block; text-align: center;
               line-height: 22px; font-weight: bold; margin-right: 8px;
               font-size: 0.85em;", col),
            as.character(i)
          ),
          tags$b(row$driving_test_centre)
        ),
        tags$div(
          style = "font-size: 0.85em; color: #555;",
          sprintf("Pass %.1f%% | Wait %s | %.1f km",
                  row$pass_rate, wait_display, row$distance_km)
        )
      )
    })

    do.call(tagList, cards)
  })

  output$map <- renderLeaflet({
    # Map only shows centres that have coordinates
    df <- filtered() %>% filter(!is.na(latitude))

    if (isTRUE(input$main_tabs == "Wait Times")) {
      # Keep centres with missing wait data - show them in grey
      pal <- colorNumeric("viridis", domain = df$wait_weeks,
                          reverse = TRUE, na.color = "#999999")
      df$color_value <- df$wait_weeks
      legend_title <- "Wait (weeks)"
      popup_text <- ifelse(
        is.na(df$wait_weeks),
        paste0("<b>", df$driving_test_centre, "</b><br>",
               "<i>No wait time data available</i>"),
        paste0("<b>", df$driving_test_centre, "</b><br>",
               "Wait: ", round(df$wait_weeks, 1), " weeks")
      )
    } else {
      pal <- colorNumeric("viridis", domain = df$pass_rate)
      df$color_value <- df$pass_rate
      legend_title <- "Pass Rate (%)"
      popup_text <- paste0("<b>", df$driving_test_centre, "</b><br>",
                           "Pass rate: ", round(df$pass_rate, 1), "%")
    }

    m <- leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -7.9, lat = 53.4, zoom = 7) %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 8,
        color = ~pal(color_value),
        fillOpacity = 0.8, stroke = FALSE,
        popup = popup_text
      ) %>%
      addLegend(pal = pal, values = ~color_value,
                title = legend_title, position = "bottomright")

    # Add user location marker if they've submitted a location
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

    # Highlight top 3 recommended centres
    top <- top_centres_rv()
    if (!is.null(top) && nrow(top) > 0) {
      top <- top %>% filter(!is.na(latitude))
      rank_colors <- c("#d4af37", "#9ca3af", "#cd7f32")[seq_len(nrow(top))]
      m <- m %>%
        addCircleMarkers(
          data = top,
          lng = ~longitude, lat = ~latitude,
          radius = 16,
          color = rank_colors,
          weight = 4,
          opacity = 1,
          fillOpacity = 0,
          label = paste0("#", seq_len(nrow(top)), " ",
                         top$driving_test_centre)
        )
    }

    m
  })

  output$table <- renderDT({
    filtered() %>%
      select(driving_test_centre, pass_rate, total_delivered) %>%
      mutate(pass_rate = as.character(round(pass_rate, 1))) %>%
      mutate(total_delivered = format(total_delivered, big.mark = ",", scientific = F)) %>%
      arrange(desc(pass_rate)) %>%
      rename(`Test Centre` = driving_test_centre,
             `Pass Rate (%)` = pass_rate,
             `Tests Delivered` = total_delivered) %>%
      datatable(options = list(pageLength = 10))
  })

  output$wait_table <- renderDT({
    wait_latest %>%
      arrange(desc(wait_weeks)) %>%
      mutate(wait_weeks = as.character(round(wait_weeks, 1))) %>%
      rename(`Test Centre` = driving_test_centre,
             `Wait (weeks)` = wait_weeks) %>%
      datatable(options = list(pageLength = 10))
  })
}

shinyApp(ui, server)


