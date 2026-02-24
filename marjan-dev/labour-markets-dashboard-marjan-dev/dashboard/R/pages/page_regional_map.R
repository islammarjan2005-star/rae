# R/pages/page_regional_map.R
# Regional Employment Map page — plots UK regional data on an interactive map

library(ggplot2)
library(plotly)

# ---- Simplified UK coastline (no internet / maps package needed) ----
UK_OUTLINE <- rbind(
  # Great Britain
  data.frame(
    long = c(-5.7,-5.0,-4.2,-3.5,-2.5,-1.3,-0.8, 0.3, 1.0, 1.4, 1.7,
             1.7, 1.2, 0.5, 0.2,-0.2, 0.0,-0.5,-1.2,-1.6,-2.0,-1.8,
            -2.0,-2.5,-3.0,-3.4,-4.0,-5.0,-5.2,-5.5,-5.8,-5.5,-5.8,
            -5.5,-4.8,-5.0,-5.0,-4.4,-3.4,-3.0,-3.1,-3.4,-4.0,-4.5,
            -5.2,-5.3,-4.5,-3.8,-3.2,-3.0,-3.6,-4.2,-5.0,-5.7),
    lat  = c(50.1, 50.0, 50.3, 50.2, 50.6, 50.7, 50.7, 50.8, 51.1, 51.4,
             51.9, 52.5, 52.8, 53.0, 53.5, 53.7, 54.0, 54.6, 55.0, 55.6,
             56.0, 56.5, 57.0, 57.7, 58.2, 58.6, 58.6, 58.6, 58.0, 57.5,
             57.0, 56.5, 56.0, 55.8, 55.3, 55.0, 54.7, 54.8, 54.5, 53.5,
             53.2, 53.0, 52.8, 52.5, 51.8, 51.5, 51.4, 51.5, 51.4, 51.2,
             51.0, 50.8, 50.3, 50.1),
    group = 1L, stringsAsFactors = FALSE
  ),
  # Northern Ireland
  data.frame(
    long = c(-5.5,-5.9,-6.2,-6.7,-7.3,-7.6,-7.3,-7.0,-6.5,-6.0,-5.5),
    lat  = c(54.1, 54.3, 54.6, 55.0, 55.2, 55.3, 55.0, 54.7, 54.3, 54.1, 54.1),
    group = 2L, stringsAsFactors = FALSE
  )
)

# ---- UK Region Reference Data (ONS area codes -> names & centroids) ----
UK_REGIONS <- data.frame(
  area_code   = c("E12000001", "E12000002", "E12000003", "E12000004",
                   "E12000005", "E12000006", "E12000007", "E12000008",
                   "E12000009", "W92000004", "S92000003", "N92000002"),
  region_name = c("North East", "North West", "Yorkshire and The Humber",
                   "East Midlands", "West Midlands", "East of England",
                   "London", "South East", "South West",
                   "Wales", "Scotland", "Northern Ireland"),
  lat = c(55.0, 54.0, 53.8, 52.8, 52.5, 52.2, 51.5, 51.2, 50.7, 52.0, 56.5, 54.6),
  lon = c(-1.5, -2.7, -1.2, -1.0, -2.0,  0.5, -0.1, -0.8, -3.2, -3.5, -4.0, -6.8),
  stringsAsFactors = FALSE
)

REGIONAL_AREA_CODES <- UK_REGIONS$area_code

EMPLOYMENT_GROUPS  <- c("Employment", "Unemployment",
                        "Economically active", "Economically inactive")
VALUE_TYPES        <- c("Rate", "Level")
AGE_GROUPS_REGIONAL <- c("Aged 16 and over", "Aged 16 to 64")


# ---- Data fetch (adjust table name to match your DB) ----
get_regional_tbl <- function(conn) {
  # Table 22 – Regional LFS Summary
  # Change the schema/table below if your DB uses a different name
  DBI::dbGetQuery(conn, "
    SELECT employment_group,
           age_group,
           value_type,
           area_code,
           CAST(value AS NUMERIC) AS value
    FROM   ons.labour_market__regional_survey
    WHERE  area_code IN (
             'E12000001','E12000002','E12000003','E12000004',
             'E12000005','E12000006','E12000007','E12000008',
             'E12000009','W92000004','S92000003','N92000002'
           )
    ORDER BY area_code
  ")
}


# ---- UI ----
regional_map_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "govuk-width-container",
      tags$main(class = "govuk-main-wrapper",
        tags$span(class = "govuk-caption-xl", "Labour Market"),
        tags$h1(class = "govuk-heading-xl", "Regional Employment Map"),
        tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),

        div(class = "govuk-grid-row",
          div(class = "govuk-grid-column-full",

            # ---- Map card ----
            mod_govuk_data_vis_card_ui(
              id    = ns("map_card"),
              title = "UK Regional Employment Map",
              help_text = paste(
                "Employment data by UK region.",
                "Bubble size and colour represent the selected measure.",
                "Source: ONS Labour Force Survey (Table 22)."
              ),
              visual_content = plotlyOutput(ns("uk_map"), height = "600px"),
              table_content  = reactableOutput(ns("region_table"), height = "400px"),

              controls = list(
                div(class = "govuk-grid-row",
                  div(class = "govuk-grid-column-one-third",
                    selectInput(ns("measure"), "Employment Measure",
                                choices  = EMPLOYMENT_GROUPS,
                                selected = "Employment")
                  ),
                  div(class = "govuk-grid-column-one-third",
                    selectInput(ns("value_type"), "Value Type",
                                choices  = VALUE_TYPES,
                                selected = "Rate")
                  ),
                  div(class = "govuk-grid-column-one-third",
                    selectInput(ns("age_group"), "Age Group",
                                choices  = AGE_GROUPS_REGIONAL,
                                selected = "Aged 16 and over")
                  )
                )
              )
            ),

            # ---- Bar chart card ----
            tags$div(style = "margin-top: 30px;",
              mod_govuk_data_vis_card_ui(
                id    = ns("bar_card"),
                title = "Regional Comparison",
                help_text = "Side-by-side comparison of all UK regions for the selected measure.",
                visual_content = plotlyOutput(ns("bar_chart"), height = "450px")
              )
            )
          )
        )
      )
    )
  )
}


# ---- Server ----
regional_map_server <- function(id, conn = APP_DB$pool) {
  moduleServer(id, function(input, output, session) {

    mod_govuk_data_vis_card_server("map_card")
    mod_govuk_data_vis_card_server("bar_card")

    # Fetch all regional data (cached per session)
    all_regional <- reactive({
      get_regional_tbl(conn)
    })

    # Filtered + merged with coordinates
    map_df <- reactive({
      d <- all_regional()
      req(nrow(d) > 0)

      d <- d[d$employment_group == input$measure &
             d$value_type      == input$value_type &
             d$age_group       == input$age_group, ]

      merged <- merge(d, UK_REGIONS, by = "area_code", all.x = FALSE)
      merged$value <- as.numeric(merged$value)
      merged <- merged[!is.na(merged$value), ]
      merged
    })

    # ---- Interactive map using embedded UK outline (offline-safe) ----
    output$uk_map <- renderPlotly({
      d <- map_df()
      req(nrow(d) > 0)

      is_rate <- input$value_type == "Rate"
      suffix  <- if (is_rate) "%" else " (000s)"

      # Build bubble size
      vals <- d$value
      size_range <- c(4, 12)
      if (length(unique(vals)) == 1L) {
        d$pt_size <- rep(mean(size_range), length(vals))
      } else {
        d$pt_size <- size_range[1] + (vals - min(vals, na.rm = TRUE)) /
          (max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE)) *
          (size_range[2] - size_range[1])
      }

      d$hover_text <- paste0(
        d$region_name, "\n",
        input$measure, " (", input$value_type, "): ",
        format(round(d$value, 1), big.mark = ","), suffix
      )

      p <- ggplot() +
        geom_polygon(
          data = UK_OUTLINE,
          aes(x = long, y = lat, group = group),
          fill = "#f3f2f1", colour = "#b1b4b6", linewidth = 0.3
        ) +
        geom_point(
          data = d,
          aes(x = lon, y = lat, size = pt_size, colour = value,
              text = hover_text),
          alpha = 0.85
        ) +
        scale_colour_gradient(
          low  = "#1d70b8",
          high = "#cf102d",
          name = paste0(input$value_type, suffix)
        ) +
        scale_size_identity() +
        coord_quickmap(xlim = c(-9, 3), ylim = c(49.5, 59.5)) +
        theme_void() +
        theme(
          legend.position = "right",
          plot.background = element_rect(fill = "#e8f4f8", colour = NA)
        )

      ggplotly(p, tooltip = "text") %>%
        layout(
          showlegend = FALSE,
          margin = list(l = 0, r = 0, t = 10, b = 0)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ---- Horizontal bar chart ----
    output$bar_chart <- renderPlotly({
      d <- map_df()
      req(nrow(d) > 0)

      is_rate <- input$value_type == "Rate"
      suffix  <- if (is_rate) "%" else " (000s)"
      x_title <- paste0(input$measure, " ", input$value_type, suffix)

      # Order regions by value
      d <- d[order(d$value), ]
      d$region_name <- factor(d$region_name, levels = d$region_name)

      ggplotly(
        ggplot(d, aes(x = value, y = region_name, fill = value,
                      text = paste0(region_name, ": ",
                                    format(round(value, 1), big.mark = ","),
                                    suffix))) +
          geom_col(width = 0.7, show.legend = FALSE) +
          scale_fill_gradient(low = "#1d70b8", high = "#cf102d") +
          labs(x = x_title, y = NULL) +
          theme_minimal(base_size = 13) +
          theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank(),
            axis.text.y  = element_text(size = 11),
            plot.margin   = margin(5, 15, 5, 5)
          ),
        tooltip = "text"
      ) %>%
        layout(
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ---- Data table ----
    output$region_table <- reactable::renderReactable({
      d <- map_df()
      req(nrow(d) > 0)

      is_rate <- input$value_type == "Rate"
      suffix  <- if (is_rate) "%" else " (000s)"

      display <- d[, c("region_name", "employment_group", "age_group",
                        "value_type", "value")]
      names(display) <- c("Region", "Measure", "Age Group", "Type", "Value")
      display$Value <- round(display$Value, 1)

      reactable::reactable(
        display,
        sortable    = TRUE,
        filterable  = TRUE,
        highlight   = TRUE,
        striped     = TRUE,
        defaultSorted = list(Value = "desc"),
        columns = list(
          Value = reactable::colDef(
            format = reactable::colFormat(separators = TRUE, digits = 1)
          )
        )
      )
    })
  })
}
