# R/pages/page_regional_map.R
# Regional Employment Map page — plots UK regional data on an interactive map

library(ggplot2)
library(plotly)

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
    map_data <- reactive({
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

    # ---- Interactive scattergeo map ----
    output$uk_map <- renderPlotly({
      d <- map_data()
      req(nrow(d) > 0)

      is_rate <- input$value_type == "Rate"
      suffix  <- if (is_rate) "%" else " (000s)"

      # Bubble sizing
      vals <- d$value
      size_range <- if (is_rate) c(22, 48) else c(15, 55)
      if (length(unique(vals)) == 1L) {
        scaled <- rep(mean(size_range), length(vals))
      } else {
        scaled <- scales::rescale(vals, to = size_range, from = range(vals, na.rm = TRUE))
      }

      # GOV.UK colour ramp
      col_low  <- "#1d70b8"
      col_high <- "#cf102d"

      plot_ly(
        d,
        type  = "scattergeo",
        lat   = ~lat,
        lon   = ~lon,
        text  = ~paste0(
          "<b>", region_name, "</b><br>",
          input$measure, " (", input$value_type, "): ",
          format(round(value, 1), big.mark = ","), suffix
        ),
        hoverinfo = "text",
        marker = list(
          size  = scaled,
          color = vals,
          colorscale = list(c(0, col_low), c(1, col_high)),
          colorbar   = list(
            title     = list(text = paste0(input$value_type, suffix)),
            thickness = 15,
            len       = 0.5
          ),
          line    = list(color = "#ffffff", width = 1.5),
          opacity = 0.85
        )
      ) %>%
        layout(
          geo = list(
            scope          = "europe",
            resolution     = 50,
            showland       = TRUE,
            landcolor      = "#f3f2f1",
            showocean      = TRUE,
            oceancolor     = "#e8f4f8",
            showcountries  = TRUE,
            countrycolor   = "#b1b4b6",
            showcoastlines = TRUE,
            coastlinecolor = "#b1b4b6",
            lonaxis    = list(range = c(-9, 3)),
            lataxis    = list(range = c(49.5, 59.5)),
            projection = list(type = "mercator")
          ),
          showlegend = FALSE,
          margin     = list(l = 0, r = 0, t = 10, b = 0)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # ---- Horizontal bar chart ----
    output$bar_chart <- renderPlotly({
      d <- map_data()
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
      d <- map_data()
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
