# R/pages/page_unemployment.R

source("R/pages/labour_market_helpers.R")

unemployment_age_codes <- data.frame(
  age_group = AGE_CHOICES,
  level_code = c("MGSC", "LF2I", "YBVH", "YBVN", "YCGM", "YCGS", "LF28", "K5HU"),
  rate_code  = c("MGSX", "LF2Q", "YBVK", "YBVQ", "YCGP", "YCGV", "LF2E", "K5HW"),
  stringsAsFactors = FALSE
)

stacked_unemployment_codes <- data.frame(
  age_group = AGE_STACK,
  code = c("YBVH", "YBVN", "YCGM", "YCGS", "LF28", "K5HU"),
  stringsAsFactors = FALSE
)

#' Unemployment Page UI
#'
#' Creates the Unemployment page with headline stats cards and the same
#' card pattern as the Employment page.
#'
#' @param id Character. The module namespace ID.
#' @return A Shiny tagList containing the complete page UI.
#' @export
unemployment_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper",
                  tags$span(class = "govuk-caption-xl", "Labour Market"),
                  tags$h1(class = "govuk-heading-xl", "Unemployment"),
                  tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),

                  div(class = "govuk-grid-row",
                      div(class = "govuk-grid-column-full",

                          tags$section(id = "unemployment-overview",
                                       div(class = "govuk-grid-row",
                                           uiOutput(ns("card_total")),
                                           uiOutput(ns("card_rate")),
                                           uiOutput(ns("card_yoy"))
                                       )
                          ),

                          tags$section(id = "unemployment-age",
                                       tags$h1(class = "govuk-heading-xl", "Unemployment by Age"),

                                       unemployment_age_stats_card_ui(
                                         id = ns("age_card")
                                       )
                          )
                      )
                  )
        )
    )
  )
}

#' Unemployment Page Server
#'
#' Server logic for the Unemployment page.
#'
#' @param id Character. The module namespace ID.
#' @export
unemployment_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Card 1: Total Unemployment (16+) — level in 000s
    output$card_total <- renderUI({
      df <- query_data(APP_DB$pool, "MGSC", divide = 1)
      req(nrow(df) >= 2)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      govuk_stats_card(
        id       = session$ns("total_unemp"),
        title    = "Total Unemployment (16+)",
        headline = paste0(govuk_format_number(round(curr)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        period   = "vs previous period",
        good_if_increase = FALSE
      )
    })

    # Card 2: Unemployment Rate (16-64) — rate in %
    output$card_rate <- renderUI({
      df <- query_data(APP_DB$pool, "LF2Q", divide = 1)
      req(nrow(df) >= 2)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      govuk_stats_card(
        id       = session$ns("unemp_rate"),
        title    = "Unemployment Rate (16-64)",
        headline = govuk_format_percent1(curr),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", delta), "pp"),
        period   = "vs previous period",
        good_if_increase = FALSE
      )
    })

    # Card 3: Year-on-Year Change — computed from MGSC
    output$card_yoy <- renderUI({
      df <- query_data(APP_DB$pool, "MGSC", divide = 1)
      req(nrow(df) >= 13)
      curr <- tail(df$value, 1)
      yoy  <- df$value[nrow(df) - 12]
      delta <- curr - yoy
      govuk_stats_card(
        id       = session$ns("yoy_change"),
        title    = "Year-on-Year Change",
        headline = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", (delta / yoy) * 100), "%"),
        period   = "vs 12 months ago",
        good_if_increase = FALSE
      )
    })

    unemployment_age_stats_card_server(id = "age_card")
  })
}
