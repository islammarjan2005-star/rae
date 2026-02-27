
# R/pages/page_employment.R

source("R/pages/labour_market_helpers.R")

library(ggplot2)
library(scales)
library(plotly)



#' Employment age group codes for levels and rates
#' @description Maps each age group to its ONS dataset codes
employment_age_codes <- data.frame(
  age_group = AGE_CHOICES,
  level_code = c("MGRZ", "LF2G", "YBTO", "YBTR", "YBTU", "YBTX", "LF26", "LFK4"),
  rate_code  = c("MGSR", "LF24", "YBUA", "YBUD", "YBUG", "YBUJ", "LF2U", "LFK6"),
  stringsAsFactors = FALSE
)

#' Employment codes for stacked 
stacked_employment_codes <- data.frame(
  age_group = AGE_STACK,
  code = c("YBTO", "YBTR", "YBTU", "YBTX", "LF26", "LFK4"),
  stringsAsFactors = FALSE
)



#' Employment Page UI
#'
#' Creates the full Employment page with multiple sections:
#' Overview, Employment by Age, Employment by Gender, Employment by Sector.
#'
#' @param id Character. The module namespace ID.
#' @return A Shiny tagList containing the complete page UI.
#' @export
employment_ui <- function(id) {
  ns <- NS(id)
  
  toc_sections <- list(
    list(
      heading = "Live Full Sample",
      items = c("Overview"           = "employment-overview",
                "Employment by Age"  = "employment-age")
    ),
    list(
      heading = "Full Sample Microdata",
      items = c("Employment by Gender" = "employment-gender",
                "Employment by Sector" = "employment-sector")
    )
  )
  
  tagList(
    side_nav(ns, sections = toc_sections, title = "On this page"),
    
    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper",
                  tags$span(class = "govuk-caption-xl", "Labour Market"),
                  tags$h1(class = "govuk-heading-xl", "Employment"),
                  tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),
                  
                  #  Grid-
                  div(class = "govuk-grid-row",
                      div(class = "govuk-grid-column-full",
                          
                          tags$section(id = "employment-overview",
                                       div(class = "govuk-grid-row",
                                           uiOutput(ns("card_unemploy")),
                                           uiOutput(ns("card_duration")),
                                           uiOutput(ns("card_pop"))
                                       )
                          ),
                          
                          tags$section(id = "employment-age",
                                       tags$h1(class = "govuk-heading-xl", "Employment by Age"),

                                       # Employment by Age (replicating FT/PT card pattern)
                                       employment_age_stats_card_ui(
                                         id = ns("age_card")
                                       )
                          ),
                          
                          tags$section(id = "employment-gender",
                                       tags$h1(class = "govuk-heading-xl", "Employment by Gender"),
                                       textAreaInput(ns("notes_gender"), label = NULL, value = "",
                                                     placeholder = strrep("This is a very long placeholder. ", 200))
                          ),
                          
                          tags$section(id = "employment-sector",
                                       tags$h1(class = "govuk-heading-xl", "Employment by Sector"),
                                       textAreaInput(ns("notes_sector"), label = NULL, value = "",
                                                     placeholder = strrep("This is a very long placeholder. ", 200))
                          )
                      )
                  )
        )
    )
  )
}




#' Employment Page Server
#'
#' Server logic for the Employment page. Handles data fetching, filtering,
#' and wiring up the various visualization modules.
#'
#' @param id Character. The module namespace ID.
#' @export
employment_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Card 1: Total Employment (16+) — level in 000s
    output$card_unemploy <- renderUI({
      df <- query_data(APP_DB$pool, "MGRZ", divide = 1)
      req(nrow(df) >= 2)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      govuk_stats_card(
        id       = session$ns("total_emp"),
        title    = "Total Employment (16+)",
        headline = paste0(govuk_format_number(round(curr)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        period   = "vs previous period",
        good_if_increase = TRUE
      )
    })

    # Card 2: Employment Rate (16-64) — rate in %
    output$card_duration <- renderUI({
      df <- query_data(APP_DB$pool, "LF24", divide = 1)
      req(nrow(df) >= 2)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      govuk_stats_card(
        id       = session$ns("emp_rate"),
        title    = "Employment Rate (16-64)",
        headline = govuk_format_percent1(curr),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", delta), "pp"),
        period   = "vs previous period",
        good_if_increase = TRUE
      )
    })

    # Card 3: Year-on-Year Change — computed from MGRZ
    output$card_pop <- renderUI({
      df <- query_data(APP_DB$pool, "MGRZ", divide = 1)
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
        good_if_increase = TRUE
      )
    })

    # Employment by Age (replicating FT/PT card pattern)
    employment_age_stats_card_server(
      id = "age_card"
    )

  })
}