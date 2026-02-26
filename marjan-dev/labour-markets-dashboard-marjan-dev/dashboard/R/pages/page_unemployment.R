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
#' Creates the Unemployment page with the same card pattern as the Employment page.
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
    unemployment_age_stats_card_server(id = "age_card")
  })
}
