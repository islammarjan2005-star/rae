
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
    skeleton_card_css(),
    back_to_top_css(),

    side_nav(ns, sections = toc_sections, title = "On this page"),

    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper", id = "main-content",
                  tags$span(class = "govuk-caption-xl", "Labour Market"),
                  tags$h1(class = "govuk-heading-xl", "Employment"),
                  tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),

                  #  Grid-
                  div(class = "govuk-grid-row",
                      div(class = "govuk-grid-column-full",

                          tags$section(id = "employment-overview",
                                       div(class = "skeleton-container", uiOutput(ns("takeaway_banner"))),
                                       div(class = "govuk-grid-row",
                                           div(class = "skeleton-container", uiOutput(ns("card_unemploy"))),
                                           div(class = "skeleton-container", uiOutput(ns("card_duration"))),
                                           div(class = "skeleton-container", uiOutput(ns("card_pop")))
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
                                       coming_soon_card("This section is under development.")
                          ),

                          tags$section(id = "employment-sector",
                                       tags$h1(class = "govuk-heading-xl", "Employment by Sector"),
                                       coming_soon_card("This section is under development.")
                          )
                      )
                  )
        )
    ),

    # Back to top
    tags$a(id = ns("back_to_top"), href = "#", class = "govuk-link back-to-top",
           HTML("&uarr; Back to top")),
    tags$script(HTML(sprintf("
      $(function(){
        var btn = document.getElementById('%s');
        if (!btn) return;
        window.addEventListener('scroll', function(){
          btn.classList.toggle('visible', window.scrollY > 400);
        });
        btn.addEventListener('click', function(e){
          e.preventDefault();
          window.scrollTo({top:0, behavior:'smooth'});
        });
      });
    ", ns("back_to_top"))))
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

    # Takeaway banner (notification-banner style)
    output$takeaway_banner <- renderUI({
      df_level <- query_data(APP_DB$pool, "MGRZ", divide = 1)
      df_rate  <- query_data(APP_DB$pool, "LF24", divide = 1)
      req(nrow(df_level) >= 2, nrow(df_rate) >= 2)

      period     <- tail(df_level$time_period, 1)
      emp_curr   <- tail(df_level$value, 1)
      emp_prev   <- tail(df_level$value, 2)[1]
      emp_delta  <- emp_curr - emp_prev
      rate_curr  <- tail(df_rate$value, 1)
      rate_prev  <- tail(df_rate$value, 2)[1]
      rate_delta <- rate_curr - rate_prev

      direction <- if (emp_delta >= 0) "rose" else "fell"
      rate_dir  <- if (abs(rate_delta) < 0.1) "held steady at"
                   else if (rate_delta > 0) "rose to" else "fell to"

      summary <- sprintf(
        "Total employment %s by %sk to %sk in %s, while the employment rate %s %s.",
        direction,
        govuk_format_number(abs(round(emp_delta))),
        govuk_format_number(round(emp_curr)),
        period, rate_dir,
        govuk_format_percent1(rate_curr)
      )

      tags$div(
        class = "govuk-notification-banner",
        role = "region",
        `aria-labelledby` = "emp-banner-title",
        `data-module` = "govuk-notification-banner",
        tags$div(class = "govuk-notification-banner__header",
          tags$h2(class = "govuk-notification-banner__title",
                  id = "emp-banner-title", "Latest from the data")
        ),
        tags$div(class = "govuk-notification-banner__content",
          tags$p(class = "govuk-notification-banner__heading", summary)
        )
      )
    })

    # Card 1: Total Employment (16+) — level in 000s
    output$card_unemploy <- renderUI({
      df <- query_data(APP_DB$pool, "MGRZ", divide = 1)
      req(nrow(df) >= 2)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      spark <- sparkline_svg(tail(df$value, 12), colour = "#00703c")
      govuk_stats_card(
        id       = session$ns("total_emp"),
        title    = "Total Employment (16+)",
        subtitle = latest_period,
        sparkline = spark,
        headline = paste0(govuk_format_number(round(curr)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        period   = "vs previous period",
        accent_hex = "#00703c",
        good_if_increase = TRUE
      )
    })

    # Card 2: Employment Rate (16-64) — rate in %
    output$card_duration <- renderUI({
      df <- query_data(APP_DB$pool, "LF24", divide = 1)
      req(nrow(df) >= 2)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      spark <- sparkline_svg(tail(df$value, 12), colour = "#00703c")
      govuk_stats_card(
        id       = session$ns("emp_rate"),
        title    = "Employment Rate (16-64)",
        subtitle = latest_period,
        sparkline = spark,
        headline = govuk_format_percent1(curr),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", delta), "pp"),
        period   = "vs previous period",
        accent_hex = "#00703c",
        good_if_increase = TRUE
      )
    })

    # Card 3: Year-on-Year Change — computed from MGRZ
    output$card_pop <- renderUI({
      df <- query_data(APP_DB$pool, "MGRZ", divide = 1)
      req(nrow(df) >= 13)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1)
      yoy  <- df$value[nrow(df) - 12]
      delta <- curr - yoy
      spark <- sparkline_svg(tail(df$value, 12), colour = "#00703c")
      govuk_stats_card(
        id       = session$ns("yoy_change"),
        title    = "Year-on-Year Change",
        subtitle = latest_period,
        sparkline = spark,
        headline = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", (delta / yoy) * 100), "%"),
        period   = "vs 12 months ago",
        accent_hex = "#00703c",
        good_if_increase = TRUE
      )
    })

    # Employment by Age (replicating FT/PT card pattern)
    employment_age_stats_card_server(
      id = "age_card"
    )

  })
}
