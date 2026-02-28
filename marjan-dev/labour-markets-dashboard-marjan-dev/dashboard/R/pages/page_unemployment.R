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
#' Creates the Unemployment page with headline stats cards, takeaway banner,
#' side navigation, and the same card pattern as the Employment page.
#'
#' @param id Character. The module namespace ID.
#' @return A Shiny tagList containing the complete page UI.
#' @export
unemployment_ui <- function(id) {
  ns <- NS(id)

  toc_sections <- list(
    list(
      heading = "Live Full Sample",
      items = c("Overview"              = "unemployment-overview",
                "Unemployment by Age"   = "unemployment-age")
    )
  )

  tagList(
    skeleton_card_css(),
    back_to_top_css(),

    side_nav(ns, sections = toc_sections, title = "On this page"),

    div(class = "govuk-width-container",
        tags$main(class = "govuk-main-wrapper", id = "main-content",
                  tags$span(class = "govuk-caption-xl", "Labour Market"),
                  tags$h1(class = "govuk-heading-xl", "Unemployment"),
                  tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),

                  div(class = "govuk-grid-row",
                      div(class = "govuk-grid-column-full",

                          tags$section(id = "unemployment-overview",
                                       div(class = "skeleton-container", uiOutput(ns("takeaway_banner"))),
                                       div(class = "govuk-grid-row",
                                           div(class = "skeleton-container", uiOutput(ns("card_total"))),
                                           div(class = "skeleton-container", uiOutput(ns("card_rate"))),
                                           div(class = "skeleton-container", uiOutput(ns("card_yoy")))
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

#' Unemployment Page Server
#'
#' Server logic for the Unemployment page.
#'
#' @param id Character. The module namespace ID.
#' @export
unemployment_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Takeaway banner (notification-banner style)
    output$takeaway_banner <- renderUI({
      df_level <- query_data(APP_DB$pool, "MGSC", divide = 1)
      df_rate  <- query_data(APP_DB$pool, "LF2Q", divide = 1)
      req(nrow(df_level) >= 2, nrow(df_rate) >= 2)

      period      <- tail(df_level$time_period, 1)
      unemp_curr  <- tail(df_level$value, 1)
      unemp_prev  <- tail(df_level$value, 2)[1]
      unemp_delta <- unemp_curr - unemp_prev
      rate_curr   <- tail(df_rate$value, 1)
      rate_prev   <- tail(df_rate$value, 2)[1]
      rate_delta  <- rate_curr - rate_prev

      direction <- if (unemp_delta >= 0) "rose" else "fell"
      rate_dir  <- if (abs(rate_delta) < 0.1) "held steady at"
                   else if (rate_delta > 0) "rose to" else "fell to"

      summary <- sprintf(
        "Total unemployment %s by %sk to %sk in %s, while the unemployment rate %s %s.",
        direction,
        govuk_format_number(abs(round(unemp_delta))),
        govuk_format_number(round(unemp_curr)),
        period, rate_dir,
        govuk_format_percent1(rate_curr)
      )

      tags$div(
        class = "govuk-notification-banner",
        role = "region",
        `aria-labelledby` = "unemp-banner-title",
        `data-module` = "govuk-notification-banner",
        tags$div(class = "govuk-notification-banner__header",
          tags$h2(class = "govuk-notification-banner__title",
                  id = "unemp-banner-title", "Latest from the data")
        ),
        tags$div(class = "govuk-notification-banner__content",
          tags$p(class = "govuk-notification-banner__heading", summary)
        )
      )
    })

    # Card 1: Total Unemployment (16+) — level in 000s
    output$card_total <- renderUI({
      df <- query_data(APP_DB$pool, "MGSC", divide = 1)
      req(nrow(df) >= 2)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      spark <- sparkline_svg(tail(df$value, 12), colour = "#f47738")
      govuk_stats_card(
        id       = session$ns("total_unemp"),
        title    = "Total Unemployment (16+)",
        subtitle = latest_period,
        sparkline = spark,
        headline = paste0(govuk_format_number(round(curr)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        period   = "vs previous period",
        accent_hex = "#f47738",
        good_if_increase = FALSE
      )
    })

    # Card 2: Unemployment Rate (16-64) — rate in %
    output$card_rate <- renderUI({
      df <- query_data(APP_DB$pool, "LF2Q", divide = 1)
      req(nrow(df) >= 2)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      spark <- sparkline_svg(tail(df$value, 12), colour = "#f47738")
      govuk_stats_card(
        id       = session$ns("unemp_rate"),
        title    = "Unemployment Rate (16-64)",
        subtitle = latest_period,
        sparkline = spark,
        headline = govuk_format_percent1(curr),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", delta), "pp"),
        period   = "vs previous period",
        accent_hex = "#f47738",
        good_if_increase = FALSE
      )
    })

    # Card 3: Year-on-Year Change — computed from MGSC
    output$card_yoy <- renderUI({
      df <- query_data(APP_DB$pool, "MGSC", divide = 1)
      req(nrow(df) >= 13)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1)
      yoy  <- df$value[nrow(df) - 12]
      delta <- curr - yoy
      spark <- sparkline_svg(tail(df$value, 12), colour = "#f47738")
      govuk_stats_card(
        id       = session$ns("yoy_change"),
        title    = "Year-on-Year Change",
        subtitle = latest_period,
        sparkline = spark,
        headline = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", (delta / yoy) * 100), "%"),
        period   = "vs 12 months ago",
        accent_hex = "#f47738",
        good_if_increase = FALSE
      )
    })

    unemployment_age_stats_card_server(id = "age_card")
  })
}
