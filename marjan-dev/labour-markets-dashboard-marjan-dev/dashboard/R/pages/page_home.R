# R/pages/page_home.R

home_ui <- function(id) {
  ns <- NS(id)

  div(class = "govuk-width-container",
    tags$main(class = "govuk-main-wrapper",
      tags$h1(class = "govuk-heading-xl", "Labour Markets Dashboard"),
      tags$p(class = "govuk-body-l",
        "Overview of UK labour market indicators from the ONS Labour Force Survey."
      ),
      tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),

      # Combined takeaway banner
      uiOutput(ns("home_takeaway")),

      # Employment section
      tags$h2(class = "govuk-heading-m", style = "margin-top: 30px;",
        tags$a(href = "#!/employment", class = "govuk-link", "Employment")
      ),
      div(class = "govuk-grid-row",
        uiOutput(ns("home_emp_total")),
        uiOutput(ns("home_emp_rate")),
        uiOutput(ns("home_emp_yoy"))
      ),

      # Unemployment section
      tags$h2(class = "govuk-heading-m", style = "margin-top: 30px;",
        tags$a(href = "#!/unemployment", class = "govuk-link", "Unemployment")
      ),
      div(class = "govuk-grid-row",
        uiOutput(ns("home_unemp_total")),
        uiOutput(ns("home_unemp_rate")),
        uiOutput(ns("home_unemp_yoy"))
      )
    )
  )
}

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Combined takeaway banner (employment + unemployment)
    output$home_takeaway <- renderUI({
      df_emp   <- query_data(APP_DB$pool, "MGRZ", divide = 1)
      df_erate <- query_data(APP_DB$pool, "LF24", divide = 1)
      df_unemp <- query_data(APP_DB$pool, "MGSC", divide = 1)
      df_urate <- query_data(APP_DB$pool, "LF2Q", divide = 1)
      req(nrow(df_emp) >= 2, nrow(df_erate) >= 2,
          nrow(df_unemp) >= 2, nrow(df_urate) >= 2)

      period <- tail(df_emp$time_period, 1)

      # Employment sentence
      emp_curr  <- tail(df_emp$value, 1)
      emp_prev  <- tail(df_emp$value, 2)[1]
      emp_delta <- emp_curr - emp_prev
      emp_dir   <- if (emp_delta >= 0) "rose" else "fell"
      erate_curr  <- tail(df_erate$value, 1)
      erate_delta <- tail(df_erate$value, 1) - tail(df_erate$value, 2)[1]
      erate_dir   <- if (abs(erate_delta) < 0.1) "held steady at"
                     else if (erate_delta > 0) "rose to" else "fell to"

      emp_sentence <- sprintf(
        "Employment %s by %sk to %sk, while the employment rate %s %s.",
        emp_dir,
        govuk_format_number(abs(round(emp_delta))),
        govuk_format_number(round(emp_curr)),
        erate_dir,
        govuk_format_percent1(erate_curr)
      )

      # Unemployment sentence
      unemp_curr  <- tail(df_unemp$value, 1)
      unemp_prev  <- tail(df_unemp$value, 2)[1]
      unemp_delta <- unemp_curr - unemp_prev
      unemp_dir   <- if (unemp_delta >= 0) "rose" else "fell"
      urate_curr  <- tail(df_urate$value, 1)
      urate_delta <- tail(df_urate$value, 1) - tail(df_urate$value, 2)[1]
      urate_dir   <- if (abs(urate_delta) < 0.1) "held steady at"
                     else if (urate_delta > 0) "rose to" else "fell to"

      unemp_sentence <- sprintf(
        "Unemployment %s by %sk to %sk, while the unemployment rate %s %s.",
        unemp_dir,
        govuk_format_number(abs(round(unemp_delta))),
        govuk_format_number(round(unemp_curr)),
        urate_dir,
        govuk_format_percent1(urate_curr)
      )

      tags$div(class = "govuk-inset-text", style = "border-color: #1d70b8;",
        tags$p(class = "govuk-body",
          tags$strong(paste0("In ", period, ": ")),
          emp_sentence, " ", unemp_sentence
        )
      )
    })

    # --- Employment cards ---

    output$home_emp_total <- renderUI({
      df <- query_data(APP_DB$pool, "MGRZ", divide = 1)
      req(nrow(df) >= 2)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      govuk_stats_card(
        id       = session$ns("emp_total"),
        title    = "Total Employment (16+)",
        subtitle = latest_period,
        headline = paste0(govuk_format_number(round(curr)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        period   = "vs previous period",
        good_if_increase = TRUE
      )
    })

    output$home_emp_rate <- renderUI({
      df <- query_data(APP_DB$pool, "LF24", divide = 1)
      req(nrow(df) >= 2)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      govuk_stats_card(
        id       = session$ns("emp_rate"),
        title    = "Employment Rate (16-64)",
        subtitle = latest_period,
        headline = govuk_format_percent1(curr),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", delta), "pp"),
        period   = "vs previous period",
        good_if_increase = TRUE
      )
    })

    output$home_emp_yoy <- renderUI({
      df <- query_data(APP_DB$pool, "MGRZ", divide = 1)
      req(nrow(df) >= 13)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1)
      yoy  <- df$value[nrow(df) - 12]
      delta <- curr - yoy
      govuk_stats_card(
        id       = session$ns("emp_yoy"),
        title    = "Employment YoY Change",
        subtitle = latest_period,
        headline = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", (delta / yoy) * 100), "%"),
        period   = "vs 12 months ago",
        good_if_increase = TRUE
      )
    })

    # --- Unemployment cards ---

    output$home_unemp_total <- renderUI({
      df <- query_data(APP_DB$pool, "MGSC", divide = 1)
      req(nrow(df) >= 2)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      govuk_stats_card(
        id       = session$ns("unemp_total"),
        title    = "Total Unemployment (16+)",
        subtitle = latest_period,
        headline = paste0(govuk_format_number(round(curr)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        period   = "vs previous period",
        good_if_increase = FALSE
      )
    })

    output$home_unemp_rate <- renderUI({
      df <- query_data(APP_DB$pool, "LF2Q", divide = 1)
      req(nrow(df) >= 2)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1); prev <- tail(df$value, 2)[1]
      delta <- curr - prev
      govuk_stats_card(
        id       = session$ns("unemp_rate"),
        title    = "Unemployment Rate (16-64)",
        subtitle = latest_period,
        headline = govuk_format_percent1(curr),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", delta), "pp"),
        period   = "vs previous period",
        good_if_increase = FALSE
      )
    })

    output$home_unemp_yoy <- renderUI({
      df <- query_data(APP_DB$pool, "MGSC", divide = 1)
      req(nrow(df) >= 13)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1)
      yoy  <- df$value[nrow(df) - 12]
      delta <- curr - yoy
      govuk_stats_card(
        id       = session$ns("unemp_yoy"),
        title    = "Unemployment YoY Change",
        subtitle = latest_period,
        headline = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", (delta / yoy) * 100), "%"),
        period   = "vs 12 months ago",
        good_if_increase = FALSE
      )
    })
  })
}
