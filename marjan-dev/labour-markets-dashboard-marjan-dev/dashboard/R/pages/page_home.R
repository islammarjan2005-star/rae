# R/pages/page_home.R

home_ui <- function(id) {
  ns <- NS(id)

  tagList(
    skeleton_card_css(),

    # --- Scoped CSS for home page zones ---
    tags$style(HTML("
      /* Zone 1: Hero panel */
      .home-hero {
        background: linear-gradient(135deg, #cf102d 0%, #8b0b1e 100%);
        padding: 45px 0 40px;
        margin-bottom: 0;
      }
      .home-hero .govuk-panel {
        background: transparent;
        border: none;
        text-align: left;
        padding: 0;
      }
      .home-hero .govuk-panel__title {
        color: #fff;
        margin-bottom: 10px;
      }
      .home-hero .govuk-panel__body {
        color: rgba(255,255,255,0.85);
        font-size: 19px;
      }
      .home-hero .home-hero__meta {
        margin-top: 15px;
        display: flex;
        align-items: center;
        gap: 15px;
      }
      .home-hero .home-hero__updated {
        color: rgba(255,255,255,0.7);
        font-size: 16px;
      }

      /* Zone 3: Swim lanes */
      .home-lane {
        border-left: 5px solid #b1b4b6;
        padding-left: 25px;
        margin-top: 25px;
        margin-bottom: 10px;
      }
      .home-lane--green  { border-left-color: #00703c; }
      .home-lane--orange { border-left-color: #f47738; }

      .home-lane__header {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 10px;
      }
      .home-lane__header .govuk-heading-m {
        margin-bottom: 0;
      }
      .home-lane__arrow {
        font-size: 20px;
        text-decoration: none;
        font-weight: 700;
      }

      /* Zone 4: Navigation cards */
      .home-nav-grid {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 20px;
        margin-top: 45px;
        margin-bottom: 30px;
      }
      @media (max-width: 640px) {
        .home-nav-grid {
          grid-template-columns: 1fr;
        }
      }
      .home-nav-card {
        background: #fff;
        border: 1px solid #b1b4b6;
        padding: 25px 20px 20px;
        transition: transform 0.15s ease, box-shadow 0.15s ease;
        text-decoration: none;
        display: block;
        color: inherit;
      }
      .home-nav-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 4px 15px rgba(0,0,0,0.12);
      }
      .home-nav-card--green  { border-top: 5px solid #00703c; }
      .home-nav-card--red    { border-top: 5px solid #d4351c; }
      .home-nav-card--blue   { border-top: 5px solid #1d70b8; }

      .home-nav-card__title {
        margin-bottom: 8px;
      }
      .home-nav-card__desc {
        margin-bottom: 12px;
      }
      .home-nav-card__link {
        font-weight: 700;
      }
    ")),

    # --- Zone 1: Hero banner (full-bleed) ---
    div(class = "home-hero",
      div(class = "govuk-width-container",
        div(class = "govuk-panel",
          tags$h1(class = "govuk-panel__title", "Labour Markets Dashboard"),
          div(class = "govuk-panel__body",
            "UK labour market indicators from the ONS Labour Force Survey"
          ),
          div(class = "home-hero__meta",
            uiOutput(ns("hero_period_badge"), inline = TRUE),
            tags$span(class = "home-hero__updated", paste("Last updated:", Sys.Date()))
          )
        )
      )
    ),

    # --- Zones 2-4: Main content ---
    div(class = "govuk-width-container",
      tags$main(class = "govuk-main-wrapper",

        # Zone 2: Notification banner (takeaway)
        div(class = "skeleton-container", uiOutput(ns("home_takeaway"))),

        # Zone 3a: Employment swim lane
        div(class = "home-lane home-lane--green",
          div(class = "home-lane__header",
            tags$h2(class = "govuk-heading-m",
              tags$a(href = "#!/employment", class = "govuk-link", "Employment")
            ),
            tags$a(href = "#!/employment", class = "home-lane__arrow govuk-link",
              HTML("&rarr;")
            )
          ),
          div(class = "govuk-grid-row",
            div(class = "skeleton-container", uiOutput(ns("home_emp_total"))),
            div(class = "skeleton-container", uiOutput(ns("home_emp_rate"))),
            div(class = "skeleton-container", uiOutput(ns("home_emp_yoy")))
          )
        ),

        # Separator
        tags$hr(class = "govuk-section-break govuk-section-break--l govuk-section-break--visible"),

        # Zone 3b: Unemployment swim lane
        div(class = "home-lane home-lane--orange",
          div(class = "home-lane__header",
            tags$h2(class = "govuk-heading-m",
              tags$a(href = "#!/unemployment", class = "govuk-link", "Unemployment")
            ),
            tags$a(href = "#!/unemployment", class = "home-lane__arrow govuk-link",
              HTML("&rarr;")
            )
          ),
          div(class = "govuk-grid-row",
            div(class = "skeleton-container", uiOutput(ns("home_unemp_total"))),
            div(class = "skeleton-container", uiOutput(ns("home_unemp_rate"))),
            div(class = "skeleton-container", uiOutput(ns("home_unemp_yoy")))
          )
        ),

        # Separator
        tags$hr(class = "govuk-section-break govuk-section-break--l govuk-section-break--visible"),

        # Zone 4: Explore more navigation cards
        tags$h2(class = "govuk-heading-m", "Explore in detail"),
        div(class = "home-nav-grid",
          tags$a(href = "#!/employment", class = "home-nav-card home-nav-card--green",
            tags$h3(class = "govuk-heading-s home-nav-card__title", "Employment"),
            tags$p(class = "govuk-body-s home-nav-card__desc",
              "Levels, rates, and trends by age group, gender, and sector."
            ),
            tags$span(class = "govuk-link home-nav-card__link", "View employment data", HTML(" &rarr;"))
          ),
          tags$a(href = "#!/unemployment", class = "home-nav-card home-nav-card--red",
            tags$h3(class = "govuk-heading-s home-nav-card__title", "Unemployment"),
            tags$p(class = "govuk-body-s home-nav-card__desc",
              "Unemployment levels and rates broken down by age group."
            ),
            tags$span(class = "govuk-link home-nav-card__link", "View unemployment data", HTML(" &rarr;"))
          ),
          tags$a(href = "#!/regional_map", class = "home-nav-card home-nav-card--blue",
            tags$h3(class = "govuk-heading-s home-nav-card__title", "Regional Map"),
            tags$p(class = "govuk-body-s home-nav-card__desc",
              "Geographic breakdown of labour market indicators across UK regions."
            ),
            tags$span(class = "govuk-link home-nav-card__link", "View regional data", HTML(" &rarr;"))
          )
        )
      )
    )
  )
}

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Hero period badge — shows latest data period as a tag
    output$hero_period_badge <- renderUI({
      df <- query_data(APP_DB$pool, "MGRZ", divide = 1)
      req(nrow(df) >= 1)
      latest_period <- tail(df$time_period, 1)
      tags$strong(class = "govuk-tag govuk-tag--light-blue", latest_period)
    })

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

      # GOV.UK notification banner
      tags$div(
        class = "govuk-notification-banner",
        role = "region",
        `aria-labelledby` = "home-banner-title",
        `data-module` = "govuk-notification-banner",
        tags$div(class = "govuk-notification-banner__header",
          tags$h2(class = "govuk-notification-banner__title",
                  id = "home-banner-title", "Latest from the data")
        ),
        tags$div(class = "govuk-notification-banner__content",
          tags$p(class = "govuk-notification-banner__heading",
            paste0("In ", period, ": "),
            emp_sentence, " ", unemp_sentence
          )
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
      spark <- sparkline_svg(tail(df$value, 12), colour = "#00703c")
      govuk_stats_card(
        id       = session$ns("emp_total"),
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

    output$home_emp_rate <- renderUI({
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

    output$home_emp_yoy <- renderUI({
      df <- query_data(APP_DB$pool, "MGRZ", divide = 1)
      req(nrow(df) >= 13)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1)
      yoy  <- df$value[nrow(df) - 12]
      delta <- curr - yoy
      spark <- sparkline_svg(tail(df$value, 12), colour = "#00703c")
      govuk_stats_card(
        id       = session$ns("emp_yoy"),
        title    = "Employment YoY Change",
        subtitle = latest_period,
        sparkline = spark,
        headline = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", (delta / yoy) * 100), "%"),
        period   = "vs 12 months ago",
        accent_hex = "#00703c",
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
      spark <- sparkline_svg(tail(df$value, 12), colour = "#f47738")
      govuk_stats_card(
        id       = session$ns("unemp_total"),
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

    output$home_unemp_rate <- renderUI({
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

    output$home_unemp_yoy <- renderUI({
      df <- query_data(APP_DB$pool, "MGSC", divide = 1)
      req(nrow(df) >= 13)
      latest_period <- tail(df$time_period, 1)
      curr <- tail(df$value, 1)
      yoy  <- df$value[nrow(df) - 12]
      delta <- curr - yoy
      spark <- sparkline_svg(tail(df$value, 12), colour = "#f47738")
      govuk_stats_card(
        id       = session$ns("unemp_yoy"),
        title    = "Unemployment YoY Change",
        subtitle = latest_period,
        sparkline = spark,
        headline = paste0(ifelse(delta >= 0, "+", ""), govuk_format_number(round(delta)), "k"),
        delta    = paste0(ifelse(delta >= 0, "+", ""), sprintf("%.1f", (delta / yoy) * 100), "%"),
        period   = "vs 12 months ago",
        accent_hex = "#f47738",
        good_if_increase = FALSE
      )
    })
  })
}
