
#' Simple number formatter (GOV.UK style)
#'
#' @param x Numeric vector.
#' @return Character vector with thousands separators.
#' @examples
#' govuk_format_number(12345)  # "12,345"
#' @export
govuk_format_number <- function(x) {
  scales::comma(x)
}

#' Simple percent formatter (1 decimal place)
#'
#' Expects values already on the 0–100 scale (e.g., 12.3 -> "12.3%").
#'
#' @param x Numeric vector (percent values).
#' @return Character vector like "12.3%".
#' @examples
#' govuk_format_percent1(12.3)  # "12.3%"
#' @export
govuk_format_percent1 <- function(x) {
  sprintf("%.1f%%", x)
}


#' Internal: choose GOV.UK tag colour from a delta
#'
#' @param delta Numeric or character; leading "+" / "-" parsed if character.
#' @param good_if_increase Logical; if TRUE, positive is "green", else "red".
#' @return One of "green", "red", or "blue".
#' @keywords internal
#' @noRd
.govuk_tag_colour <- function(delta, good_if_increase = TRUE) {
  if (is.null(delta)) return("blue")  # neutral when no delta

  # Determine sign for numeric or "+/-" strings
  sign <- if (is.numeric(delta)) {
    if (delta > 0) 1L else if (delta < 0) -1L else 0L
  } else if (is.character(delta)) {
    if (grepl("^\\s*\\+", delta)) 1L else if (grepl("^\\s*-", delta)) -1L else 0L
  } else 0L

  if (sign == 0) {
    "blue"
  } else if (good_if_increase) {
    if (sign > 0) "green" else "red"
  } else {
    if (sign > 0) "red" else "green"
  }
}



#' GOV.UK-style statistics summary card
#'
#' Displays a headline value and optional delta tag with a short period label.
#'
#' @param id HTML id for the card.
#' @param title Short title above the headline.
#' @param headline Character or numeric; main value (use `format_headline` for numeric).
#' @param delta Optional character or numeric change (use `format_delta` for numeric).
#' @param period Character label appended after delta (default "vs last month").
#' @param accent_hex Hex colour for the top border (default "#cf102d").
#' @param good_if_increase Logical; TRUE -> positive is good (green).
#' @param tag_colour Optional override: "green"|"red"|"blue".
#' @param width_class GOV.UK grid class for width.
#' @param classes Extra classes on the summary card.
#' @param format_headline Optional function to format numeric `headline`.
#' @param format_delta Optional function to format numeric `delta`.
#' @return An `htmltools` tag `<div>`.
#' @examples
#' govuk_stats_card(
#'   id = "rate",
#'   title = "Unemployment rate",
#'   headline = 4.9,
#'   delta = -0.3,
#'   period = "vs last month",
#'   format_headline = govuk_format_percent1,
#'   format_delta = govuk_format_percent1,
#'   good_if_increase = FALSE
#' )
#' @export
govuk_stats_card <- function(
    id,
    title,
    headline,                  # formatted string OR numeric; main value
    delta = NULL,              # formatted string OR numeric; shown as govuk-tag
    period = "vs last month",  # label after delta
    accent_hex = "#cf102d",    # top border accent (DBT red)
    good_if_increase = TRUE,   # TRUE: increase => green; FALSE: increase => red
    tag_colour = NULL,         # override: 'green'|'red'|'blue'
    width_class = "govuk-grid-column-one-third",
    classes = NULL,            # extra classes on the card
    format_headline = NULL,    # formatter for numeric headline
    format_delta = NULL        # formatter for numeric delta
) {
  # Headline
  headline_out <- if (is.numeric(headline) && !is.null(format_headline)) {
    format_headline(headline)
  } else headline

  # Delta (optional)
  delta_out <- NULL
  if (!is.null(delta)) {
    delta_out <- if (is.numeric(delta) && !is.null(format_delta)) {
      format_delta(delta)
    } else delta
  }

  # Tag colour (override > inferred)
  tag_col <- if (!is.null(tag_colour)) tag_colour else .govuk_tag_colour(delta, good_if_increase)
  if (!tag_col %in% c("green", "red", "blue")) tag_col <- "blue"

  # HTML
  htmltools::tags$div(
    class = width_class,
    htmltools::tags$div(
      id    = id,
      class = paste("govuk-summary-card", if (!is.null(classes)) classes else ""),
      style = paste0("padding:15px; background:#f3f2f1; border-top:4px solid ", accent_hex, ";"),
      htmltools::tags$h3(class = "govuk-heading-s", title),
      htmltools::tags$h2(class = "govuk-heading-l", headline_out),
      if (!is.null(delta_out)) htmltools::tags$strong(
        class = paste0("govuk-tag govuk-tag--", tag_col),
        paste(delta_out, period)
      )
    )
  )
}

#' GOV.UK / UKHSA data-visualisation card (UI-only)
#'
#' Title + hint + optional filter dropdown (regular controls and/or accordion),
#' then tabs with panels for a chart, a table, and a download area.
#' No server logic here; wire outputs in your server module/app.
#'
#' @param id Module id.
#' @param title Card title (shown in header).
#' @param help_text Optional hint paragraph under the title.
#' @param visual_content UI for the visual (e.g., plotOutput, highchartOutput).
#' @param table_content UI for the table (e.g., reactableOutput, DT::dataTableOutput).
#' @param controls Optional list of general controls (e.g., sliderInput, selectInput).
#' @param accordion_controls FALSE or UI tags to render inside a collapsible panel.
#' @param query Optional output id (character) to show in the download tab as verbatim text.
#' @return An `htmltools::tagList`.
#'
#' @examples
#' # ui <- fluidPage(
#' #   mod_govuk_data_vis_card_ui(
#' #     id = "jobs",
#' #     title = "Jobs dashboard",
#' #     help_text = "Explore trends and export data.",
#' #     visual_content = plotOutput("jobs_plot"),
#' #     table_content  = reactable::reactableOutput("jobs_tbl"),
#' #     controls = list(selectInput("geo", "Area", choices = c("UK","England"))),
#' #     accordion_controls = list(dateRangeInput("rng", "Period"))
#' #   )
#' # )
mod_govuk_data_vis_card_ui <- function(
  id,
  title,
  help_text = NULL,
  visual_content = NULL,
  table_content = NULL,
  controls = NULL,            # plain controls (shown in dropdown)
  accordion_controls = FALSE, # set to UI tags to enable accordion; FALSE => none
  query = NULL
) {
  ns <- shiny::NS(id)

  # ---- General controls (if any) ----
  controls_resolved <- NULL
  if (!is.null(controls) && length(controls) > 0) {
    controls_resolved <- htmltools::tagList(controls)
  }

  # ---- Optional accordion (closed by default) ----
  accordion_ui     <- NULL
  accordion_assets <- NULL

  has_accordion <-
    !identical(accordion_controls, FALSE) &&
    !is.null(accordion_controls) &&
    length(accordion_controls) > 0

  if (has_accordion) {
    accordion_inner <- htmltools::tagList(accordion_controls)

    accordion_ui <- htmltools::div(
      id = ns("chart-controls-acc"),
      shinyBS::bsCollapse(
        id   = ns("chart_controls_collapse"),
        open = NULL,  # closed initially
        shinyBS::bsCollapsePanel(
          title = "Chart Controls",
          value = "chart-controls",
          accordion_inner
        )
      )
    )

    # Scoped CSS/JS for accordion cosmetics and dropdown behaviour
    accordion_assets <- htmltools::singleton(
      htmltools::tags$head(
        htmltools::tags$style(
          htmltools::HTML(sprintf("
            /* ======================= ACCORDION SCOPE ======================= */
            #%s .panel {
              border: 0 !important; box-shadow: none !important; background: transparent !important;
              margin-bottom: 0.5rem;
            }
            #%s .panel-heading {
              background: transparent !important; border: 0 !important; padding: 8px 0;
            }
            #%s .panel-title a {
              display: block; position: relative; padding-right: 28px;
              text-decoration: none; color: inherit;
              font-size: 20px;
              line-height: 1.3; cursor: pointer;
              user-select: none; -webkit-user-select: none; -ms-user-select: none;
              -webkit-tap-highlight-color: transparent;
              background: transparent !important; outline: none !important; box-shadow: none !important;
            }
            #%s .panel-title a:hover,
            #%s .panel-title a:focus,
            #%s .panel-title a:active,
            #%s .panel-title a:visited {
              text-decoration: none !important; color: inherit !important;
              background: transparent !important; outline: none !important; box-shadow: none !important;
            }
            #%s .panel-title a:focus-visible {
              outline: 2px dotted rgba(0,0,0,0.35); outline-offset: 2px;
            }
            #%s .panel-body {
              border-top: 0 !important; padding: 8px 0 0 0; background: transparent !important;
            }
            /* Chevron using Font Awesome 4 */
            #%s .panel-title a:after {
              content: '\\f078'; font-family: 'FontAwesome';
              position: absolute; right: 0; top: 50%%;
              transform: translateY(-50%%) rotate(0deg); transition: transform .2s ease;
              font-size: 12px; opacity: 0.8;
            }
            #%s .panel-title a[aria-expanded='true']::after {
              transform: translateY(-50%%) rotate(180deg);
            }

            /* ======================= DROPDOWN SCOPE ======================== */
            .lm-card-ukhsa .dropdown-menu {
              padding: 12px 14px;
              font-size: 14px;
              line-height: 1.35;
            }
            .lm-card-ukhsa .dropdown-menu .shiny-input-container > label,
            .lm-card-ukhsa .dropdown-menu .control-label {
              font-size: 14px;
              margin-bottom: 6px;
            }
            .lm-card-ukhsa .dropdown-menu .btn-group .btn {
              font-size: 13px;
            }
            .lm-card-ukhsa .dropdown-menu .btn:focus,
            .lm-card-ukhsa .dropdown-menu .btn:active,
            .lm-card-ukhsa .dropdown-menu .btn:hover {
              box-shadow: none !important; outline: none !important;
            }
          ",
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc"),
          ns("chart-controls-acc")
          ))
        ),
        # Keep dropdown open when toggling accordion header inside shinyWidgets::dropdown
        htmltools::tags$script(htmltools::HTML("
          $(document).on('click', '.sw-dropdown .dropdown-menu .panel-heading, \
                                 .sw-dropdown .dropdown-menu .panel-title a', function(e){
            e.stopPropagation();
          });
        "))
      )
    )
  }

  # ---- Filters dropdown: combine general controls and accordion (if any) ----
  dropdown_body <- htmltools::tagList(controls_resolved, accordion_ui)

  dropdown_btn <- NULL
  if (length(dropdown_body) > 0) {
    dropdown_btn <- shinyWidgets::dropdown(
      dropdown_body,
      label   = "Filters",
      style   = "material-circle",
      icon    = shiny::icon("sliders-h"),
      status  = "danger",
      size    = "md",
      right   = TRUE,
      tooltip = shinyWidgets::tooltipOptions(title = "Show filters")
    )
  }

  # ---- Card structure: header + tabs + panels ----
  htmltools::tagList(
    # Load accordion assets only when used
    accordion_assets,

    # Asset hook for tab behaviour (provide this helper elsewhere)
    ukhsa_card_tabs_assets(),

    htmltools::tags$div(
      class = "lm-card-ukhsa",

      # Header
      htmltools::tags$div(
        class = "ukhsa-card-header",
        htmltools::tags$div(
          class = "ukhsa-card-header__left",
          htmltools::tags$h2(class = "govuk-heading-m", title),
          if (!is.null(help_text)) htmltools::tags$p(class = "govuk-hint", help_text)
        ),
        htmltools::tags$div(
          class = "ukhsa-card-header__right",
          dropdown_btn
        )
      ),

      # Tabs + panels
      htmltools::tags$div(
        class = "ukhsa-tabs",

        # Tab buttons
        htmltools::tags$div(
          class = "ukhsa-tabs__list", role = "tablist",
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "true", tabindex = "0",
            `data-target` = ns("chart"), "Chart"
          ),
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "false", tabindex = "-1",
            `data-target` = ns("table"), "Tabular data"
          ),
          htmltools::tags$a(
            class = "ukhsa-tabs__tab",
            role = "tab", `aria-selected` = "false", tabindex = "-1",
            `data-target` = ns("download"), "Download"
          )
        ),

        # Panels
        htmltools::tags$div(
          id = ns("chart"), class = "ukhsa-tabs__panel", role = "tabpanel",
          if (is.null(visual_content))
            htmltools::tags$p(class = "govuk-hint", "Visual Content placeholder")
          else visual_content
        ),
        htmltools::tags$div(
          id = ns("table"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          if (is.null(table_content))
            htmltools::tags$p(class = "govuk-hint", "Table placeholder")
          else table_content
        ),
        htmltools::tags$div(
          id = ns("download"), class = "ukhsa-tabs__panel is-hidden", role = "tabpanel",
          if (is.null(query))
            htmltools::tags$p(class = "govuk-hint", "Download placeholder")
          else htmltools::tags$div(class = "code-block", shiny::verbatimTextOutput(query))
        )
      )
    ),

    # Card-level layout tweaks
    htmltools::tags$style(htmltools::HTML("
      .lm-card-ukhsa .ukhsa-card-header {
        display: flex; align-items: flex-start; justify-content: space-between;
        gap: 12px; margin-bottom: 6px;
      }
      .lm-card-ukhsa .ukhsa-card-header__left { min-width: 0; }
      .lm-card-ukhsa .ukhsa-card-header__right { display: flex; gap: 8px; align-items: flex-start; }
      .dropdown-menu { padding: 12px 14px; }
      .dropdown-menu .form-group, .dropdown-menu .sw-control, .dropdown-menu .shiny-input-container { margin-bottom: 10px; }
      @media (max-width: 640px) {
        .lm-card-ukhsa .ukhsa-card-header { flex-direction: column; align-items: stretch; gap: 8px; }
        .lm-card-ukhsa .ukhsa-card-header__right { justify-content: flex-end; }
      }
    "))
  )
}


#' GOV.UK / UKHSA data-visualisation card (server)
#'
#' Minimal server module; triggers client-side initialisation of custom tabs.
#' Extend this to wire your outputs (tables, downloads, etc.).
#'
#' @param id Module id.
#' @return Invisibly initialises tab behaviour (side effects).
#'
#' @examples
#' # server <- function(input, output, session) {
#' #   mod_govuk_data_vis_card_server("jobs")
#' # }
mod_govuk_data_vis_card_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Ensure the tab JS binds after render passes
    session$onFlushed(function() {
      session$sendCustomMessage("ukhsa-tabs-init", list())
    }, once = FALSE)
  })
}

# DBT TIME SERIES PLOT — ONS-styled with chart_type ("line" | "stacked_area")

# =============================================================================
# Registry utilities (dispatcher)
# =============================================================================
.dbt_chart_builders <- new.env(parent = emptyenv())

dbt_register_chart <- function(name, fn) {
  stopifnot(is.character(name), length(name) == 1, is.function(fn))
  assign(name, fn, envir = .dbt_chart_builders)
}

dbt_get_chart_builder <- function(name) {
  fn <- get0(name, envir = .dbt_chart_builders, inherits = FALSE)
  if (is.null(fn)) stop("Unsupported chart_type: '", name,
                        "'. Register a builder via dbt_register_chart().")
  fn
}

# =============================================================================
# Small internal helpers used by builders
# =============================================================================

# Period utilities (for bars)
.dbt_period_floor <- function(d, interval) {
  y <- as.integer(format(d, "%Y")); m <- as.integer(format(d, "%m"))
  if (interval == "daily") return(d)
  if (interval == "weekly") return(d - (as.integer(format(d, "%u")) - 1L))  # ISO Monday
  if (interval == "monthly") return(as.Date(format(d, "%Y-%m-01")))
  if (interval == "quarterly") {
    qstart_m <- c(1,4,7,10)[ ((m - 1) %/% 3) + 1L ]
    return(as.Date(sprintf("%04d-%02d-01", y, qstart_m)))
  }
  if (interval == "annually") return(as.Date(sprintf("%04d-01-01", y)))
  if (interval == "5year")   return(as.Date(sprintf("%04d-01-01", y - (y %% 5))))
  if (interval == "decade")  return(as.Date(sprintf("%04d-01-01", y - (y %% 10))))
  d
}
.dbt_by_string <- function(interval) switch(
  interval,
  daily = "1 day", weekly = "7 days", monthly = "1 month",
  quarterly = "3 months", annually = "1 year",
  `5year` = "5 years", decade = "10 years", "1 month"
)

# =============================================================================
# Chart builders
#   - Return list(plt, indices_for_group, legend_flags, range_hint = NULL)
#   - Do NOT set theme/layout/axes/modebar; the main function owns those.
# =============================================================================

# ---- LINE (with optional df2 overlay) ----
dbt_build_line <- function(
  plt, df1, df2,
  groups, col_vec,
  line_width, connect_gaps,
  dataset1_name, dataset2_name,
  dataset1_group, dataset2_group,
  line_dash1, line_dash2,
  fmt_financial3, y_prefix, y_suffix
) {
  indices_for_group <- setNames(vector("list", length(groups)), groups)
  trace_index <- -1L
  set_title_1 <- FALSE
  set_title_2 <- FALSE

  for (g in groups) {
    col_g <- unname(col_vec[[g]])
    d1g <- dplyr::filter(df1, .data$group == g)
    if (nrow(d1g) > 0) {
      indices_for_group[[g]] <- c(indices_for_group[[g]], trace_index + 1L)
      plt <- plotly::add_trace(
        plt, data = d1g,
        x = ~time_period, y = ~value, text = ~.__hover_y__.,
        type = "scatter", mode = "lines",
        connectgaps = connect_gaps,
        name = paste0(as.character(g), " — ", dataset1_name),
        line = list(width = line_width, color = col_g, dash = line_dash1),
        hovertemplate = paste0(as.character(g), ": %{text}<extra></extra>"),
        legendgroup = dataset1_group,
        legendgrouptitle = if (!set_title_1) list(text = dataset1_group) else NULL,
        showlegend = TRUE
      )
      trace_index <- trace_index + 1L
      set_title_1 <- TRUE

      if (!is.null(df2)) {
        d2g <- dplyr::filter(df2, .data$group == g)
        if (nrow(d2g) > 0) {
          d2g$.__hover_y__. <- vapply(d2g$value, fmt_financial3, "",
                                      prefix = y_prefix, suffix = y_suffix)
          indices_for_group[[g]] <- c(indices_for_group[[g]], trace_index + 1L)
          plt <- plotly::add_trace(
            plt, data = d2g,
            x = ~time_period, y = ~value, text = ~.__hover_y__.,
            type = "scatter", mode = "lines",
            connectgaps = connect_gaps,
            name = paste0(as.character(g), " — ", dataset2_name),
            line = list(width = line_width, color = col_g, dash = line_dash2),
            hovertemplate = paste0(as.character(g), ": %{text}<extra></extra>"),
            legendgroup = dataset2_group,
            legendgrouptitle = if (!set_title_2) list(text = dataset2_group) else NULL,
            showlegend = TRUE
          )
          trace_index <- trace_index + 1L
          set_title_2 <- TRUE
        }
      }
    }
  }

  list(
    plt = plt,
    indices_for_group = indices_for_group,
    legend_flags = list(set_title_1 = set_title_1, set_title_2 = set_title_2),
    range_hint = NULL
  )
}

# ---- STACKED AREA ----
dbt_build_stacked_area <- function(
  plt, df1, groups, col_vec, dataset1_group
) {
  indices_for_group <- setNames(vector("list", length(groups)), groups)
  trace_index <- -1L
  set_title_1 <- FALSE
  is_first <- TRUE

  for (g in groups) {
    col_g <- unname(col_vec[[g]])
    d1g <- dplyr::filter(df1, .data$group == g)
    if (nrow(d1g) > 0) {
      indices_for_group[[g]] <- c(indices_for_group[[g]], trace_index + 1L)
      fill_mode <- if (is_first) "tozeroy" else "tonexty"
      is_first <- FALSE
      plt <- plotly::add_trace(
        plt, data = d1g,
        x = ~time_period, y = ~value, text = ~.__hover_y__.,
        type = "scatter", mode = "lines",
        stackgroup = "one",
        fill = fill_mode,
        fillcolor = col_g,
        line = list(width = 0.5, color = col_g),
        name = as.character(g),
        hovertemplate = paste0(as.character(g), ": %{text}<extra></extra>"),
        legendgroup = dataset1_group,
        legendgrouptitle = if (!set_title_1) list(text = dataset1_group) else NULL,
        showlegend = TRUE
      )
      trace_index <- trace_index + 1L
      set_title_1 <- TRUE
    }
  }

  list(
    plt = plt,
    indices_for_group = indices_for_group,
    legend_flags = list(set_title_1 = set_title_1, set_title_2 = FALSE),
    range_hint = list(
      kind = "stacked",
      df_total = df1 |>
        dplyr::group_by(time_period) |>
        dplyr::summarise(
          total_pos = sum(pmax(value, 0), na.rm = TRUE),
          total_neg = sum(pmin(value, 0), na.rm = TRUE),
          .groups = "drop"
        )
    )
  )
}

# ---- STACKED BAR (periodise + aggregate inside) ----
dbt_build_stacked_bar <- function(
  plt, df1, groups, col_vec, dataset1_group,
  bar_interval = c("daily","weekly","monthly","quarterly","annually","5year","decade"),
  bar_agg = c("sum","mean","last"),
  fmt_financial3, y_prefix, y_suffix,
  # --- internal defaults for labels/totals (kept inside the helper) ---
  bar_show_segment_labels = TRUE,   # only inside; tiny segments omitted
  bar_label_min_prop      = 0.08,   # >= 8% of stack height to draw label
  bar_show_totals         = TRUE,   # bold total above each stack
  bar_total_label_color   = "#000000",
  bar_total_label_size    = 12L
) {
  bar_interval <- match.arg(bar_interval)
  bar_agg <- match.arg(bar_agg)

  # ---------- Periodise + aggregate ----------
  dfb <- df1
  dfb$period <- .dbt_period_floor(dfb$time_period, bar_interval)

  if (bar_agg == "sum") {
    dfb <- dfb |>
      dplyr::group_by(period, group) |>
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  } else if (bar_agg == "mean") {
    dfb <- dfb |>
      dplyr::group_by(period, group) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  } else { # "last"
    dfb <- dfb |>
      dplyr::arrange(period) |>
      dplyr::group_by(period, group) |>
      dplyr::slice_tail(n = 1) |>
      dplyr::ungroup()
  }

  # ---------- Complete the period grid ----------
  if (nrow(dfb) > 0) {
    by_str <- .dbt_by_string(bar_interval)
    pmin <- min(dfb$period, na.rm = TRUE)
    pmax <- max(dfb$period, na.rm = TRUE)
    seqp <- seq(from = pmin, to = pmax, by = by_str)
    full_grid <- tidyr::expand_grid(period = seqp, group = unique(dfb$group))
    dfb <- full_grid |>
      dplyr::left_join(dfb, by = c("period","group")) |>
      dplyr::mutate(value = tidyr::replace_na(value, 0)) |>
      dplyr::arrange(period, group)
  }

  # ---------- Per-period totals & shares ----------
  totals <- dfb |>
    dplyr::group_by(period) |>
    dplyr::summarise(
      total_pos  = sum(pmax(value, 0), na.rm = TRUE),
      total_neg  = sum(pmin(value, 0), na.rm = TRUE),
      total_sum  = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  dfb <- dfb |>
    dplyr::left_join(totals, by = "period")

  # Hover text for all segments
  dfb$.__hover_y__. <- vapply(dfb$value, fmt_financial3, "", prefix = y_prefix, suffix = y_suffix)

  # Segment labels shown *only* inside if segment ≥ threshold share of stack
  if (isTRUE(bar_show_segment_labels)) {
    share <- ifelse(
      dfb$value >= 0,
      ifelse(dfb$total_pos > 0, dfb$value / pmax(dfb$total_pos, .Machine$double.eps), 0),
      ifelse(dfb$total_neg < 0, abs(dfb$value) / pmax(abs(dfb$total_neg), .Machine$double.eps), 0)
    )
    dfb$.__seg_text__. <- ifelse(share >= bar_label_min_prop, dfb$.__hover_y__., NA_character_)
  } else {
    dfb$.__seg_text__. <- NA_character_
  }

  # ---------- Add stacked bar traces ----------
  indices_for_group <- setNames(vector("list", length(groups)), groups)
  trace_index <- -1L
  set_title_1 <- FALSE

  for (g in groups) {
    col_g <- unname(col_vec[[g]])
    dbg <- dplyr::filter(dfb, .data$group == g)
    if (nrow(dbg) > 0) {
      indices_for_group[[g]] <- c(indices_for_group[[g]], trace_index + 1L)
      plt <- plotly::add_bars(
        plt,
        data = dbg,
        x = ~period, y = ~value,
        # keep 'text' for hover; use 'texttemplate' for visible callouts
        text = ~.__hover_y__.,
        texttemplate = ~.__seg_text__.,
        textposition = "inside",
        insidetextanchor = "middle",
        name = as.character(g),
        marker = list(color = col_g),
        hovertemplate = paste0(as.character(g), ": %{text}<extra></extra>"),
        legendgroup = dataset1_group,
        legendgrouptitle = if (!set_title_1) list(text = dataset1_group) else NULL,
        showlegend = TRUE
      )
      trace_index <- trace_index + 1L
      set_title_1 <- TRUE
    }
  }

  plt <- plotly::layout(plt, barmode = "stack")

  # ---------- Bold totals above each stack ----------
  extra_annotations <- list()
  if (isTRUE(bar_show_totals) && nrow(totals) > 0) {
    for (i in seq_len(nrow(totals))) {
      # top of stack (prefer positive; if only negatives, use negative top)
      top_y <- if (totals$total_pos[i] != 0) totals$total_pos[i] else totals$total_neg[i]
      if (!is.finite(top_y)) top_y <- 0
      if (top_y == 0 && totals$total_sum[i] == 0) next

      total_lbl <- fmt_financial3(totals$total_sum[i], prefix = y_prefix, suffix = y_suffix)
      yanchor <- if (top_y >= 0) "bottom" else "top"
      yshift  <- if (top_y >= 0) 2 else -2

      extra_annotations[[length(extra_annotations) + 1L]] <- list(
        x = totals$period[i], xref = "x",
        y = top_y,            yref = "y",
        xanchor = "center",
        yanchor = yanchor,
        text = paste0("<b>", total_lbl, "</b>"),
        showarrow = FALSE,
        align = "center",
        yshift = yshift,
        font = list(color = bar_total_label_color, size = bar_total_label_size),
        borderpad = 0
      )
    }
  }

  list(
    plt = plt,
    indices_for_group = indices_for_group,
    legend_flags = list(set_title_1 = set_title_1, set_title_2 = FALSE),
    range_hint = list(
      kind = "stacked",
      df_total = totals |>
        dplyr::transmute(
          time_period = period,
          total_pos = total_pos,
          total_neg = total_neg
        )
    ),
    extra_annotations = extra_annotations
  )
}

# ---- Optional future builders (skeletons) ----

# Unstacked AREA (fill to zero, no df2 overlay)
dbt_build_area <- function(
  plt, df1, groups, col_vec, dataset1_group
) {
  indices_for_group <- setNames(vector("list", length(groups)), groups)
  trace_index <- -1L
  set_title_1 <- FALSE

  for (g in groups) {
    col_g <- unname(col_vec[[g]])
    d1g <- dplyr::filter(df1, .data$group == g)
    if (nrow(d1g) > 0) {
      indices_for_group[[g]] <- c(indices_for_group[[g]], trace_index + 1L)
      plt <- plotly::add_trace(
        plt, data = d1g,
        x = ~time_period, y = ~value, text = ~.__hover_y__.,
        type = "scatter", mode = "lines",
        fill = "tozeroy",
        line = list(width = 0, color = col_g),
        fillcolor = col_g,
        name = as.character(g),
        hovertemplate = paste0(as.character(g), ": %{text}<extra></extra>"),
        legendgroup = dataset1_group,
        legendgrouptitle = if (!set_title_1) list(text = dataset1_group) else NULL,
        showlegend = TRUE
      )
      trace_index <- trace_index + 1L
      set_title_1 <- TRUE
    }
  }

  list(
    plt = plt,
    indices_for_group = indices_for_group,
    legend_flags = list(set_title_1 = set_title_1, set_title_2 = FALSE),
    range_hint = NULL
  )
}

# HEATMAP (expects df already in long form: x=time_period, group=row, value -> z)
# This is a minimal skeleton; adapt to your matrix pivot if needed.
dbt_build_heatmap <- function(
  plt, df1, groups, col_vec, colorscale = "Viridis"
) {
  # A simple pivot to wide: rows = group, cols = time_period, values = value
  if (!all(c("time_period","group","value") %in% names(df1))) {
    stop("heatmap builder expects columns: time_period, group, value")
  }
  # Ensure time axis sorted
  df1 <- dplyr::arrange(df1, time_period, group)
  # Build z-matrix (group x time)
  w <- tidyr::pivot_wider(df1, names_from = time_period, values_from = value)
  rowlabs <- w$group
  z <- as.matrix(w[setdiff(names(w), "group")])
  # Add heatmap trace
  plt <- plotly::add_heatmap(
    plt,
    z = z,
    x = as.Date(colnames(z)),
    y = as.character(rownamelabs <- rowlabs),
    colorscale = colorscale,
    showscale = TRUE
  )
  # No per-group indices in a single heatmap trace, but keep shape for palette menu
  list(
    plt = plt,
    indices_for_group = list(`heatmap` = 0L),
    legend_flags = list(set_title_1 = FALSE, set_title_2 = FALSE),
    range_hint = NULL
  )
}

# =============================================================================
# Register supported chart types (current)
# =============================================================================
dbt_register_chart("line",         dbt_build_line)
dbt_register_chart("stacked_area", dbt_build_stacked_area)
dbt_register_chart("stacked_bar",  dbt_build_stacked_bar)

# (Future) You can enable once ready:
# dbt_register_chart("area",    dbt_build_area)
# dbt_register_chart("heatmap", dbt_build_heatmap)


# =============================================================================
# Main function: dbt_ts_plot (dispatcher-based)
# =============================================================================
dbt_ts_plot <- function(
  df,
  df2 = NULL,
  x_title = "Time period (YYYY‑MM)",
  y_title = "Value",
  palette = dbt_palettes$gaf,
  group_order = NULL,
  initial_legend_mode = c("hidden","overlay","export"),
  y_zero = TRUE,
  y_pad = 0.05,
  line_width = 3,
  # column mapping for primary dataset
  value_col = "value",
  group_col = "sector",
  time_col  = "time_period",
  # column mapping for second dataset (defaults to primary mappings if NULL)
  value_col2 = NULL,
  group_col2 = NULL,
  time_col2  = NULL,
  # legend labels & line dashes per dataset
  dataset1_name  = "Solid",
  dataset2_name  = "Dashed",
  line_dash1     = "solid",
  line_dash2     = "dash",
  # legend grouping titles (shown in legend)
  dataset1_group = "Dataset 1",
  dataset2_group = "Dataset 2",
  # reference lines (vertical on time axis, horizontal on value axis)
  vlines       = NULL,
  vline_labels = NULL,
  vline_color  = "rgba(0,0,0,0.3)", # ONS light grey for vertical lines
  vline_width  = 1.5,
  vline_dash   = "dot",
  hlines       = NULL,
  hline_labels = NULL,
  hline_color  = "#444444",
  hline_width  = 1.5,
  hline_dash   = "dot",
  # Y-axis label wrapping controls
  y_title_wrap       = TRUE,
  y_title_wrap_width = 15L,
  # chart type toggle (keep current options for backward compatibility)
  chart_type = c("line","stacked_area","stacked_bar"),
  # ---- Bar options ----
  bar_interval = c("daily","weekly","monthly","quarterly","annually","5year","decade"),
  bar_agg      = c("sum","mean","last"),
  # ---- Formatting controls ----
  y_tickformat = ",.0f",
  y_prefix = "",
  y_suffix = "",
  # Gap handling for lines
  connect_gaps = FALSE
) {
  initial_legend_mode <- match.arg(initial_legend_mode)
  chart_type <- match.arg(chart_type)
  bar_interval <- match.arg(bar_interval)
  bar_agg <- match.arg(bar_agg)

  # --- ONS/DBT style tokens ---
  ons_text_grey   <- "rgba(0,0,0,0.45)"   # axis titles, x ticks, legend, vline labels
  ons_axis_grey   <- "rgba(0,0,0,0.30)"   # axis lines and tick marks
  black           <- "#000000"            # y tick values
  axis_title_size <- 11

  # --- vline label overlap controls ---
  min_gap_days  <- 120L
  stack_step_px <- 16L
  
  # --- Reference label defaults (applied to **hline** labels only) ---
  REF_LABEL_WRAP_WIDTH  <- 16L
  REF_LABEL_BG          <- "rgba(255,255,255,0.85)"
  REF_LABEL_BORDERCOLOR <- "rgba(0,0,0,0.1)"
  REF_LABEL_BORDERWIDTH <- 0L
  REF_LABEL_BORDERPAD   <- 1L

  # --- Fixed right padding (in pixels) for hline labels & docked-right legend ---
  RIGHT_PAD_PX <- 16L
  LABEL_INNER_OFFSET_PX <- 6L  # xshift used to nudge label slightly into margin

  shapes <- list()
  annots <- list()
  extra_right_margin_px <- 0L   # will grow if any hline label exists

  stopifnot(is.data.frame(df))
  stopifnot(all(c(value_col, group_col, time_col) %in% names(df)))
  if (!is.null(df2)) stopifnot(is.data.frame(df2))

  # --- Resolve mappings for df2 (fallback to primary if not provided) ---
  if (!is.null(df2)) {
    value_col2 <- if (is.null(value_col2)) value_col else value_col2
    group_col2 <- if (is.null(group_col2)) group_col else group_col2
    time_col2  <- if (is.null(time_col2))  time_col  else time_col2
    if (!all(c(value_col2, group_col2, time_col2) %in% names(df2))) {
      stop("df2 is missing one or more of the mapped columns: ",
           paste(c(value_col2, group_col2, time_col2), collapse = ", "))
    }
  }

  # --- Standardize primary data ---
  df1 <- dplyr::rename(
    df,
    value       = !!rlang::sym(value_col),
    group       = !!rlang::sym(group_col),
    time_period = !!rlang::sym(time_col)
  )
  if (is.factor(df1$time_period)) df1$time_period <- as.character(df1$time_period)
  if (!inherits(df1$time_period, "Date")) df1$time_period <- as.Date(df1$time_period)
  if (!is.numeric(df1$value)) df1$value <- suppressWarnings(as.numeric(df1$value))
  if (!is.null(group_order)) df1$group <- factor(df1$group, levels = group_order)

  # --- Drop NA early ---
  df1 <- df1 |>
    dplyr::filter(!is.na(time_period), !is.na(value), !is.na(group))

  # --- Aggregate duplicates (sum within same time_period, group) ---
  df1 <- df1 |>
    dplyr::group_by(time_period, group) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(time_period, group)

  # --- Standardize comparison data ---
  df2_std <- NULL
  if (!is.null(df2)) {
    df2_std <- dplyr::rename(
      df2,
      value       = !!rlang::sym(value_col2),
      group       = !!rlang::sym(group_col2),
      time_period = !!rlang::sym(time_col2)
    )
    if (is.factor(df2_std$time_period)) df2_std$time_period <- as.character(df2_std$time_period)
    if (!inherits(df2_std$time_period, "Date")) df2_std$time_period <- as.Date(df2_std$time_period)
    if (!is.numeric(df2_std$value)) df2_std$value <- suppressWarnings(as.numeric(df2_std$value))
    if (!is.null(group_order)) df2_std$group <- factor(df2_std$group, levels = group_order)

    df2_std <- df2_std |>
      dplyr::filter(!is.na(time_period), !is.na(value), !is.na(group)) |>
      dplyr::group_by(time_period, group) |>
      dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(time_period, group)
  }

  # --- Palette handling (include "ONS Full") ---
  palette_options <- list(
    "gaf"                 = unname(dbt_palettes$gaf),
    "dbt_default"         = unname(dbt_palettes$dbt$default),
    "dbt_extended"        = unname(dbt_palettes$dbt$extended),
    "dbt_reverse"         = unname(dbt_palettes$dbt$reverse),
    "blues"               = unname(dbt_palettes$blues),
    "corporate"           = unname(dbt_palettes$corporate),
    "sequential_default"  = unname(dbt_palettes$sequential$default),
    "sequential_blue"     = unname(dbt_palettes$sequential$blue),
    "ONS Full"            = c("#f39431", "#206095", "#a8bd3a", "#871a5b", "#f66068",
                              "#05341a", "#27a0cc", "#003c57", "#22d0b6", "#746cb1")
  )
  if (is.character(palette) && length(palette) == 1 && palette %in% names(palette_options)) {
    palette_vec <- palette_options[[palette]]
  } else {
    palette_vec <- unname(palette)
  }

  # --- Group list (defer order if stacked charts & no explicit group_order) ---
  base_groups <- unique(df1$group) |> as.character()
  groups <- base_groups
  n_groups <- length(groups)
  col_vec <- rep(palette_vec, length.out = n_groups); names(col_vec) <- groups

  # --- Financial formatting for hover (3 sig figs for m/bn, thousands whole, sub-1k 1 dp) ---
  .trim_zeros <- function(s) sub("\\.?0+$", "", s)
  .sig3 <- function(x) .trim_zeros(formatC(signif(x, 3), format = "fg", digits = 3))
  fmt_financial3 <- function(x, prefix = "", suffix = "") {
    if (is.na(x)) return(NA_character_)
    ax <- abs(x); sgn <- if (x < 0) "-" else ""
    if (ax >= 1e9)      paste0(sgn, prefix, .sig3(ax/1e9), "bn", suffix)
    else if (ax >= 1e6) paste0(sgn, prefix, .sig3(ax/1e6), "m",  suffix)
    else if (ax >= 1e3) paste0(sgn, prefix, formatC(ax, big.mark = ",", format = "f", digits = 0), suffix)
    else                paste0(sgn, prefix, formatC(ax, big.mark = ",", format = "f", digits = 1), suffix)
  }

  # --- Precompute hover text for df1
  df_area <- df1
  df_area$.__hover_y__. <- vapply(df_area$value, fmt_financial3, "", prefix = y_prefix, suffix = y_suffix)

  # --- Decide groups and their order
  .latest_by_group <- function(dd) {
    if (nrow(dd) == 0) return(setNames(numeric(0), character(0)))
    last_t <- max(dd$time_period, na.rm = TRUE)
    vals <- dd |>
      dplyr::filter(time_period == last_t) |>
      dplyr::group_by(group) |>
      dplyr::summarise(v = sum(value, na.rm = TRUE), .groups = "drop")
    setNames(vals$v, as.character(vals$group))
  }

  if (!is.null(group_order)) {
    groups <- unique(c(as.character(group_order), setdiff(base_groups, as.character(group_order))))
  } else if (chart_type %in% c("stacked_area","stacked_bar")) {
    latest_vals <- if (chart_type == "stacked_area") {
      .latest_by_group(df_area)
    } else {
      .latest_by_group(df_area)
    }
    latest_vals <- latest_vals[intersect(names(latest_vals), base_groups)]
    missing <- setdiff(base_groups, names(latest_vals))
    if (length(missing)) latest_vals <- c(latest_vals, setNames(rep(0, length(missing)), missing))
    groups <- names(sort(latest_vals, decreasing = TRUE))
  } else {
    groups <- base_groups
  }

  # Colors aligned to ordered groups
  n_groups <- length(groups)
  col_vec <- rep(palette_vec, length.out = n_groups); names(col_vec) <- groups

  # --- Build base plotly and dispatch to builder ---
  plt <- plotly::plot_ly()
  builder <- dbt_get_chart_builder(chart_type)

  build_res <- switch(
    chart_type,
    line = builder(
      plt = plt,
      df1 = df_area,
      df2 = df2_std,
      groups = groups,
      col_vec = col_vec,
      line_width = line_width,
      connect_gaps = connect_gaps,
      dataset1_name = dataset1_name,
      dataset2_name = dataset2_name,
      dataset1_group = dataset1_group,
      dataset2_group = dataset2_group,
      line_dash1 = line_dash1,
      line_dash2 = line_dash2,
      fmt_financial3 = fmt_financial3,
      y_prefix = y_prefix, y_suffix = y_suffix
    ),
    stacked_area = builder(
      plt = plt,
      df1 = df_area,
      groups = groups,
      col_vec = col_vec,
      dataset1_group = dataset1_group
    ),
    stacked_bar = builder(
      plt = plt,
      df1 = df_area,
      groups = groups,
      col_vec = col_vec,
      dataset1_group = dataset1_group,
      bar_interval = bar_interval,
      bar_agg = bar_agg,
      fmt_financial3 = fmt_financial3, y_prefix = y_prefix, y_suffix = y_suffix
    ),
    stop("Unhandled chart_type: ", chart_type)
  )

  plt <- build_res$plt
  indices_for_group <- build_res$indices_for_group

  # --- Apply DBT theme ---
  plt <- theme_dbt_plotly(plt)

  # --- y-axis base config ---
  yaxis_cfg <- list(
    title     = list(text = NULL),
    showline  = FALSE,
    ticks     = "outside",
    tickcolor = ons_axis_grey,
    ticklen   = 4,
    tickfont  = list(color = black),
    zeroline  = FALSE,
    tickformat = y_tickformat
  )

  # --- Robust y range logic (line/stacked_area/stacked_bar) ---
  if (chart_type == "stacked_area") {
    totals <- df_area |>
      dplyr::group_by(time_period) |>
      dplyr::summarise(
        total_pos = sum(pmax(value, 0), na.rm = TRUE),
        total_neg = sum(pmin(value, 0), na.rm = TRUE),
        .groups = "drop"
      )
    top <- suppressWarnings(max(totals$total_pos, na.rm = TRUE))
    bot <- if (isTRUE(y_zero)) 0 else suppressWarnings(min(totals$total_neg, 0, na.rm = TRUE))
    if (!is.finite(top)) top <- 0
    if (!is.finite(bot)) bot <- 0
    span <- max(1e-9, top - bot)
    yaxis_cfg$range <- c(bot - y_pad * span, top + y_pad * span)

  } else if (chart_type == "stacked_bar") {
    totals <- build_res$range_hint$df_total
    top <- suppressWarnings(max(totals$total_pos, na.rm = TRUE))
    bot <- if (isTRUE(y_zero)) 0 else suppressWarnings(min(totals$total_neg, 0, na.rm = TRUE))
    if (!is.finite(top)) top <- 0
    if (!is.finite(bot)) bot <- 0
    span <- max(1e-9, top - bot)
    yaxis_cfg$range <- c(bot - y_pad * span, top + y_pad * span)

  } else {
    y_vals <- df_area$value
    if (!is.null(df2_std)) y_vals <- c(y_vals, df2_std$value)
    ymin <- suppressWarnings(min(y_vals, na.rm = TRUE))
    ymax <- suppressWarnings(max(y_vals, na.rm = TRUE))
    if (!is.finite(ymin)) ymin <- 0
    if (!is.finite(ymax)) ymax <- 0
    if (isTRUE(y_zero)) {
      lo <- min(0, ymin)
      hi <- max(0, ymax)
      span <- max(1e-9, hi - lo)
      yaxis_cfg$range <- c(lo, hi + y_pad * span)
    } else {
      span <- max(1e-9, ymax - ymin)
      yaxis_cfg$range <- c(ymin - y_pad * span, ymax + y_pad * span)
    }
  }

  # --- x-axis (zoom-friendly date tickformatstops) ---
  xaxis_cfg <- list(
    type = "date",
    title = list(text = x_title, font = list(color = ons_text_grey, size = axis_title_size)),
    showline = TRUE, linecolor = ons_axis_grey, linewidth = 1,
    ticks = "outside", tickcolor = ons_axis_grey, ticklen = 4,
    tickfont = list(color = ons_text_grey),
    zeroline = FALSE,
    tickformatstops = list(
      list(dtickrange = list(NULL, 86400000), value = "%Y-%m-%d"), # < 1 day
      list(dtickrange = list(86400000, "M1"), value = "%d %b %Y"), # day to < 1 month
      list(dtickrange = list("M1", "M12"),   value = "%b %Y"),     # month to < 1 year
      list(dtickrange = list("M12", NULL),   value = "%Y")         # >= 1 year
    )
  )

  plt <- plt |>
    plotly::layout(
      title     = list(text = NULL),
      hovermode = "x unified",
      xaxis = c(
        xaxis_cfg,
        list(
          showspikes = TRUE,
          spikemode = "lines",
          spikesnap = "cursor",
          spikedash = "dot",
          spikethickness = 1,
          spikecolor = "rgba(0,0,0,0.5)"
        )
      ),
      yaxis = yaxis_cfg,
      hoverlabel = list(
        bgcolor = "rgba(240,240,240,0.95)",   # ONS light grey box
        font = list(color = "black"),
        bordercolor = "rgba(0,0,0,0.1)"
      ),
      legend = list(groupclick = "toggleitem", font = list(color = ons_text_grey)),
      separators = ",."
    )

  # --- Legend position modes ---
  overlay_layout <- list(
    "legend.orientation" = "v",
    "legend.x" = 0.98, "legend.y" = 0.92,
    "legend.xanchor" = "right", "legend.yanchor" = "top",
    "legend.bgcolor" = "rgba(255,255,255,0.85)",
    "legend.bordercolor" = "rgba(0,0,0,0.2)",
    "legend.borderwidth" = 1
  )
  export_right_layout <- list(
    "legend.orientation" = "v",
    "legend.x" = 1.02, "legend.xanchor" = "left",
    "legend.y" = 1.0,  "legend.yanchor" = "top"
  )
  if (initial_legend_mode == "hidden") {
    plt <- plt |> plotly::layout(showlegend = FALSE)
  } else if (initial_legend_mode == "overlay") {
    plt <- plt |> plotly::layout(c(list(showlegend = TRUE), overlay_layout))
  } else if (initial_legend_mode == "export") {
    plt <- plt |> plotly::layout(c(list(showlegend = TRUE), export_right_layout))
  }

  # --- Helpers for reference lines & annotation text wrapping ---
  parse_dates <- function(x) {
    if (inherits(x, "Date")) return(x)
    out <- try(as.Date(x), silent = TRUE)
    if (inherits(out, "try-error") || any(is.na(out))) {
      stop("Unable to parse one or more 'vlines' dates; please supply as Date or 'YYYY-MM-DD'.")
    }
    out
  }

  wrap_text <- function(txt, width = 15L) {
    if (is.null(txt) || !nzchar(txt)) return(txt)
    if (grepl("\n", txt, fixed = TRUE) || grepl("<br>", txt, fixed = TRUE)) {
      return(gsub("\n", "<br>", txt, fixed = TRUE))
    }
    words <- strsplit(txt, "\\s+")[[1]]
    lines <- character(0)
    cur <- ""
    for (w in words) {
      add <- if (nzchar(cur)) paste(cur, w) else w
      if (nchar(add) <= width) {
        cur <- add
      } else {
        if (nzchar(cur)) lines <- c(lines, cur)
        cur <- w
      }
    }
    if (nzchar(cur)) lines <- c(lines, cur)
    paste(lines, collapse = "<br>")
  }

  # --- Vertical reference lines & labels (stacked to avoid overlap) ---
  if (!is.null(vlines) && length(vlines) > 0) {
    vx <- parse_dates(vlines)
    vlab <- vline_labels; if (is.null(vlab)) vlab <- rep("", length(vx))
    if (length(vlab) == 1L && length(vx) > 1L) vlab <- rep(vlab, length(vx))
    if (length(vlab) != length(vx)) stop("'vline_labels' must be length 1 or match length of 'vlines'.")
    vx_num <- as.numeric(vx)
    ord <- order(vx_num)
    levels <- integer(length(vx))
    if (length(vx) >= 2L) {
      levels[ord[1]] <- 0L
      for (k in 2:length(vx)) {
        levels[ord[k]] <- if ((vx_num[ord[k]] - vx_num[ord[k-1]]) < min_gap_days) {
          levels[ord[k-1]] + 1L
        } else 0L
      }
    }
    for (i in seq_along(vx)) {
      shapes[[length(shapes) + 1L]] <- list(
        type = "line", xref = "x", yref = "paper",
        x0 = vx[i], x1 = vx[i], y0 = 0, y1 = 1,
        line = list(color = vline_color, width = vline_width, dash = vline_dash),
        layer = "above"
      )
      if (!is.null(vlab[i]) && nzchar(vlab[i])) {
        yshift_px <- - (levels[i] * stack_step_px)
        annots[[length(annots) + 1L]] <- list(
          x = vx[i], xref = "x",
          y = 1,     yref = "paper",
          xanchor = "right",
          yanchor = "top",
          text = vlab[i],
          align = "right",
          showarrow = FALSE,
          yshift = yshift_px,
          font = list(color = ons_text_grey, size = axis_title_size)
        )
      }
    }
  }

  # --- Horizontal reference lines (with robust right margin & legend shift) ---
  if (!is.null(hlines) && length(hlines) > 0) {
    hy   <- as.numeric(hlines)
    hlab <- hline_labels; if (is.null(hlab)) hlab <- rep("", length(hy))
    if (length(hlab) == 1L && length(hy) > 1L) hlab <- rep(hlab, length(hy))
    if (length(hlab) != length(hy)) stop("'hline_labels' must be length 1 or match length of 'hlines'.")

    # Measure max required width for any hline label (in px) using wrapped text
    if (any(nzchar(hlab))) {
      char_px <- ceiling(axis_title_size * 0.62) # heuristic char width in px for the current font size
      for (i in seq_along(hy)) {
        if (!nzchar(hlab[i])) next
        htext_tmp <- wrap_text(hlab[i], width = REF_LABEL_WRAP_WIDTH)
        lines <- strsplit(htext_tmp, "<br>", fixed = TRUE)[[1]]
        max_chars <- max(1L, max(nchar(lines, type = "width")))
        # box width: text + small inner padding + border paddings
        label_w_px <- (max_chars * char_px) + 10L + (2L * as.integer(REF_LABEL_BORDERPAD))
        # allow for the xshift we apply to nudge into the margin
        extra_right_margin_px <- max(extra_right_margin_px, label_w_px + LABEL_INNER_OFFSET_PX)
      }
    }

    for (i in seq_along(hy)) {
      # draw the line
      shapes[[length(shapes) + 1L]] <- list(
        type = "line", xref = "paper", yref = "y",
        x0 = 0, x1 = 1, y0 = hy[i], y1 = hy[i],
        line = list(color = hline_color, width = hline_width, dash = hline_dash),
        layer = "above"
      )

      # label: pinned to right edge inside plot area, offset a bit into the margin
      if (!is.null(hlab[i]) && nzchar(hlab[i])) {
        htext <- wrap_text(hlab[i], width = REF_LABEL_WRAP_WIDTH)
        annots[[length(annots) + 1L]] <- list(
          x = 1, xref = "paper", y = hy[i], yref = "y",
          xanchor = "left", yanchor = "middle",
          xshift = LABEL_INNER_OFFSET_PX,
          text = htext, showarrow = FALSE, align = "left",
          font = list(color = ons_text_grey, size = axis_title_size),
          bgcolor = REF_LABEL_BG,
          bordercolor = REF_LABEL_BORDERCOLOR,
          borderwidth = REF_LABEL_BORDERWIDTH,
          borderpad = REF_LABEL_BORDERPAD,
          cliponaxis = FALSE
        )
      }
    }
  }

  # --- Y-axis title as top-left annotation (left of axis), wrapped ---
  if (!is.null(y_title) && nzchar(y_title)) {
    y_text <- if (grepl("\n|<br>", y_title)) {
      gsub("\\n", "<br>", y_title)
    } else if (isTRUE(y_title_wrap)) {
      wrap_text(y_title, width = y_title_wrap_width)
    } else {
      y_title
    }
    annots[[length(annots) + 1L]] <- list(
      x = -0.03, xref = "paper",
      y = 1.015, yref = "paper",
      xanchor = "left",
      yanchor = "bottom",
      text = y_text,
      showarrow = FALSE,
      align = "left",
      font = list(color = ons_text_grey, size = axis_title_size),
      borderpad = 0,
      cliponaxis = FALSE
    )
  }
  
  # --- Extra annotations coming from the chart builder (e.g., stacked-bar totals) ---
  if (!is.null(build_res$extra_annotations) && length(build_res$extra_annotations)) {
    annots <- c(annots, build_res$extra_annotations)
  }

  base_margin <- list(t = 60)
  if (extra_right_margin_px > 0L) {
    base_margin$r <- max(0L, extra_right_margin_px)
  }
  plt <- plotly::layout(
    plt,
    shapes = shapes,
    annotations = annots,
    margin = base_margin
  )

  # --- Keep a fixed RIGHT_PAD_PX gap between plot edge and right-docked legend (export posture)
  # Shift the legend by the **measured** label width (extra_right_margin_px) + small fixed pad.
  if (extra_right_margin_px > 0L) {
    plt <- htmlwidgets::onRender(plt, sprintf("
      function(el){
        var EXTRA_PX = %d;   // measured label width (px)
        var PAD_PX   = %d;   // small safety pad (px)
        var gd = document.getElementById(el.id);
        if(!gd) return;

        function placeLegend(){
          try{
            var fl = gd._fullLayout;
            if (!fl || !gd.layout.showlegend) return;
            var L = fl.legend;
            if (!L) return;

            // Only adjust when legend is docked-right (xanchor='left' and x >= 1)
            if (L.xanchor !== 'left' || (L.x || 0) < 1) return;

            var plotW = (fl._size && fl._size.w) ? fl._size.w : 0;
            if (plotW <= 0) return;

            var dx = (EXTRA_PX + PAD_PX) / plotW;  // convert pixels -> paper units
            var target = 1 + dx;
            if (Math.abs((L.x || 0) - target) > 1e-4) {
              Plotly.relayout(gd, {'legend.x': target});
            }
          } catch(e) {}
        }

        gd.on('plotly_afterplot', placeLegend);
        gd.on('plotly_relayout',  placeLegend);
        setTimeout(placeLegend, 0);
      }
    ", as.integer(extra_right_margin_px), as.integer(RIGHT_PAD_PX)))
  }

  # If the initial legend mode is 'export' and we needed right margin for hline labels,
  # a small initial nudge; the onRender code above will compute the exact final position.
  if (identical(initial_legend_mode, "export") && extra_right_margin_px > 0L) {
    plt <- plotly::layout(plt, legend = list(x = 1.06, xanchor = "left"))
  }

  # --- Palette dropdown (line / stacked_area / stacked_bar) ---
  all_indices <- unlist(indices_for_group, use.names = FALSE)
  if (length(all_indices) > 0) {
    palette_buttons <- lapply(names(palette_options), function(nm) {
      pal <- palette_options[[nm]]
      group_cols <- unname(rep(pal, length.out = n_groups))
      if (chart_type == "line") {
        cols_for_indices <- unlist(mapply(function(gc, idxs) rep(gc, length(idxs)),
                                          gc = group_cols, idxs = indices_for_group,
                                          SIMPLIFY = FALSE, USE.NAMES = FALSE))
        list(method = "restyle",
             args = list(list("line.color" = cols_for_indices), as.list(all_indices)),
             label = nm)
      } else if (chart_type == "stacked_area") {
        fill_for_indices <- unlist(mapply(function(gc, idxs) rep(gc, length(idxs)),
                                          gc = group_cols, idxs = indices_for_group,
                                          SIMPLIFY = FALSE, USE.NAMES = FALSE))
        line_cols_for_indices <- fill_for_indices
        list(method = "restyle",
             args = list(list("fillcolor" = fill_for_indices,
                              "line.color" = line_cols_for_indices),
                         as.list(all_indices)),
             label = nm)
      } else {
        cols_for_indices <- unlist(mapply(function(gc, idxs) rep(gc, length(idxs)),
                                          gc = group_cols, idxs = indices_for_group,
                                          SIMPLIFY = FALSE, USE.NAMES = FALSE))
        list(method = "restyle",
             args = list(list("marker.color" = cols_for_indices), as.list(all_indices)),
             label = nm)
      }
    })
    plt <- plt |>
      plotly::layout(
        updatemenus = list(
          list(
            type = "dropdown",
            direction = "down",
            x = 0, y = 1.18, xanchor = "left", yanchor = "top",
            buttons = palette_buttons,
            showactive = TRUE,
            bgcolor = "white",
            bordercolor = "rgba(0,0,0,0.2)",
            visible = FALSE
          )
        )
      )
  }

  # --- Modebar icons & handlers (legend/palette only) ---
  icon_overlay <- htmlwidgets::JS(
    "{ width:1000, height:1000, ascent:1000, descent:0,
       path: [
         'M140 290 L200 290 L200 370 L140 370 Z',
         'M260 290 L320 290 L320 370 L260 370 Z',
         'M380 290 L440 290 L440 370 L380 370 Z',
         'M500 290 L560 290 L560 370 L500 370 Z',
         'M620 290 L680 290 L680 370 L620 370 Z',
         'M740 290 L800 290 L800 370 L740 370 Z',
         'M140 460 L200 460 L200 540 L140 540 Z',
         'M260 460 L320 460 L320 540 L260 540 Z',
         'M380 460 L440 460 L440 540 L380 540 Z',
         'M500 460 L560 460 L560 540 L500 540 Z',
         'M620 460 L680 460 L680 540 L620 540 Z',
         'M740 460 L800 460 L800 540 L740 540 Z',
         'M140 630 L200 630 L200 710 L140 710 Z',
         'M260 630 L320 630 L320 710 L260 710 Z',
         'M380 630 L440 630 L440 710 L380 710 Z',
         'M500 630 L560 630 L560 710 L500 710 Z',
         'M620 630 L680 630 L680 710 L620 710 Z',
         'M740 630 L800 630 L800 710 L740 710 Z'
       ].join(' ')
     }"
  )
  icon_export <- htmlwidgets::JS(
    "{ width:1000, height:1000, ascent:1000, descent:0,
       path: [
         'M120 260 L150 260 L150 760 L120 760 Z',
         'M160 300 L560 300 L560 380 L160 380 Z',
         'M160 500 L760 500 L760 580 L160 580 Z',
         'M160 700 L920 700 L920 780 L160 780 Z'
       ].join(' ')
     }"
  )
  icon_drop <- htmlwidgets::JS(
    "{ width:1000, height:1000, ascent:1000, descent:0,
       path:
         'M500 937.5 ' +
         'C500 937.5 203.125 625 203.125 406.25 ' +
         'C203.125 242.1875 335.9375 109.375 500 109.375 ' +
         'C664.0625 109.375 796.875 242.1875 796.875 406.25 ' +
         'C796.875 625 500 937.5 500 937.5 Z'
     }"
  )
  overlay_js <- htmlwidgets::JS("
    function(gd){
      var isVisible = !!gd.layout.showlegend;
      if(!isVisible || gd._legendMode !== 'overlay'){
        Plotly.relayout(gd, {
          'showlegend': true,
          'legend.orientation': 'v',
          'legend.x': 0.98, 'legend.y': 0.92,
          'legend.xanchor': 'right', 'legend.yanchor': 'top',
          'legend.bgcolor': 'rgba(255,255,255,0.85)',
          'legend.bordercolor': 'rgba(0,0,0,0.2)',
          'legend.borderwidth': 1
        });
        gd._legendMode = 'overlay';
      } else {
        Plotly.relayout(gd, {'showlegend': false});
        gd._legendMode = 'hidden';
      }
    }
  ")
  export_js <- htmlwidgets::JS("
    function(gd){
      var isVisible = !!gd.layout.showlegend;
      if(isVisible && gd._legendMode === 'export'){
        Plotly.relayout(gd, {'showlegend': false});
        gd._legendMode = 'hidden';
        return;
      }
      Plotly.relayout(gd, {
        'showlegend': true,
        'legend.orientation': 'v',
        'legend.x': 1.06, 'legend.xanchor': 'left',
        'legend.y': 1.0,  'legend.yanchor': 'top',
        'legend.bgcolor': null, 'legend.bordercolor': null, 'legend.borderwidth': null
      });
      gd._legendMode = 'export';
    }
  ")
  palette_toggle_js <- htmlwidgets::JS("
    function(gd){
      try{
        var cur = (gd.layout.updatemenus && gd.layout.updatemenus[0] && gd.layout.updatemenus[0].visible) ? gd.layout.updatemenus[0].visible : false;
        var next = !(cur === true);
        var obj = {};
        obj['updatemenus[0].visible'] = next;
        Plotly.relayout(gd, obj);
      }catch(e){ console.warn('Unable to toggle palette dropdown:', e); }
    }
  ")

  plt <- plotly::config(
    plt,
    displaylogo = FALSE,
    modeBarButtonsToAdd = list(
      list(name = "Overlay legend", title = "Toggle overlay legend", click = overlay_js, icon = icon_overlay),
      list(name = "Legend right",   title = "Dock legend right",     click = export_js,  icon = icon_export),
      list(name = "Palette",        title = "Show/hide palette menu", click = palette_toggle_js, icon = icon_drop)
    )
  )

  # --- JS onRender: y-axis ticks follow the same financial logic as hover ---
  plt <- htmlwidgets::onRender(plt, "
    function(el, x){
      var gd = document.getElementById(el.id);
      if(!gd) return;

      function postprocess() {
        try{
          var nodes = el.querySelectorAll('.yaxislayer-above .ytick text');
          nodes.forEach(function(t){
            var s = t.textContent || '';
            if (s.indexOf('G') >= 0) t.textContent = s.replace('G', 'bn');
            else if (s.indexOf('M') >= 0) t.textContent = s.replace('M', 'm');
          });
        }catch(e){}
      }

      var applying = false;
      function applyTickFormat(){
        if(!gd || !gd._fullLayout || !gd._fullLayout.yaxis) return;
        var ya = gd._fullLayout.yaxis;
        var r = ya.range || [0, 1];
        var maxAbs = Math.max(Math.abs(r[0]), Math.abs(r[1]));
        var fmt;
        if (maxAbs < 1000)      fmt = ',.1f';
        else if (maxAbs < 1e6)  fmt = ',.0f';
        else                    fmt = '.3~s';
        if (ya.tickformat === fmt) { postprocess(); return; }
        if (applying) return;
        applying = true;
        Plotly.relayout(gd, {'yaxis.tickformat': fmt}).then(function(){
          applying = false;
          postprocess();
        }).catch(function(){ applying = false; });
      }

      gd.on('plotly_afterplot', applyTickFormat);
      gd.on('plotly_relayout',  applyTickFormat);
      applyTickFormat();
    }
  ")

  return(plt)
}


### Vertical and Horizontal Lines Controls -----

# --- UI ---
mod_annotation_line_ui <- function(
  id,
  title = "String + Date pairs",
  add_label = "+ Add",
  placeholder_label = "Enter label…",
  placeholder_date  = "YYYY-MM-DD",
  show_delete = TRUE,
  auto_mask_date = TRUE,
  type = c("date", "value"),
  gap_px = 8  # fixed horizontal gap in pixels
) {
  type <- match.arg(type)
  ns <- NS(id)

  right_placeholder <- if (type == "date") placeholder_date else "Enter numeric value…"

  tagList(
    # CSS (scoped by IDs to this module instance)
    tags$style(HTML(sprintf("
      /* Row behaves as a 3-column grid: 50%% | 40%% | auto */
      #%s .sdlm-grid {
        display: grid;
        grid-template-columns: 50%% 40%% auto;
        column-gap: %dpx;                   /* fixed gap you control */
        align-items: center;                /* baseline safety */
        margin-bottom: 6px;
        width: 100%%;
        min-width: 0;                       /* prevent overflow from long content */
      }

      /* Ensure inputs fill their grid cell and don't overflow */
      #%s .sdlm-grid .sdlm-cell > .form-group,
      #%s .sdlm-grid .sdlm-cell input,
      #%s .sdlm-grid .sdlm-cell .form-control {
        width: 100%%;
        min-width: 0;
      }

      /* Minimal extra CSS (Option B):
         - Make every cell a flex container to vertically center contents
         - Center the delete cell horizontally (last grid cell)
         - Normalize Bootstrap form-group margins so inputs don't inflate the row */
      #%s .sdlm-grid .sdlm-cell {
        display: flex;
        align-items: center;    /* vertical centering */
        min-width: 0;
      }
      #%s .sdlm-grid .sdlm-cell:last-child {
        justify-content: center;  /* center the ✕ horizontally */
      }
      #%s .sdlm-grid .form-group {
        margin-bottom: 0;         /* avoid extra vertical space */
      }

      /* Minimalist cross (no background hover; text-only emphasis) */
      #%s .sdlm-x {
        display: inline-flex;     /* predictable centering box */
        align-items: center;
        justify-content: center;
        color: #6c757d;
        font-weight: 600;
        text-decoration: none !important;
        padding: 0;
        margin: 0;
        width: 28px;              /* small square click target */
        height: 28px;
        border-radius: 4px;
        line-height: 1;           /* prevent baseline drift */
        cursor: pointer;
        user-select: none;
        border: 0;
        background: transparent;
      }
      
      #%s .sdlm-x:hover,
      #%s .sdlm-x:focus {
        color: #dc3545;           /* make the X pop */
        font-weight: 700;         /* stronger/bolder X on hover/focus */
        background: transparent;  /* ensure no background fill */
        outline: none;
        box-shadow: none;
      }


      /* Invalid glow */
      #%s .sdlm-invalid input {
        border-color: #d9534f !important;
        box-shadow: 0 0 0 0.18rem rgba(217,83,79,.25) !important;
      }

      /* Add button area */
      #%s .sdlm-add { margin-top: 6px; }
    ",
      ns("rows_container"), gap_px,  # grid + gap

      ns("rows_container"),          # ensure inputs fill cell width
      ns("rows_container"),
      ns("rows_container"),

      ns("rows_container"),          # .sdlm-cell flex centering
      ns("rows_container"),          # last child centered (delete cell)
      ns("rows_container"),          # normalize form-group margin

      ns("rows_container"),          # .sdlm-x base
      ns("rows_container"),          # .sdlm-x:hover
      ns("rows_container"),          # .sdlm-x:focus

      ns("rows_container"),          # invalid glow
      ns("root")                     # add button scope
    ))),

    # Scope all row styles to this module instance
    div(id = ns("root"),
  div(class = "form-group shiny-input-container",
    tags$label(
      id    = paste0(ns("chart_type"), "-label"),  # matches shinyWidgets pattern
      class = "control-label",
      `for` = ns("chart_type"),                    # associate with the input
      title
            )
          )
        ), 

      # Start empty: rows will be inserted here
      div(id = ns("rows_container")),

      # Add button
      div(
        actionButton(ns("add"), add_label, icon = icon("plus")),
        class = "sdlm-add"
      ),

      # Date auto-mask (only in date mode)
      if (auto_mask_date && type == "date") tags$script(HTML(sprintf("
        (function(){
          var prefix = '%s';
          document.addEventListener('input', function(e){
            if(!e.target || !e.target.id) return;
            if(e.target.id.startsWith(prefix + 'date_')){
              let v = e.target.value.replace(/[^0-9]/g,'').slice(0,8);
              if(v.length > 4) v = v.slice(0,4) + '-' + v.slice(4);
              if(v.length > 7) v = v.slice(0,7) + '-' + v.slice(7);
              e.target.value = v;
            }
          }, true);
        })();
      ", ns("")))
    )
  )
}


# --- SERVER ---
mod_annotation_line_server <- function(id, type = c("date", "value")) {
  type <- match.arg(type)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Start with NO rows
    max_idx   <- reactiveVal(0L)
    active_ix <- reactiveVal(integer(0))

    # Namespaced row and inputs
    id_row    <- function(i) paste0("#", ns(paste0("row_", i)))
    id_label  <- function(i) ns(paste0("label_",  i))
    id_date   <- function(i) ns(paste0("date_",   i))
    id_remove <- function(i) ns(paste0("remove_", i))

    # Add row (creates the first too)
    observeEvent(input$add, {
      i <- max_idx() + 1L
      insertUI(
        selector = paste0("#", ns("rows_container")),
        where = "beforeEnd",

        ui = tags$div(
          id = ns(paste0("row_", i)),
          class = "sdlm-grid",

          # 50% cell: label
          tags$div(class = "sdlm-cell",
            textInput(id_label(i), label = NULL, placeholder = "Enter label…")
          ),

          # 40% cell: date/value
          tags$div(class = "sdlm-cell",
            textInput(
              inputId = id_date(i),
              label   = NULL,
              placeholder = if (type == "date") "YYYY-MM-DD" else "Enter numeric value…"
            )
          ),

          # auto cell: minimalist cross (no outline)
          tags$div(class = "sdlm-cell",
            actionLink(id_remove(i), label = "✕", class = "sdlm-x", title = "Remove row")
          )
        )
      )
      max_idx(i)
      active_ix(sort(unique(c(active_ix(), i))))
    }, ignoreInit = TRUE)

    # Remove row (safe for max_idx() == 0)
    observe({
      lapply(seq_len(max_idx()), function(i) {
        rid <- paste0("remove_", i)
        if (!is.null(input[[rid]])) {
          observeEvent(input[[rid]], {
            removeUI(selector = id_row(i), multiple = FALSE)
            active_ix(setdiff(active_ix(), i))
          }, ignoreInit = TRUE, once = TRUE)
        }
        NULL
      })
    })

    # Helpers
    `%||%` <- function(a, b) if (is.null(a)) b else a
    is_real_ymd <- function(x) {
      x <- trimws(x)
      if (!nzchar(x)) return(FALSE)
      if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", x, perl = TRUE)) return(FALSE)
      px <- try(as.Date(x), silent = TRUE)
      if (inherits(px, "try-error")) return(FALSE)
      !is.na(px)
    }
    is_numeric_text <- function(x) {
      x <- trimws(x)
      if (!nzchar(x)) return(FALSE)
      grepl("^[+-]?\\d+(?:\\.\\d+)?$", x, perl = TRUE)  # no scientific notation by default
    }

    # Collect
    collected <- reactive({
      idx <- active_ix()
      if (!length(idx)) {
        return(list(index = integer(0),
                    labels = character(0),
                    date_raw = character(0),
                    valid = logical(0)))
      }

      labels <- vapply(idx, function(i) input[[paste0("label_", i)]] %||% "", character(1))
      right_raw <- vapply(idx, function(i) input[[paste0("date_",  i)]] %||% "", character(1))
      right_trim <- trimws(right_raw)

      valid <- if (type == "date") {
        vapply(right_trim, is_real_ymd, logical(1))
      } else {
        vapply(right_trim, is_numeric_text, logical(1))
      }

      # Red highlight only if non-empty and invalid
      bad_idx <- idx[!valid & nzchar(right_trim)]
      lapply(idx, function(i) {
        toggle_invalid_class(session, paste0("date_", i), add = i %in% bad_idx)
        NULL
      })

      list(index = idx, labels = labels, date_raw = right_raw, valid = valid)
    })

    # Raw outputs (all active rows)
    labels_raw <- reactive(collected()$labels)
    date_raw   <- reactive(collected()$date_raw)
    valid      <- reactive(collected()$valid)

    # Filtered outputs; return NULL if none valid
    labels_out <- reactive({
      vals <- collected()
      ok <- vals$valid
      if (!length(ok) || !any(ok, na.rm = TRUE)) return(NULL)
      vals$labels[ok]
    })
    values_out <- reactive({
      vals <- collected()
      ok <- vals$valid
      if (!length(ok) || !any(ok, na.rm = TRUE)) return(NULL)
      vals$date_raw[ok]
    })

    return(list(
      labels_raw = labels_raw,
      date_raw   = date_raw,
      valid      = valid,
      labels_out = labels_out,   # NULL if none valid
      values_out = values_out    # NULL if none valid
    ))
  })
}

# Minimal JS helper: add/remove the .sdlm-invalid class on the input's parent container
toggle_invalid_class <- function(session, input_id, add = TRUE) {
  session$sendCustomMessage("sdlm_toggle_invalid", list(id = session$ns(input_id), add = isTRUE(add)))
}

stringDateListInitJS <- function() {
  tags$script(HTML("
    Shiny.addCustomMessageHandler('sdlm_toggle_invalid', function(x){
      var el = document.getElementById(x.id);
      if(!el) return;
      var parent = el.closest('.form-group') || el.parentElement;
      if(!parent) parent = el.parentElement;
      if(x.add){ parent.classList.add('sdlm-invalid'); }
      else{ parent.classList.remove('sdlm-invalid'); }
    });
  "))
}





### Quick Date Ranges ----

# ---- Quick Date Range Module: UI (calendar/slider shown only for "Custom") ----
mod_quick_date_range_ui <- function(
  id,
  label_quick    = "Quick ranges",
  label_picker   = "Time period",                 # label shown above calendar/slider
  custom_picker  = c("calendar", "slider"),       # <-- new param, not exposed to end users
  presets        = c("custom", "ytd", "past_year", "3y", "5y", "none")  # default removes "latest"
) {
  ns <- NS(id)
  custom_picker <- match.arg(custom_picker)

  # Map *keys* -> labels (we will use keys as the input value)
  preset_labels <- c(
    custom     = "Custom",                         # <-- new "Custom" quick button
    ytd        = "Year to date",
    past_year  = "Past year",
    `3y`       = "3 years",
    `5y`       = "5 years",
    none       = "No filter",
    latest     = "Latest"                          # still supported if you pass it in `presets`
  )

  # Ensure valid non-empty presets in the given order
  presets <- intersect(presets, names(preset_labels))
  if (!length(presets)) presets <- "none"

  # radioGroupButtons returns the *value* of each element in `choices`;
  # using a named vector → names = labels (shown), values = keys (returned).
  choice_names  <- unname(preset_labels[presets])
  choice_values <- presets
  choices_vec   <- stats::setNames(choice_values, choice_names)

  tagList(
    shinyWidgets::radioGroupButtons(
      inputId   = ns("quick_range"),
      label     = label_quick,
      choices   = choices_vec,                     # value is a *key* such as "custom"
      status    = "danger",
      justified = TRUE,
      width     = "100%"
    ),
    # Placeholder for the on-demand picker (calendar OR slider).
    # It is rendered *only* when "Custom" is selected.
    uiOutput(ns("custom_picker"))
  )
}

# ---- Quick Date Range Module: Server (danger styling fixed) ----------------
# bounds_reactive: reactive() returning list(min = Date, max = Date)
# ---- Quick Date Range Module: Server (calendar/slider only for "Custom") ----
# `data_tbl` can be a reactive or a static tbl_sql with a Date column `time_period`.
mod_quick_date_range_server <- function(
  id,
  data_tbl,
  presets            = c("custom", "ytd", "past_year", "3y", "5y", "none"),
  default            = c("5y", "3y", "past_year", "ytd", "custom", "none"),
  frequency          = c("monthly", "quarterly", "daily", "weekly"),
  custom_picker      = c("calendar", "slider"),   # <-- choose the hidden control type
  label_picker       = "Time period",             # <-- label shown above the picker
  preserve_selection = TRUE
) {
  moduleServer(id, function(input, output, session) {
    # ----- Validate args -------------------------------------------------------
    valid_freq <- c("monthly", "quarterly", "daily", "weekly")
    frequency  <- if (length(frequency) == 1 && frequency %in% valid_freq) frequency else "monthly"

    #custom_picker <- match.arg(custom_picker)

    preset_labels <- c(
      custom     = "Custom",
      ytd        = "Year to date",
      past_year  = "Past year",
      `3y`       = "3 years",
      `5y`       = "5 years",
      none       = "No filter",
      latest     = "Latest"  # supported if you allow it in `presets`
    )

    presets <- intersect(presets, names(preset_labels))
    if (!length(presets)) presets <- "none"

    valid_default <- c("5y", "3y", "past_year", "ytd", "custom", "latest", "none")
    default <- if (length(default) == 1 && default %in% valid_default) default else "5y"
    if (!(default %in% presets)) default <- presets[[1]]

    to_reactive <- function(x) if (shiny::is.reactive(x)) x else shiny::reactive(x)
    data_r <- to_reactive(data_tbl)

    # ----- Bounds from DB (no na.rm to avoid pool issues) ---------------------
    bounds <- shiny::reactive({
      d <- data_r(); req(inherits(d, "tbl_sql"))
      b <- d %>%
        dplyr::summarise(
          min = min(.data$time_period),
          max = max(.data$time_period)
        ) %>%
        dplyr::collect()
      list(min = as.Date(b$min[[1]]), max = as.Date(b$max[[1]]))
    })

    # ----- Helper: date range for a preset, clamped to bounds -----------------
    .range_for_preset <- function(key, b) {
      minD <- b$min; maxD <- b$max
      if (is.null(minD) || is.null(maxD) || anyNA(c(minD, maxD))) return(NULL)
      clamp <- function(d) max(minD, min(d, maxD))
      ytd_start <- as.Date(sprintf("%s-01-01", lubridate::year(maxD)))

      rng <- switch(key,
        latest    = c(maxD, maxD),
        ytd       = c(clamp(ytd_start), maxD),
        past_year = c(clamp(maxD %m-% lubridate::years(1)), maxD),
        `3y`      = c(clamp(maxD %m-% lubridate::years(3)), maxD),
        `5y`      = c(clamp(maxD %m-% lubridate::years(5)), maxD),
        none      = c(minD, maxD),
        custom    = NULL,  # handled by picker
        NULL
      )
      if (is.null(rng)) NULL else as.Date(rng)
    }

    # Helper for slider: build a sequence of "tick" dates by frequency
    .seq_by_freq <- function(minD, maxD, freq) {
      minD <- as.Date(minD); maxD <- as.Date(maxD)
      switch(freq,
        daily     = seq(minD, maxD, by = "1 day"),
        weekly    = seq(lubridate::floor_date(minD, "week", week_start = 1),
                        lubridate::floor_date(maxD, "week", week_start = 1), by = "1 week"),
        quarterly = seq(lubridate::floor_date(minD, "quarter"),
                        lubridate::floor_date(maxD, "quarter"), by = "3 months"),
        monthly   = seq(lubridate::floor_date(minD, "month"),
                        lubridate::floor_date(maxD, "month"), by = "1 month")
      )
    }

    first_bounds_applied <- shiny::reactiveVal(FALSE)

    # This is the single source of truth for the currently effective range
    current_range <- shiny::reactiveVal(NULL)

    # ----- Initialise radio with keys (so input$quick_range returns a key) ----
    shiny::observeEvent(TRUE, {
      choice_names  <- unname(preset_labels[presets])
      choice_values <- presets
      choices_vec   <- stats::setNames(choice_values, choice_names)
      shinyWidgets::updateRadioGroupButtons(
        session, "quick_range",
        choices   = choices_vec,
        selected  = default,
        status    = "danger",
        justified = TRUE
      )
    }, once = TRUE)

    # ----- Apply bounds; set initial current_range from the selected preset ----
    shiny::observeEvent(bounds(), {
      b <- bounds(); if (is.null(b) || anyNA(c(b$min, b$max))) return()

      # Determine starting range based on default selection (or keep preserved)
      if (!isTRUE(first_bounds_applied())) {
        start_key <- default
        rng <- if (identical(start_key, "custom")) .range_for_preset("none", b) else .range_for_preset(start_key, b)
        current_range(rng)
        first_bounds_applied(TRUE)
      } else if (isTRUE(preserve_selection)) {
        key <- input$quick_range
        if (length(key) == 1 && !is.na(key) && key != "custom") {
          current_range(.range_for_preset(key, b))
        }
      }
    }, ignoreInit = FALSE)

    # ----- Render the picker *only* for "custom" --------------------------------
    output$custom_picker <- shiny::renderUI({
      req(bounds())
      if (!identical(input$quick_range, "custom")) return(NULL)

      b   <- bounds()
      rng <- current_range()
      if (is.null(rng) || length(rng) != 2 || anyNA(rng)) rng <- c(b$min, b$max)

      if (identical(custom_picker, "calendar")) {

      shinyWidgets::airDatepickerInput(
          inputId        = session$ns("date_range"),
          label          = label_picker,
          value          = rng,
          range          = TRUE,
          autoClose      = TRUE,
          clearButton    = TRUE,
          separator      = " to ",
          inline         = TRUE,              # shows only while "Custom" is selected
          toggleSelected = FALSE,
          todayButton    = TRUE,
          minDate        = as.Date(b$min),    # <-- move here
          maxDate        = as.Date(b$max),    # <-- move here
          width          = "100%"
        )

      } else {
        # Two-way slider from shinyWidgets (labels are ISO dates)
        ticks <- .seq_by_freq(b$min, b$max, frequency)
        labs  <- format(ticks, "%Y-%m-%d")
        shinyWidgets::sliderTextInput(
          inputId   = session$ns("date_range"),
          label     = label_picker,
          choices   = labs,
          selected  = format(rng, "%Y-%m-%d"),  # length 2 => range mode
          grid      = TRUE,
          dragRange = TRUE,
          width     = "100%"
        )
      }
    })

    # ----- When user clicks a quick preset (non-custom) -> compute range -------
    shiny::observeEvent(input$quick_range, {
      b <- bounds(); if (is.null(b)) return()
      key <- input$quick_range
      if (length(key) != 1 || is.na(key)) return()

      if (!identical(key, "custom")) {
        rng <- .range_for_preset(key, b)
        current_range(rng)
        shinyWidgets::updateRadioGroupButtons(
          session, "quick_range",
          selected = key,
          status   = "danger"
        )
      }
      # If "custom", we do nothing here — the picker (when rendered) will seed
      # itself from current_range() and any change will be captured below.
    }, ignoreInit = TRUE)

    # ----- Manual edits in the picker (only visible for "custom") --------------
    shiny::observeEvent(input$date_range, {
      req(identical(input$quick_range, "custom"))
      v <- input$date_range
      if (is.null(v) || length(v) != 2) return()

      # Calendar yields Date; slider yields character (ISO). Normalize to Date.
      if (inherits(v, "Date")) {
        rng <- as.Date(v)
      } else {
        rng <- as.Date(v, format = "%Y-%m-%d")
      }

      # Clamp to bounds defensively
      b <- bounds()
      rng <- c(max(b$min, min(rng[1], b$max)), max(b$min, min(rng[2], b$max)))
      current_range(as.Date(rng))
    }, ignoreInit = TRUE)

    # ----- Public API -----------------------------------------------------------
    list(
      date_range = shiny::reactive({
        rng <- current_range()
        if (is.null(rng) || length(rng) != 2 || anyNA(rng)) return(NULL)
        as.Date(rng)
      }),
      selected_quick_range = shiny::reactive({
        key <- input$quick_range
        if (length(key) != 1 || is.na(key)) return(NA_character_)
        key
      }),
      bounds = shiny::reactive(bounds())
    )
  })
}



# Filter Picker Server ---- 
# ---- UI: sector picker -------------------------------------------------------
  mod_filter_picker_ui <- function(
    id,
    label                = "Sector Filtering",
    multiple             = TRUE,
    actions_box          = TRUE,
    live_search          = TRUE,
    virtual_scroll       = 10,
    selected_text_format = "count > 2",
    width                = NULL
  ) {
    ns <- NS(id)
    shinyWidgets::pickerInput(
      inputId = ns("sectors"),
      label   = label,
      choices = NULL,            # populated by the server module below
      multiple = multiple,
      options  = list(
        `actions-box`           = actions_box,
        `live-search`           = live_search,
        `virtual-scroll`        = virtual_scroll,
        `selected-text-format`  = selected_text_format
      ),
      width = width
    )
  }



# ---- Server: sector picker ---------------------------------------------------

mod_filter_picker_server <- function(
  id,
  data_tbl,                  # lazy tbl_sql (or tibble)
  column,                    # string or reactive string
  defaults_pretty   = character(0),
  mapping_provider  = NULL,  # optional custom ordering/label mapping
  normalise_fn      = identity
) {
  moduleServer(id, function(input, output, session) {
    to_reactive <- function(x) if (shiny::is.reactive(x)) x else shiny::reactive(x)
    data_r   <- to_reactive(data_tbl)
    column_r <- to_reactive(column)

    raw_values <- shiny::reactive({
      d   <- data_r()
      col <- column_r()
      validate(shiny::need(is.character(col) && length(col) == 1L && nzchar(col),
                           "Filter column must be a single, non-empty string"))

      if (inherits(d, "tbl_sql")) {
        vals <- d %>%
          dplyr::filter(!is.na(.data[[col]]), .data[[col]] != "") %>%
          dplyr::distinct(.data[[col]]) %>%
          dplyr::arrange(.data[[col]]) %>%
          dplyr::pull(1)  # pulls only the vector
        return(vals %||% character(0))
      }

      # in-memory fallback
      if (!is.data.frame(d) || !col %in% names(d)) return(character(0))
      unique(sort(as.character(stats::na.omit(d[[col]]))))
    })

    mapping <- shiny::reactive({
      rv <- raw_values()
      if (!length(rv)) {
        return(list(
          choices      = stats::setNames(character(0), character(0)),
          canon_levels = character(0),
          expand       = function(x) character(0)
        ))
      }
      if (is.null(mapping_provider)) {
        list(
          choices      = stats::setNames(rv, rv),
          canon_levels = rv,
          expand       = function(x) x
        )
      } else {
        out <- mapping_provider(rv)
        stopifnot(is.list(out), !is.null(out$choices), !is.null(out$canon_levels),
                  !is.null(out$expand), is.function(out$expand))
        out
      }
    })

    shiny::observeEvent(mapping(), {
      m <- mapping()
      current_sel <- input$sectors
      current_sel <- if (is.null(current_sel)) character(0) else normalise_fn(as.character(current_sel))
      valid_sel   <- intersect(current_sel, m$canon_levels)

      if (!length(valid_sel) && length(defaults_pretty)) {
        def_norm  <- normalise_fn(as.character(defaults_pretty))
        valid_sel <- intersect(def_norm, m$canon_levels)
        if (!length(valid_sel)) valid_sel <- NULL
      } else if (!length(valid_sel)) valid_sel <- NULL

      shinyWidgets::updatePickerInput(
        session  = session,
        inputId  = "sectors",
        choices  = m$choices,
        selected = valid_sel
      )
    }, ignoreInit = FALSE)

    selected <- shiny::reactive({
      sel <- input$sectors
      if (is.null(sel) || !length(sel)) NULL else as.character(sel)
    })

    list(
      choices    = shiny::reactive(mapping()$choices),
      selected   = selected,
      expand_raw = shiny::reactive(mapping()$expand)
    )
  })
}



