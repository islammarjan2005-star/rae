### EMPLOYMENT BY AGE CARD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Identically replicates the FT/PT card pattern for Employment by Age data

## EMPLOYMENT BY AGE UI ## -----
employment_age_stats_card_ui <- function(id) {
  ns <- NS(id)
  tags$script("$(function() {$('[data-toggle=\"tooltip\"]').tooltip();});")

  mod_govuk_data_vis_card_ui(
    id = ns("age_trend_card"),
    title = "Employment by Age Group",
    help_text = "Employment levels and rates by age group. Source: ONS - Labour Force Survey",
    table_content  = reactableOutput(ns("age_table"), height = "350px"),
    visual_content = plotlyOutput(ns("age_plot"),  height = "350px"),
    query = ns("sql_query"),

    # Plain controls (stay visible in the dropdown)
    controls = list(
      shinyWidgets::radioGroupButtons(
        inputId = ns("which_measure"),
        label = "Level or Rate",
        choices = c("Level (000s)", "Rate (%)"),
        justified = TRUE,
        status = "danger"
      ),

      mod_quick_date_range_ui(
        id = ns("age_dates"),
        label_quick = "Quick Ranges",
        label_picker  = "Time period",
        custom_picker = "slider",
        presets = c("custom", "ytd", "past_year", "3y", "5y", "none")
      ),
      mod_filter_picker_ui(
        id = ns("age_group_filter"),
        label = "Age Group Filtering",
        multiple = TRUE,
        actions_box = TRUE,
        live_search = TRUE,
        virtual_scroll = 10,
        selected_text_format = "count > 2"
      )
    ),

    # Inner UI for the accordion (module wraps/stylizes it)
    accordion_controls = list(
      shinyWidgets::radioGroupButtons(
        inputId = ns("chart_type"),
        label   = "Choose a graph :",
        choiceNames = list(
          tags$span(`data-toggle`="tooltip", title = "Stacked Bar Chart", tags$i(class = "fa fa-bar-chart")),
          tags$span(`data-toggle`="tooltip", title = "Line Chart",        tags$i(class = "fa fa-line-chart")),
          tags$span(`data-toggle`="tooltip", title = "Area Chart",        tags$i(class = "fa fa-area-chart"))
        ),
        choiceValues = c("stacked_bar","line","stacked_area"),
        justified = TRUE,
        size = "sm",
        status = "danger"
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'stacked_bar'", ns("chart_type")),
        shinyWidgets::sliderTextInput(
          inputId = ns("stack_mode"),
          label = "Time Interval between bars",
          choices = c("monthly","quarterly","annually","5year","decade"),
          selected = "annually",
          grid = TRUE
        )
      ),

      mod_annotation_line_ui(ns("date_lines"),
        type = "date", title = "Add key dates",
        add_label = "Add key date", show_delete = TRUE, auto_mask_date = TRUE
      ),
      mod_annotation_line_ui(ns("value_lines"),
        type = "value", title = "Add key values",
        add_label = "Add key values", show_delete = TRUE, auto_mask_date = TRUE
      )
    )
  )
}

## EMPLOYMENT BY AGE SERVER ## -----
employment_age_stats_card_server <- function(id, conn = APP_DB$pool) {
  moduleServer(id, function(input, output, session) {

    # UI defaults + annotations
    shinyWidgets::updateRadioGroupButtons(session, "which_measure", selected = "Level (000s)")
    date_lines  <- mod_annotation_line_server("date_lines",  type = "date")
    value_lines <- mod_annotation_line_server("value_lines", type = "value")

    # Reactive codes based on Level vs Rate selection
    codes <- reactive({
      sel <- input$which_measure %||% "Level (000s)"
      if (sel == "Level (000s)") {
        AGE_LEVEL_CODES
      } else {
        AGE_RATE_CODES
      }
    })

    # Y-axis label
    y_axis_label <- reactive({
      sel <- input$which_measure %||% "Level (000s)"
      if (sel == "Level (000s)") "Employment (000s)" else "Employment Rate (%)"
    })

    # Base cleaned view (lazy)
    cleaned_full_tbl <- reactive({
      get_age_tbl(codes())
    }) %>% bindCache(input$which_measure)

    # Picker and Date modules
    sector <- mod_filter_picker_server(
      id       = "age_group_filter",
      data_tbl = cleaned_full_tbl,
      column   = "age_group"
    )
    dates <- mod_quick_date_range_server(
      id        = "age_dates",
      data_tbl  = cleaned_full_tbl,
      presets   = c("custom", "ytd", "past_year", "3y", "5y", "none"),
      default   = "5y", frequency = "monthly",
      custom_picker = "slider",
      preserve_selection = TRUE
    )

    # Final data (lazy filters -> collect + SQL)
    dat <- reactive({
      dr <- dates$date_range()
      date_from <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[1]]) else NULL
      date_to   <- if (!is.null(dr) && length(dr) == 2) as.Date(dr[[2]]) else NULL
      vals      <- sector$selected()

      t <- cleaned_full_tbl() %>%
        apply_filters_general(
          date_col     = "time_period",
          date_from    = date_from,
          date_to      = date_to,
          where_in     = list(age_group = vals)
        )

      list(
        data = t %>% dplyr::collect(),
        sql  = sql_render_pool_safe(t)
      )
    })

    # Outputs
    output$age_table <- reactable::renderReactable({
      out <- dat(); req(nrow(out$data) > 0)
      reactable::reactable(out$data, sortable = TRUE, filterable = TRUE, resizable = TRUE,
                           pagination = TRUE, highlight = TRUE, striped = TRUE)
    })
    output$sql_query <- renderText({ dat()$sql })
    output$age_plot <- plotly::renderPlotly({
      out <- dat(); req(nrow(out$data) > 0)
      dbt_ts_plot(
        df = out$data,
        chart_type = input$chart_type,
        bar_interval = input$stack_mode,
        bar_agg = "last",
        x_title = "Time period (YYYY\u2011MM)", y_title = y_axis_label(),
        palette = dbt_palettes$gaf, initial_legend_mode = "hidden",
        group_col = "age_group",
        vlines = date_lines$values_out(), vline_labels = date_lines$labels_out(),
        hlines = value_lines$values_out(), hline_labels = value_lines$labels_out()
      )
    })
  })
}

## EMPLOYMENT BY AGE DATA ## ------

# Level codes: employment levels (000s) by age group
AGE_LEVEL_CODES <- c("YBTO", "YBTR", "YBTU", "YBTX", "LF26", "LFK4")

# Rate codes: employment rates (%) by age group
AGE_RATE_CODES  <- c("YBUA", "YBUD", "YBUG", "YBUJ", "LF2U", "LFK6")

# Code-to-age-group mapping (shared by both level and rate codes)
AGE_CODE_MAP <- c(
  # Level codes
  "YBTO" = "16-17", "YBTR" = "18-24", "YBTU" = "25-34",
  "YBTX" = "35-49", "LF26" = "50-64", "LFK4" = "65+",
  # Rate codes
  "YBUA" = "16-17", "YBUD" = "18-24", "YBUG" = "25-34",
  "YBUJ" = "35-49", "LF2U" = "50-64", "LFK6" = "65+"
)

get_age_tbl <- function(codes) {
  stopifnot(is.character(codes), length(codes) >= 1L)

  base <- dplyr::tbl(APP_DB$pool, dbplyr::in_schema("ons", "labour_market__age_group"))

  period_sql <- dbplyr::sql("
    to_date(
      initcap(substr(
        btrim(split_part(
          regexp_replace(
            btrim(regexp_replace(time_period::text, '\\\\s+', ' ', 'g')),
            '[\\u2013\\u2014]', '-', 'g'
          ),
          '-', 2
        )),
        1, 8
      )),
      'Mon YYYY'
    )::date
  ")

  base %>%
    dplyr::filter(.data$dataset_indentifier_code %in% !!codes) %>%
    dplyr::mutate(time_period = !!period_sql) %>%
    dplyr::mutate(
      age_group = dplyr::case_when(
        .data$dataset_indentifier_code %in% c("YBTO", "YBUA") ~ "16-17",
        .data$dataset_indentifier_code %in% c("YBTR", "YBUD") ~ "18-24",
        .data$dataset_indentifier_code %in% c("YBTU", "YBUG") ~ "25-34",
        .data$dataset_indentifier_code %in% c("YBTX", "YBUJ") ~ "35-49",
        .data$dataset_indentifier_code %in% c("LF26", "LF2U") ~ "50-64",
        .data$dataset_indentifier_code %in% c("LFK4", "LFK6") ~ "65+",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      time_period,
      dataset_id = .data$dataset_indentifier_code,
      age_group,
      value
    )
}

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
