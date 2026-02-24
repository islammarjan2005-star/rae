#' Create a GOV.UK-style top navigation bar for Shiny tabsets
#'
#' Generates a responsive GOV.UK service navigation component that works with
#' a hidden `tabsetPanel`. Each navigation item is rendered as an `actionLink`
#' so that clicking it triggers a server-side observer to switch tabs.
#'
#' @param tabs Named character vector. Names are the labels shown in the
#'   navigation bar; values are the corresponding `value`s used in the
#'   `tabsetPanel`.
#'   Example: `c("Home" = "Home", "Employment" = "Employment")`.
#' @param selected Character. Optional label or tab value to mark as active.
#'   Defaults to `NULL` (no active state).
#' @param id_prefix Character. Prefix for the `actionLink` IDs. Defaults to `"nav"`.
#'   The final ID will be `paste0(id_prefix, "-", value)`.
#'
#' @return A `<div>` containing the GOV.UK service navigation markup.
#'
#' @examples
#' # Basic GOV.UK navigation bar for a tabsetPanel
#' govuk_top_nav(
#'   tabs = c("Home" = "Home", "Employment" = "Employment", "Vacancies" = "Vacancies"),
#'   selected = "Home",
#'   id_prefix = "nav"
#' )
#'
#' @export


govuk_top_nav <- function(tabs, selected = NULL, id_prefix = "nav") {
  stopifnot(!is.null(names(tabs)), all(nzchar(names(tabs))))
  labels <- names(tabs)                # e.g. "Home", "Employment"
  routes <- unname(as.character(tabs)) # e.g. "home", "employment"
  
  items <- lapply(seq_along(labels), function(i) {
    lab <- labels[i]
    route <- routes[i]
    is_active <- identical(route, selected) || identical(lab, selected)
    
    li_class <- paste(
      "govuk-service-navigation__item",
      if (is_active) "govuk-service-navigation__item--active"
    )
    
    # IMPORTANT: plain <a> with hash href for shiny.router
    tags$li(
      class = li_class,
      tags$a(
        href = paste0("#!/", route),
        class = "govuk-service-navigation__link",
        `aria-current` = if (is_active) "page" else NULL,
        lab
      )
    )
  })
  
  tags$div(
    class = "govuk-service-navigation",
    `data-module` = "govuk-service-navigation",
    tags$div(
      class = "govuk-width-container",
      tags$div(
        class = "govuk-service-navigation__container",
        tags$nav(
          `aria-label` = "Menu",
          class = "govuk-service-navigation__wrapper",
          tags$button(
            type = "button",
            class = "govuk-service-navigation__toggle govuk-js-service-navigation-toggle",
            `aria-controls` = "navigation",
            hidden = "hidden",
            "Menu"
          ),
          tags$ul(class = "govuk-service-navigation__list", id = "navigation", items)
        )
      )
    )
  )
}




