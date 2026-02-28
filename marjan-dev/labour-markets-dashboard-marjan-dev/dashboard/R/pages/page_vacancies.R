vacancies_ui <- function(id) {
  ns <- NS(id)
  div(class = "govuk-width-container",
    tags$main(class = "govuk-main-wrapper", id = "main-content",
      tags$span(class = "govuk-caption-xl", "Labour Market"),
      tags$h1(class = "govuk-heading-xl", "Vacancies"),
      tags$p(class = "govuk-body-s", paste("Last updated:", Sys.Date())),
      coming_soon_card("Vacancy statistics and trends are under development and will be available soon.")
    )
  )
}

vacancies_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Vacancies-specific server logic (add later)
  })
}
