


# library(dplyr)
# library(DT)
# library(ggplot2)
# library(faux)
# library(ggrain)
# library(shinydashboard)
# library(shiny)
# library(tibble)
# library(DT)

#' Run a shiny app to check TAP results
#'
#' @param ... Additional arguments
#'
#' @import dplyr ggplot2 faux ggrain shiny shinydashboard tibble tidyr
#' @return runs a shiny app
#' @export
#'
#' @examples
#'  \dontrun{
#'  mousetrap_checkr()
#' }
mousetrap_checkr <- function(...) {

  id_names <- c("id", "ID", "i.d.", "id_num", "id_no")
  condition_names <- c("condition", "CONDITION", "cond", "Condition", "COND")
  response_names <- c("response", "RESPONSE", "resp", "Response", "RESP")
  reward_names <- c("reward", "REWARD", "rew", "Reward", "REW")

  names_grid <- tidyr::expand_grid(id_names, condition_names, response_names, reward_names)

  ui <- dashboardPage(
    dashboardHeader(title = "Sniffy CheckR"),

    dashboardSidebar(
      sidebarMenu(
        id = "sidebar_menu",
        menuItem(text = "Data", tabName = "data"),
        menuItem(text = "Results", tabName = "results", selected = TRUE)
      ),
      numericInput(inputId = "cand_no", label = "Candidate number", value = 12345),
      fluidRow(actionButton(inputId = "check_results", label = "Check results"), align = "center")
    ),

    dashboardBody(

      tags$style(HTML("

                .box.box-solid.box-primary>.box-header {
                color:#fff;
                background:#066379
                }
                .skin-blue .main-header .logo {
                background-color: #034554;
                }
                .skin-blue .main-header .logo:hover {
                background-color: #066379;
                }
                .skin-blue .main-header .navbar {
                background-color: #066379;
                }
                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                background-color: #034554;
                }
                .skin-blue .sidebar-menu > li.active > a,
                .skin-blue .sidebar-menu > li:hover > a {
                 border-left-color: #833786;
                }

                ")),

      tabItems(
        tabItem(
          tabName = "results",
          fluidRow(
            column(
              12,
              fluidRow(align = "center",
                       box(width = 12, collapsible = TRUE,
                           title = "Data cleaning",
                           tableOutput("data_cleaning")
                       )
              ),
              fluidRow(align = "center",
                       box(width = 12, collapsible = TRUE,
                           title = "Descriptives",
                           tableOutput("descriptives")
                       )
              ),
              fluidRow(align = "center",
                       box(width = 12, collapsible = TRUE,
                           title = "Plot",
                           plotOutput("plot")
                       )
              ),
              fluidRow(align = "center",
                       box(width = 12, collapsible = TRUE,
                           title = "t-test results",
                           tableOutput("ttest")
                       )
              )
            )
          )
        ),
        tabItem(
          tabName = "data",
          fluidRow(
            column(
              12,
              fluidRow(align = "center",
                       box(width = 12, collapsible = TRUE,
                           title = "Dataset",
                           DT::DTOutput("full_data")),

              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    rvs <- reactiveValues(
      sniffy_data_messy = NULL,
      sniffy_data_tidy = NULL,
      sniffy_data_na_rm = NULL,
      n_missing = NULL,
      n_wrong_number = NULL
    )

    observeEvent(input$check_results, {

      rvs$sniffy_data_messy <- mousetrap::get_data(input$cand_no)

      ## mess-up names

      set.seed(input$cand_no)
      messed_up_names <- unlist(names_grid[sample(1:nrow(names_grid), size = 1), ])
      names(rvs$sniffy_data_messy) <- messed_up_names

      id = messed_up_names[1]
      condition = messed_up_names[2]
      responses = messed_up_names[3]
      reward = messed_up_names[4]

      # Data cleaning -----------------------------------------------------------

      # get number of NAs
      rvs$n_missing <- rvs$sniffy_data_messy |>
        dplyr::filter(is.na(!!sym(responses))) |>
        nrow()

      # remove missing values
      rvs$sniffy_data_na_rm <- rvs$sniffy_data_messy |>
        dplyr::filter(!is.na(!!sym(responses)))

      # get number of incorrect values
      rvs$n_wrong_number <- rvs$sniffy_data_na_rm |>
        dplyr::filter(!!sym(reward) == 0 | !!sym(reward) > 100) |>
        nrow()

      # remove incorrect values
      rvs$sniffy_data_tidy <- rvs$sniffy_data_na_rm |>
        dplyr::filter(between(!!sym(reward), 1, 100))

      # compute outcome
      rvs$sniffy_data_tidy <- rvs$sniffy_data_tidy |>
        dplyr::mutate(resp_per_rew = !!sym(responses)/!!sym(reward))

      # define conditions for the t-test
      # rvs$sniffy_data_tidy <- rvs$sniffy_data_tidy |>
      #   dplyr::mutate(condition = factor(condition,
      #                                    labels = c("Ratio", "Ratio",
      #                                               "Interval", "Interval")))
      rvs$sniffy_data_tidy[[condition]] = factor(
        rvs$sniffy_data_tidy[[condition]],
        labels = c("Ratio", "Ratio",
                   "Interval", "Interval")
      )

      # Data cleaning summary  --------------------------------------------------


      output$data_cleaning <- renderTable({

        tibble::tibble(
          Issue = c("Numer of missing values:", "Number of incorrect values:", "Total number of cases removed:"),
          N = c(rvs$n_missing, rvs$n_wrong_number, rvs$n_missing + rvs$n_wrong_number)
        )

      })


      # Descriptive summary -----------------------------------------------------


      output$descriptives <- renderTable({

        rvs$sniffy_data_tidy |>
          dplyr::group_by(!!sym(condition)) |>
          dplyr::summarise(
            n = dplyr::n(),
            min = min(resp_per_rew, na.rm = TRUE),
            max = max(resp_per_rew, na.rm = TRUE),
            mean = mean(resp_per_rew, na.rm = TRUE),
            median = median(resp_per_rew, na.rm = TRUE),
            sd = sd(resp_per_rew, na.rm = TRUE),
            ci_lower = ggplot2::mean_cl_normal(resp_per_rew)$ymin,
            ci_upper = ggplot2::mean_cl_normal(resp_per_rew)$ymax
          )

      })

      # Plots -------------------------------------------------------------------

      output$plot <- renderPlot({

        set.seed(input$cand_no)

        rvs$sniffy_data_tidy |>
          ggplot2::ggplot(data = _, aes(x = !!sym(condition), y = resp_per_rew,
                                        colour = !!sym(condition), fill = !!sym(condition))) +
          #geom_point(position = position_jitter(width = 0.15), alpha = 0.15, size = 3) +
          stat_summary(fun.data = mean_cl_normal, size = 1) +
          ggrain::geom_rain(alpha = 0.2, point.args = list(size = 2, alpha = 0.2)) +
          scale_colour_manual(values = c("#066379", "#833786")) +
          scale_fill_manual(values = c("#066379", "#833786")) +
          labs(x = "\nCondition", y = "Response/Reward ratio\n") +
          theme_minimal() +
          theme(
            axis.title = element_text(face = "bold"),
            legend.position = "none"
          )

      })

      # t-test summary ----------------------------------------------------------

      output$ttest <- renderTable({

        sniffy_t_formula = formula(paste0("resp_per_rew~", condition))

        sniffy_t <- rvs$sniffy_data_tidy |>
          t.test(sniffy_t_formula, data = _)

        tibble::tibble(
          ratio_mean = sniffy_t$estimate[1],
          int_mean = sniffy_t$estimate[2],
          df = sniffy_t$parameter,
          t.value = sniffy_t$statistic,
          p.value = sniffy_t$p.value
        )
      })

      # Full data  --------------------------------------------------------------

      output$full_data <- DT::renderDT({
        DT::datatable(
          rvs$sniffy_data_messy |>
            dplyr::mutate(
              highlight_col = dplyr::case_when(
                is.na(!!sym(responses)) ~ 1,
                !!sym(reward) == 0 | !!sym(reward) > 100 ~ 2,
                TRUE ~ 0
              )
            ),
          options = list(
            paging = FALSE,
            columnDefs = list(list(visible=FALSE, targets=c(5)))
          )
        ) |>
          DT::formatStyle(
            "highlight_col",
            target = 'row',
            backgroundColor = DT::styleEqual(c(1, 2), c('#833786', '#066379')),
            color = DT::styleEqual(c(1, 2), c("white"))

          )
      })

    })
  }

  shinyApp(ui, server, ...)
}
