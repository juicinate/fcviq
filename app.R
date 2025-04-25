# FCVIQ ------
# An interactive Version of the Flemish CVI Questionnaire.

# Version 1.1
# Last updated: 25.04.2025

# Copyright (C) 2025  J. Corazolla
# https://github.com/juicinate

# Licensed under AGPL 3.0 ------

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.

library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(gtExtras)

ui <- bslib::page_navbar(
  useShinyjs(),
  theme = bs_theme(version = 5),
  title = "Flemish CVI Questionnaire (FCVIQ)",
  nav_panel("FCVIQ", uiOutput("questionsUI"), value = "input"),
  nav_panel("Auswertung", uiOutput("resultsOutput"), value = "results"),
  nav_panel("Info", uiOutput("information"), value = "info"),
  nav_spacer(),
  nav_item(
    selectInput(
      inputId = "language",
      label = NULL,
      choices = c("Deutsch" = "de", "English" = "en"),
      selected = "de",
      width = "150px"
    )
  ),
  nav_item(bslib::input_dark_mode()),
  id = "page"
)

server <- function(input, output, session) {
  # Reactive value to store the questions data
  questions_data <- reactiveVal(NULL)

  # Reactive value to store survey responses
  responses <- reactiveVal(NULL)

  # Reactive value to store summary of responses
  group_summary <- reactiveVal(NULL)

  # devmode()

  session$allowReconnect(TRUE)

  # Load questions from RDS file on startup
  observe({
    tryCatch(
      {
        # Load the RDS file
        df <- readRDS("fcviq.rds")

        required_columns <- c("id", "question_de", "question_en", "group")

        # Validate that the data has required columns
        if (!all(required_columns %in% colnames(df))) {
          showNotification(paste0("Error: RDS file must contain these columns: ", required_columns),
            type = "error"
          )
          return(NULL)
        }

        # Process the group column to handle multiple groups
        df$group <- as.character(df$group)

        questions_data(df)
        showNotification("Questions loaded")
      },
      error = function(e) {
        showNotification(paste("Error loading questions file:", e$message),
          type = "error"
        )
      }
    )
  })

  color_values <- data.frame(
    good = "#00A600", bad = "#D93243",
    bright = "#75BEBD", dark = "#5B3F79"
  )

  group_table <- data.frame(
    group = 1:5,
    cutoff = c(
      0.27,
      0.30,
      0.32,
      0.46,
      0.30
    ),
    group_name_de = c(
      "Objekt- und Gesichtserkennung",
      "Visuelles Interesse",
      "Crowding und visueller Überblick",
      "Fortbewegung im Raum",
      "Angstbezogenes Verhalten"
    ),
    group_name_en = c(
      "Object and face recognition",
      "Visual interest",
      "Clutter and distance viewing",
      "Moving in space",
      "Anxiety-related behaviour"
    )
  )

  output$questionsUI <- renderUI({
    req(questions_data())

    questions <- questions_data()

    page_fillable(
      # Add a div to show error message for unanswered questions
      div(id = "error-message", style = "color: red; margin-bottom: 15px; display: none;"),
      lapply(1:nrow(questions), function(i) {
        question_id <- questions$id[i]
        # Get question text based on selected language
        question_text <- questions$question_de[i]

        if (input$language == "en") {
          question_text <- questions$question_en[i]
        }

        div(
          id = paste0("question_container_", question_id),
          class = "question-container",
          style = "padding: 10px; margin-bottom: 10px; border-left: 4px solid transparent; border-top: 2px solid grey",
          tagList(
            h6(paste0(i, ": ", question_text)),
            radioButtons(
              inputId = paste0("q_", question_id),
              label = NULL,
              choices = if (input$language == "de") {
                c("Trifft nicht zu" = FALSE, "Trifft zu" = TRUE)
              } else {
                c("No" = FALSE, "Yes" = TRUE)
              },
              selected = NA, # ifelse(question_id %% 2 == 0, TRUE, FALSE), # change to NA for production, FALSE for testing
              inline = TRUE
            ),
          )
        )
      }),
      br(),
      actionButton("submit", "Auswerten", class = "btn-primary", width = "100%"), 
      br()
    )
  })

  # Handle submission
  observeEvent(input$submit, {
    req(questions_data())

    questions <- questions_data()

    # Collect all responses
    response_data <- data.frame(
      id = character(),
      response = logical(),
      group_raw = character(),
      stringsAsFactors = FALSE
    )

    # Check if all questions are answered
    all_answered <- TRUE
    unanswered_questions <- c()

    # Reset all question container styles first
    for (i in 1:nrow(questions)) {
      question_id <- questions$id[i]
      runjs(sprintf("document.getElementById('question_container_%s').style.borderLeft = '4px solid transparent';", question_id))
    }

    for (i in 1:nrow(questions)) {
      question_id <- questions$id[i]
      input_id <- paste0("q_", question_id)

      if (is.null(input[[input_id]])) {
        all_answered <- FALSE
        unanswered_questions <- c(unanswered_questions, i)

        # Highlight unanswered question with red border
        runjs(sprintf("document.getElementById('question_container_%s').style.borderLeft = '4px solid #dc3545';", question_id))
        runjs(sprintf("document.getElementById('question_container_%s').scrollIntoView({ behavior: 'smooth', block: 'center' });", question_id))
      } else {
        response_data <- rbind(
          response_data,
          data.frame(
            id = question_id,
            response = as.logical(input[[input_id]]),
            group_raw = questions$group[i],
            stringsAsFactors = FALSE
          )
        )
      }
    }

    if (!all_answered) {
      # Show error message with specific question numbers
      error_message <- if (input$language == "de") {
        sprintf(
          "Bitte beantworten Sie alle Fragen. Fehlende Fragen: %s",
          paste(unanswered_questions, collapse = ", ")
        )
      } else {
        sprintf(
          "Please answer all questions. Missing questions: %s",
          paste(unanswered_questions, collapse = ", ")
        )
      }

      runjs(sprintf("
        document.getElementById('error-message').innerText = '%s';
        document.getElementById('error-message').style.display = 'block';
        document.getElementById('error-message').scrollIntoView({ behavior: 'smooth', block: 'center' });
      ", error_message))

      return()
    } else {
      # Hide error message if all questions are answered
      runjs("document.getElementById('error-message').style.display = 'none';")
    }

    # Process multiple groups
    processed_responses <- response_data %>%
      # Split the group_raw column by comma and create separate rows
      separate_rows(group_raw, sep = "\\s*,\\s*") %>%
      # Rename to group for consistency
      rename(group = group_raw) %>%
      # Trim any whitespace
      mutate(group = trimws(group))

    # Save responses
    responses(processed_responses)

    # Show notification
    submit_success <- if (input$language == "de") "Erfolgreich abgeschickt." else "Survey submitted successfully!"
    showNotification(submit_success, type = "message")

    # show the results tab
    nav_select("page", "results")
  })

  # Render results
  output$resultsOutput <- renderUI({
    req(responses())

    responses_data <- responses()

    # Group analysis
    responses_summary <- responses_data |>
      group_by(group) |>
      filter(!is.na(group)) |>
      summarise(
        score = round(sum(response == TRUE) / n(), 2)
      )

    responses_summary <- merge(responses_summary, group_table) |>
      mutate(color_val = ifelse(score <= cutoff, color_values$bad, color_values$good)) # add colour red and green

    group_summary(responses_summary)

    # Create output elements
    tagList(page_fillable(
      h3("Ergebnisse"), br(),
      layout_column_wrap(
        # Summary table
        uiOutput("summaryTable"),

        # Visualization
        card(plotOutput("groupPlot"),
          min_height = 300, full_screen = TRUE
        ),
        width = "450px"
      ),
      br(), actionButton("reset", "Neu ausfüllen", class = "btn-warn")
    ))
  })

  # Render summary table
  output$summaryTable <- renderUI({
    req(group_summary())

    group_summary_data <- group_summary()

    # Column selection
    group_summary_data <- group_summary_data |>
      select(Domäne = group_name_de, Domain = group_name_en, score, cutoff)

    gt_summary <- group_summary_data |>
      gt() |> 
      tab_options(table.width = pct(90)) |>
      cols_label(score = "Score", cutoff = "Cut-off") |>
      cols_hide(ifelse(input$language == "de", "Domain", "Domäne"))

    card(gt_summary, full_screen = TRUE, min_height = 300)
  })

  # Render plot
  output$groupPlot <- renderPlot({
    req(group_summary())

    group_summary_data <- group_summary()
    if (in_devmode() == TRUE) saveRDS(group_summary_data, "group.rds")

    gg_summary <- ggplot(
      data =
        group_summary_data |>
          select(score, color_val, cutoff, group, group_name = ifelse(input$language == "de", "group_name_de", "group_name_en")),
      aes(
        x = score,
        y = reorder(factor(group_name), group),
        fill = color_val
      )
    ) +
      annotate("rect", xmin = 0, xmax = 1, ymin = 0.6, ymax = 1.4, fill = "grey90") +
      geom_col(width = 0.3) +
      geom_segment(aes(x = cutoff, xend = cutoff, y = 0.7, yend = 1.3), colour = "grey50", linewidth = 0.8) +
      geom_text(aes(x = 1.1, label = paste0(round(score, 2))), hjust = 1) +
      scale_x_continuous(breaks = c(seq(0, 1, length.out = 6)), limits = c(0, 1.1)) +
      labs(x = "Score", y = element_blank()) +
      theme_void() +
      theme(
        axis.text = element_text(size = 16, vjust = 0.5),
        legend.position = "none"
        
      )

    gg_summary +
      facet_wrap(vars(group_name), ncol = 1, scales = "free_y") +
      theme(
        strip.text = element_blank(),
        strip.background = element_blank()
      )
  })

  observeEvent(input$reset, {
      req(responses())

      
  })

  output$information <- renderUI({
    page_fillable(includeMarkdown("README.md"), br())
  })
}

shinyApp(ui, server)
