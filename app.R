# FCVIQ ------
# An interactive Version of the Flemish CVI Questionnaire.

# Version 1.0
# Last updated: 19.04.2025

# Copyright (C) 2025  J. Corazolla
# https://github.com/juicinate

# Licensed under AGPL 3.0 ------

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(ggplot2)
library(bslib)
library(gt)
library(gtExtras)

ui <- bslib::page_navbar(
  useShinyjs(),
  theme = bs_theme(version = 5, preset = "pulse"),
  title = "Flemish CVI Questionnaire (FCVIQ)",
  nav_panel("FCVIQ", uiOutput("questionsUI"), value = "input"),
  nav_panel("Auswertung", uiOutput("resultsOutput"), value = "results"),
  nav_item(
    selectInput(
      inputId = "language",
      label = NULL,
      choices = c("Deutsch" = "de", "English" = "en"),
      selected = "de",
      width = "150px"
    )
  ),
  id = "page"
)

server <- function(input, output, session) {
  # Reactive value to store the questions data
  questions_data <- reactiveVal(NULL)

  # Reactive value to store survey responses
  responses <- reactiveVal(NULL)

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
  
  color_values <- data.frame(good = "#00A600", bad = "#D93243", 
                             bright = "#75BEBD", dark = "#5B3F79")

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
          style = "padding: 10px; margin-bottom: 15px; border-left: 4px solid transparent;",
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
              selected = NA,
              inline = TRUE
            )
          )
        )
      }),
      br(),
      actionButton("submit", "Auswerten", class = "btn-primary"),
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

    response_data <- responses()

    # Group analysis
    group_summary <- response_data %>%
      group_by(group) %>%
      summarise(
        yes_count = sum(response == TRUE),
        total_count = n(),
        score = round(yes_count / total_count, 1),
        .groups = "drop"
      )

    # Create output elements
    tagList(
      h3("Ergebnisse"), br(),

      # Summary table
      gt_output("summaryTable"), br(),

      # Visualization
      # plotOutput("groupPlot")
    )
  })

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

  # Render summary table
  output$summaryTable <- render_gt({
    req(responses())

    response_data <- responses()

    # Group analysis
    group_summary <- response_data %>%
      filter(!is.na(group)) %>%
      group_by(group) %>%
      summarise(
        `Score` = round(sum(response == TRUE) / n(), 1),
        .groups = "drop"
      )

    group_summary <- merge(group_summary, group_table) %>%
      select(Domäne = group_name_de,Score, cutoff) |> 
      mutate(colour_val = ifelse(Score > cutoff, color_values$bad, color_values$good)) # add colour red and green

    if (input$language == "en") {
      group_summary <- group_summary %>%
        mutate(Domain = group_table$group_name_en) |>
        select(-Domäne)
    }

    group_summary |>
      mutate(
        percent = Score * 100,
        cutoff_perc = cutoff * 100
      ) |>
      gt() |>
      gt_plt_bullet(column = percent, target = cutoff_perc,
          palette = c("gray", color_values$dark), palette_col = colour_val) |>
      cols_label(cutoff = "Cut-off", percent = "")
  })

  # Render plot
  output$groupPlot <- renderPlot({
    req(responses())

    response_data <- responses()

    # Group analysis
    group_summary <- response_data %>%
      filter(!is.na(group)) %>%
      group_by(group) %>%
      summarise(
        score = sum(response == TRUE) / n(),
        .groups = "drop"
      )

    group_summary <- merge(group_summary, group_table) |> group_by(group)
    # write_rds(group_summary, "group.rds")

    ggplot(group_summary, aes(
      x = group,
      y = score,
      fill = group
    )) +
      geom_col() +
      geom_text(aes(label = paste0(round(score, 1))), vjust = -0.5) +
      labs(title = "Group Scores (Percentage of 'Yes' Answers)", x = "Group", y = "Score") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      ylim(0, 1.1) # Add some space for labels
  })
}

shinyApp(ui, server)
