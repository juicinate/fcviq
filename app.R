# FCVIQ ------
# An interactive Version of the Flemish CVI Questionnaire.

# Version 1.3
# Last updated: 10.05.2025

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

# Load libraries ------
library(shiny)
library(shinyjs)
library(shinyscroll)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(gtExtras)
library(quarto)

# Source utilities ------
source("utils.R")

# Set temorary directory for quarto ------
tempDir <- prepare_report()

# Define UI  ------
ui <- bslib::page_fillable(
  useShinyjs(),
  use_shinyscroll(),
  theme = bs_theme(version = 5, `enable-shadows` = TRUE),
  title = "Flemish CVI Questionnaire (FCVIQ)",
  inlineCSS(list(.unanswered = "border-left: 4px solid red")),
  navset_pill(
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
    nav_item(
      div(
        style = "padding: 10px;",
        input_dark_mode(style = css("--vertical-correction" = "-10px"))
      )
    ),
    id = "page"
  )
)

server <- function(input, output, session) {
  # Reactive value to store the questions data
  questions_data <- reactiveVal(NULL)

  # Reactive value to store the patient data
  patient_data <- reactiveValues(
    birth_date   = NULL,
    fill_date    = NULL,
    age          = NULL,
    surname      = NULL,
    name         = NULL,
    filled_by    = NULL
  )

  # Reactive value to store the color values (over or under cut-off)
  color_values <- reactiveVal(NULL)

  # Reactive value to store the table for outputs
  group_table <- reactiveVal(NULL)

  # Reactive value to store survey responses
  responses <- reactiveVal(NULL)

  # Reactive value to store summary of responses
  group_summary <- reactiveVal(NULL)

  session$allowReconnect(TRUE)

  # Load questions from RDS file on startup and observe patient data inputs
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


  observe({
    color_values <- data.frame(
      good = "#00A600", bad = "#D93243",
      bright = "#75BEBD", dark = "#5B3F79"
    )

    color_values(color_values)

    group_table <- data.frame(
      group = 1:5,
      cutoff = c(
        0.27,
        0.30,
        0.32,
        0.46,
        0.30
      )
    )

    if (input$language == "de") {
      group_table$group_name <- c(
        "Objekt- und Gesichtserkennung",
        "Visuelles Interesse",
        "Crowding und visueller Überblick",
        "Fortbewegung im Raum",
        "Angstbezogenes Verhalten"
      )
    } else {
      group_table$group_name <- c(
        "Object and face recognition",
        "Visual interest",
        "Clutter and distance viewing",
        "Moving in space",
        "Anxiety-related behaviour"
      )
    }

    group_table(group_table)
  })


  output$questionsUI <- renderUI({
    req(questions_data())

    questions <- questions_data()

    if (input$language == "de") {
      title_text <- "Flämischer CVI Fragebogen"
      explainer <- "Bitte wählen Sie alle Antworten aus,
      die auf Ihr Kind zutreffen."
    } else {
      title_text <- "Flemish CVI Questionnaire"
      explainer <- "Please answer all the following questions.
      Tick yes if the question applies to your child."
    }

    page_fluid(
      card(
        height = pct(100),
        card_title(title_text),

        # add a card for patient data
        card(
          card_header("Patientendaten"),
          fluidRow(
            textInput("surname", "Nachname:"),
            textInput("name", "Vorname:")
          ),
          fluidRow(
            dateInput("birthdate", "Geburtsdatum:",
              value = "2023-02-01",
              format = "dd-mm-yyyy", max = Sys.Date(),
              startview = "decade", weekstart = 1
            ),
            dateInput("filldate", "Ausgefüllt am:",
              value = Sys.Date(),
              format = "dd-mm-yyyy", max = Sys.Date(), weekstart = 1
            )
          ), textOutput("output_age"),
          fluidRow(
            textInput("filled_by", "Ausgefüllt von:", placeholder = "Mutter, Vater, Betreuer...")
          )
        ),

        # Add an explainer above the questions
        h6(explainer),

        # Add questions
        lapply(seq_len(nrow(questions)), function(i) {
          question_id <- questions$id[i]

          input_id <- paste0("q_", question_id)

          # Get question text based on selected language
          question_text <- questions$question_de[i]

          if (input$language == "en") {
            question_text <- questions$question_en[i]
          }

          div(
            id = paste0("question_container_", question_id),
            tagList(fluidRow(column(
              width = 8,
              h6(paste0(i, ": ", question_text))
            ), column(
              width = 4,
              radioButtons(
                inputId = input_id,
                label = NULL,
                choices = if (input$language == "de") {
                  c("Trifft nicht zu" = FALSE, "Trifft zu" = TRUE)
                } else {
                  c("No" = FALSE, "Yes" = TRUE)
                },
                inline = TRUE,
                selected = NA # ifelse(question_id %% 2 == 0, TRUE, FALSE) # change to NA for production, FALSE for testing
              )
            )))
          )
        }),
        actionButton("submit", "Auswerten", class = "btn-primary", width = "100%")
      )
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

    lapply(seq_len(nrow(questions)), function(i) {
      question_id <- questions$id[i]
      container <- paste0("question_container_", question_id)
      input_id <- paste0("q_", question_id)

      shinyjs::toggleClass(
        id = container,
        class = "unanswered",
        condition = is.null(input[[input_id]])
      )
    })

    # Find first unanswered question
    unanswered_questions <- c()
    for (i in seq_len(nrow(questions))) {
      question_id <- questions$id[i]
      input_id <- paste0("q_", question_id)

      if (is.null(input[[input_id]])) {
        unanswered_questions <- c(unanswered_questions, question_id)
      }
    }

    # Scroll to the first unanswered question
    if (length(unanswered_questions) > 0) {
      container <- paste0("question_container_", unanswered_questions[[1]])
      shinyscroll::scroll(container, "center")

      # Show error message with specific question numbers
      error_message <- if (input$language == "de") {
        paste0(
          "Bitte beantworten Sie alle Fragen. Fehlende Fragen: ",
          paste(as.character(unanswered_questions), collapse = ", ")
        )
      } else {
        paste0(
          "Please answer all questions. Missing questions: ",
          paste(as.character(unanswered_questions), collapse = ", ")
        )
      }
      showNotification(error_message, type = "warning")
    }

    if (length(unanswered_questions) == 0) {
      for (i in seq_len(nrow(questions))) {
        question_id <- questions$id[i]
        input_id <- paste0("q_", question_id)

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

      # Store all patient values for later use
      patient_data$birth_date <- input$birthdate
      patient_data$fill_date <- input$filldate
      patient_data$surname <- input$surname
      patient_data$name <- input$name
      patient_data$filled_by <- input$filled_by
      patient_data$age <- calculate_age(
        patient_data$birth_date, patient_data$fill_date
      )

      # Show notification
      submit_success <- if (input$language == "de") {
        "Erfolgreich abgeschickt."
      } else {
        "Survey submitted successfully!"
      }
      showNotification(submit_success, type = "message")

      # show the results tab
      nav_select("page", "results")
    }
  })

  # Render results
  output$resultsOutput <- renderUI({
    req(responses())

    responses_data <- responses()
    group_table <- group_table()
    color_values <- color_values()

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
        # Patient data
        uiOutput("patientOutput"),

        # Summary table
        uiOutput("summaryTable"),

        # Visualization
        card(plotOutput("groupPlot"),
          min_height = 300, full_screen = TRUE
        ),
        width = "450px"
      ),
      br(),
      actionButton("reset", "Neu ausfüllen", class = "btn-warning"),
      downloadButton("download_pdf", "PDF Report",
        icon = icon("save"), class = "btn-primary", stlye = "float: right;"
      )
    ))
  })

  # Render patient data table
  output$patientOutput <- renderUI({
    patient_info <- reactiveValuesToList(patient_data)

    age_label <- paste0(patient_info$age$years, " Jahre, ", patient_info$age$months, " Monate")

    if (patient_info$age$months == 1) {
      age_label <- paste0(patient_info$age$years, " Jahre, ", patient_info$age$months, " Monat")
    }

    pat_mat <- matrix(
      data = c(
        "Name:", paste0(patient_info$surname, ", ", patient_info$name),
        "Geburtsdatum:", format.Date(as.Date(patient_data$birth_date), format = "%d.%m.%Y"),
        "Aufgefüllt von:", patient_info$filled_by,
        "Testdatum:", format.Date(as.Date(patient_data$fill_date), format = "%d.%m.%Y"),
        "Alter:", age_label
      ),
      nrow = 5, byrow = TRUE
    )

    gt_summary <- as.data.frame(pat_mat) |>
      gt() |>
      tab_options(
        table.width = pct(90),
        column_labels.hidden = TRUE
      )

    card(gt_summary, full_screen = FALSE, min_height = 300)
  })

  # Render summary table
  output$summaryTable <- renderUI({
    req(group_summary())

    group_summary_data <- group_summary()

    # Column selection
    group_summary_data <- group_summary_data |>
      select(group_name, score, cutoff)

    gt_summary <- group_summary_data |>
      gt() |>
      tab_options(table.width = pct(90)) |>
      cols_label(
        group_name = ifelse(input$language == "de", "Domäne", "Domain"),
        score = "Score", cutoff = "Cut-off"
      )

    card(gt_summary, full_screen = TRUE, min_height = 300)
  })

  # Render plot
  output$groupPlot <- renderPlot({
    req(group_summary())

    group_summary_data <- group_summary()

    factor_levels <- unique(group_summary_data$group_name)

    gg_summary <- ggplot(
      data =
        group_summary_data |>
          select(score, color_val, cutoff, group_name),
      aes(
        x = score,
        y = factor(group_name, levels = factor_levels),
        fill = color_val
      )
    ) +
      annotate("rect", xmin = 0, xmax = 1, ymin = 0.6, ymax = 1.4, fill = "grey90") +
      geom_col(width = 0.3) +
      geom_segment(aes(x = cutoff, xend = cutoff, y = 0.7, yend = 1.3), colour = "grey50", linewidth = 0.8) +
      geom_text(aes(x = 1.2, label = paste0(round(score, 2))), hjust = 1) +
      scale_x_continuous(breaks = c(seq(0, 1, length.out = 6)), limits = c(0, 1.2)) +
      scale_y_discrete(limits = rev) +
      theme_void(base_size = 14, base_family = "Open Sans") +
      labs(x = "Score", y = element_blank()) +
      theme(
        axis.text.y = element_text(vjust = 0.5),
        legend.position = "none"
      )

    gg_summary +
      facet_wrap(vars(group_name = factor(group_name, levels = factor_levels)),
        ncol = 1, scales = "free_y"
      ) +
      theme(
        strip.text = element_blank(),
        strip.background = element_blank()
      )
  })

  observeEvent(input$reset, {
    req(responses())

    if (input$language == "de") {
      title_text <- "Reset bestätigen"
      body_text <- "Sind Sie sicher, dass Sie zurücksetzen möchten? Alle Antworten gehen verloren."
      confirm_text <- "Ja, zurücksetzen"
      cancel_text <- "Abbrechen"
      pdf_text <- "PDF Bericht herunterladen"
    } else {
      title_text <- "Confirm Reset"
      body_text <- "Are you sure you want to reset? All responses will be lost."
      confirm_text <- "Yes, Reset"
      cancel_text <- "Cancel"
      pdf_text <- "Download PDF Report"
    }

    showModal(modalDialog(
      title = title_text,
      body_text,
      footer = tagList(
        actionButton("confirm_reset", confirm_text, class = "btn-danger"),
        actionButton("modal_pdf", pdf_text, class = "btn-primary"),
        modalButton(cancel_text)
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_reset, {
    removeModal()

    on.exit(
      nav_select("page", "input")
    )

    session$reload()
  })

  # Have modal pdf button trigger the main PDF functionality
  observeEvent(input$modal_pdf, {
    # Simulate a click on the main PDF report button
    shinyjs::click("download_pdf")
    removeModal()
  })

  output$information <- renderUI({
    page_fillable(
      card(
        style = "margin-bottom: 20px;",
        includeMarkdown("README.md")
      )
    )
  })

  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("report-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Get the current values from your reactive data frames
      questions <- questions_data()
      responses <- responses()
      group_summary <- group_summary()
      patient_data <- reactiveValuesToList(patient_data)
      report_language <- input$language

      # Set up parameters to pass to Quarto
      params <- list(
        questions = questions,
        responses = responses |> dplyr::select(!group) |> distinct(id, .keep_all = TRUE),
        group_summary = group_summary,
        patient = patient_data,
        date = Sys.Date(),
        report_language = report_language
      )

      notification_text <- ifelse(report_language == "de",
        "Bericht wird erzeugt...",
        "Report is being generated..."
      )

      id <- showNotification(
        ui = notification_text,
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
      on.exit(removeNotification(id), add = TRUE)

      # Set working directory to temp directory
      oldwd <- setwd(tempDir)
      on.exit(setwd(oldwd), add = TRUE)

      # Render the report
      quarto::quarto_render(
        input = "template.qmd", # call this file in the temporary directory
        output_file = "report.pdf",
        execute_params = params,
        output_format = "pdf"
      )
      # Copy the rendered file to the destination
      file.copy(file.path(tempDir, "report.pdf"), file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)
