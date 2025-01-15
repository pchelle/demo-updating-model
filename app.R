# Shiny app to record weight, height, age, sex and country
# store and assess if values are aligned with a pop model
library(tidyverse)
library(shiny)
library(bslib)
library(shinyWidgets)
library(plotly)

countries <- read.csv("www/countries.csv") |> arrange(Country)
country_flags <- lapply(
  1:nrow(countries),
  function(country_row) {
    HTML(paste(
      tags$img(src = countries$FlagURL[country_row], width = 20, height = 15),
      countries$Country[country_row]
    ))
  }
)
# Load regression model based on NHANES data
model_data <- read.csv("www/model_data.csv")
weight_model <- lm(
  log(weight) ~ splines::bs(age, df = 10) + sex,
  data = model_data
)
height_model <- lm(
  log(height) ~ splines::bs(age, df = 10) + sex,
  data = model_data
)

pred_age <- seq(2.0, 80, 0.5)
input_data <- bind_rows(
  data.frame(age = pred_age, sex = "Male"),
  data.frame(age = pred_age, sex = "Female")
)
pred_data <- bind_cols(
  input_data,
  weight = exp(predict(weight_model, newdata = input_data)),
  height = exp(predict(height_model, newdata = input_data))
) |>
  mutate(
    weight_min = weight * exp(1.96 * sd(weight_model$residuals)),
    weight_max = weight * exp(-1.96 * sd(weight_model$residuals)),
    height_min = height * exp(1.96 * sd(height_model$residuals)),
    height_max = height * exp(-1.96 * sd(height_model$residuals))
  )

# UI ----
ui <- page_sidebar(
  title = "Demographics",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    numericInputIcon(
      "age", "Age [yrs]",
      icon = icon("id-card"),
      value = 18,
      min = 0, max = 105, step = 0.5
    ),
    numericInputIcon(
      "weight", "Body Weight [kg]",
      icon = icon("weight-hanging"),
      value = 60,
      min = 1, max = 300, step = 0.1
    ),
    numericInputIcon(
      "height", "Height [cm]",
      icon = icon("ruler"),
      value = 170,
      min = 20, max = 250, step = 0.1
    ),
    pickerInput(
      "sex", span(icon("children"), "Sex"),
      choices = c("Male", "Female"),
      choicesOpt = list(icon = c("fa-mars", "fa-venus")),
      options = list(`icon-base` = "fas")
    ),
    pickerInput(
      "country", span(icon("flag"), "Country"),
      multiple = FALSE,
      options = list(`live-search` = TRUE),
      choices = countries$Country,
      choicesOpt = list(content = country_flags)
    ),
    actionButton("submit", "Submit", icon = icon("arrow-rotate-right"))
  ),
  layout_column_wrap(
    card(plotlyOutput("weight_in_country")),
    card(plotlyOutput("height_in_country"))
  ),
  verbatimTextOutput("test"),
  card(tableOutput("input_table"))
)

server <- function(input, output, session) {
  output$test <- renderPrint({
    paste(list.files(), collapse = " | ")
  })
  get_bmi <- reactive({
    input$weight / (input$height / 100)^2
  })
  get_ffm <- reactive({
    age_effect <- ifelse(
      input$sex %in% "Female",
      (1.11 + ((1 - 1.11) / (1 + (input$age / 7.1)^-1.1))),
      (0.88 + ((1 - 0.88) / (1 + (input$age / 13.4)^-12.7)))
    )
    ffm <- age_effect * ifelse(
      input$sex %in% "Female",
      9270 * input$weight / (8780 + (244 * get_bmi())),
      9270 * input$weight / (6680 + (216 * get_bmi()))
    )
    return(ffm)
  })
  get_input_data <- reactive({
    data.frame(
      age = input$age,
      weight = input$weight,
      height = input$height,
      sex = input$sex,
      country = input$country
    )
  })

  output$input_table <- renderTable(
    {
      weight_perc <- 100 * pnorm(
        log(input$weight),
        mean = as.numeric(predict(
          weight_model, 
          newdata = get_input_data()
          )),
        sd = sd(weight_model$residuals)
      )
      height_perc <- 100 * pnorm(
        log(input$height),
        mean = as.numeric(predict(
          height_model, 
          newdata = get_input_data()
        )),
        sd = sd(weight_model$residuals)
      )
      data.frame(
        "BMI" = paste(round(get_bmi(), 2), "kg/m<sup>2</sup>"),
        "FFM" = paste(round(get_ffm(), 2), "kg"),
        "Weight Percentile" = paste(round(weight_perc, 2), "%"),
        "Height Percentile" = paste(round(height_perc, 2), "%"),
        check.names = FALSE
      )
    },
    sanitize.text.function = identity
  )

  output$weight_in_country <- renderPlotly({
    # Plot Average Curve of weight vs Age and Sex in selected country
    ggplot(pred_data, aes(x = age, y = weight, color = sex, fill = sex)) +
      theme_bw() +
      geom_ribbon(aes(ymin = weight_min, ymax = weight_max), alpha = 0.2) +
      geom_line() +
      geom_point(data = get_input_data()) +
      scale_color_brewer(palette = "Set1") +
      scale_fill_brewer(palette = "Set1") +
      labs(x = "Age [yrs]", y = "Weight [kg]")
  })

  output$height_in_country <- renderPlotly({
    # Plot Average Curve of weight vs Age and Sex in selected country
    ggplot(pred_data, aes(x = age, y = height, color = sex, fill = sex)) +
      theme_bw() +
      geom_ribbon(aes(ymin = height_min, ymax = height_max), alpha = 0.2) +
      geom_line() +
      geom_point(data = get_input_data()) +
      scale_color_brewer(palette = "Set1") +
      scale_fill_brewer(palette = "Set1") +
      labs(x = "Age [yrs]", y = "Height [cm]")
  })

  observeEvent(input$submit, {
    show_toast(
      title = "Data added to database",
      type = "success"
    )
    # Store input data
    input_data <- get_input_data()
    write.table(
      input_data, 
      "www/new_data.csv", 
      append = TRUE,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE
      )
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
