# Load Packages
library(shiny)
library(ggplot2)

# Prepare Data
data2 <- data.frame(
  Estudiante = as.factor(1:30),
  Rendimiento = c(3.3, 3.3, 3.3, 4.2, 3.8, 4.2, 3.8, 3.8, 4.6, 3.3, 4.2, 5, 5, 4.6, 3.3, 1.7, 2.9, 4.2, 2.5, 2.9, 2.5, 2.9, 3.8, 4.2, 2.9, 4.2, 2.5, 2.5, 3.8, 3.8),
  GeneroF = as.numeric(c(1,1,0,1,1,1,1,0,0,0,1,1,0,0,1,0,0,1,0,0,0,1,0,0,1,1,1,0,1,0)),
  Exdig = as.numeric(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)),
  Edad = as.numeric(c(21, 19, 20, 22, 23, 21, 20, 22, 20, 21, 20, 21, 17, 21, 19, 23, 18, 19, 23, 19, 21, 18, 22, 20, 23, 21, 19, 17, 18, 23)),
  Estrato = as.numeric(c(5,4,4,4,5,5,5,6,3,6,6,6,6,4,5,4,6,6,6,4,4,4,4,5,3,6,3,4,4,5))
)

# Linear Model
modelo <- lm(Rendimiento ~ Exdig + GeneroF + Edad + Estrato, data = data2)

# Shiny UI
ui <- fluidPage(
  titlePanel("Regresion Linear Multiple"),
  sidebarLayout(
    sidebarPanel(
      numericInput("genero_input", "Genero (F = 1, M = 0):", value = 1, min = 0, max = 1),
      numericInput("exdig_input", "Tipo de examen (Exdig = 1, Otros = 0):", value = 1, min = 0, max = 1),
      numericInput("edad_input", "Edad:", value = 23, min = 17, max = 23),
      numericInput("estrato_input", "Estrato socioeconomico:", value = 6, min = 3, max = 6),
      actionButton("predict_button", "Predecir"),
      hr(),
      h4("Model Coefficients:"),
      verbatimTextOutput("coefficients_text")
    ),
    mainPanel(
      style = "background-color: #C0B8B8;",  # Background color change
      plotOutput("regression_plot"),
      h4("Rendimiento predecido (Y) :"),
      verbatimTextOutput("Rendimiento_predecido")
    )
  )
)

# Shiny server
server <- function(input, output) {
  # Predict function
  predict_knowledge <- function(GeneroF, Exdig, Edad, Estrato) {
    new_data <- data.frame(GeneroF = GeneroF, Exdig = Exdig, Edad = Edad, Estrato = Estrato)
    predicted_knowledge <- predict(modelo, newdata = new_data)
    return(predicted_knowledge)
  }
  
  # Render combined plot for all variables
  output$regression_plot <- renderPlot({
    ggplot(data2, aes(x = GeneroF, y = Rendimiento)) +
      geom_point(aes(color = "GeneroF"), size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
      geom_point(aes(x = Exdig , color = "Exdig"), size = 3) +
      geom_point(aes(x = Edad, color = "Edad"), size = 3) +
      geom_point(aes(x = Estrato , color = "Estrato"), size = 3) +
      labs(title = "Multiple Linear Regression",
           x = "GeneroF, Exdig, Edad y Estrato",
           y = "Rendimiento (Y)",
           color = "Variable") +
      theme_minimal()
  })
  
  # Render coefficients
  output$coefficients_text <- renderText({
    paste("Intercepto:", round(coef(modelo)[1], 4),
          "Genero:", round(coef(modelo)[2], 4),
          "Tipo de examen:", round(coef(modelo)[3], 4),
          "Edad:", round(coef(modelo)[4], 4),
          "Estrato:", round(coef(modelo)[5], 4))
  })
  
  # Event handler for prediction button
  observeEvent(input$predict_button, {
    genero_input <- input$genero_input
    exdig_input <- input$exdig_input
    edad_input <- input$edad_input
    estrato_input <- input$estrato_input
    rendimiento_predecido <- predict_knowledge(genero_input, exdig_input, edad_input, estrato_input)
    output$Rendimiento_predecido <- renderText({
      paste("Rendimiento predecido (Y):", round(rendimiento_predecido, 2))
    })
  })
}

# Run Shiny app
shinyApp(ui, server)