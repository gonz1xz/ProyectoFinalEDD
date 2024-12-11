#setwd("C:/Users/Espin/Downloads/Yo/Uni/Explotacion de datos/Final")
##setwd("C:/Users/gonzi/OneDrive/Escritorio/ProyectoFinal/ProyectoFinal")
##rm(list=ls())
##gc()

# Proyecto Final con Shiny

# Integrantes:
# - Gonzalo Castellino
# - Ariel Espinoza

# https://www.kaggle.com/datasets/shreyasg23/life-expectancy-averaged-dataset


############################################# Librerias #############################################


library(shiny)
library(shinythemes)
library(shinyjs)
library(readr)
library(tidyverse)
library(caret)
library(rpart)  
library(rpart.plot)
library(skimr)
library(caret)
library(dplyr)
library(lubridate)
library(bslib)
library(corrplot)
library(ggcorrplot)
library(sqldf)
library(shinyjs)
library(car)

# Evitamos notacion cientifica
options(scipen = 6)


# Preparacion dataset


# Lectura dataset
data <- read_csv("Life-Expectancy-Data-Averaged.csv")

names(data) <- c(
  "Pais",                     # "Country"
  "Region",                   # "Region"
  "Anio",                     # "Year"
  "Muertes_infantiles",       # "Infant_deaths"
  "Muertes_menores_de_cinco", # "Under_five_deaths"
  "Mortalidad_adulta",        # "Adult_mortality"
  "Consumo_alcohol",          # "Alcohol_consumption"
  "Hepatitis_B",              # "Hepatitis_B"
  "Sarampion",                # "Measles"
  "IMC",                      # "BMI" Indice de Masa Corporal promedio de la poblacion
  "Polio",                    # "Polio" Porcentaje de la poblacion vacunada contra la Polio.
  "Difteria",                 # "Diphtheria" Porcentaje de inmunización cubierto contra la Difteria.
  "Incidencias_HIV",          # "Incidents_HIV"  cant de nuevas infecciones cada 1000 población no infectada.
  "PIB_per_capita",           # "GDP_per_capita"
  "Poblacion_millones",       # "Population_mln"
  "Delgadez_diez_diecinueve", # "Thinness_ten_nineteen_years"
  "Delgadez_cinco_nueve",     # "Thinness_five_nine_years"
  "Escolaridad",              # "Schooling"
  "Estado_economico",         # "Economy_status" 0 = pais en via de desarrollo. 1 = pais desarrollado.
  "Esperanza_de_vida"         # "Life_expectancy"
)

colnames(data)

data$Region <- as.factor(data$Region)

querySelectRegiones <- "SELECT DISTINCT Region FROM data"
continentes_unicos <- sqldf(querySelectRegiones)
data$Estado_economico <- as.factor(data$Estado_economico)

#Eliminamos anio porque se asume que es de 2007
data <- data %>%
  select(-Anio)

#Seleccionamos solo columnas numericas
columnas_numericas <- colnames(data)[sapply(data, is.numeric)]

# Correlaciones
dataNumeric <- data[sapply(data, is.numeric)]
correlaciones <- cor(dataNumeric)

######################################## interfaz de usuario (UI) ###########################


# Interfaz de usuario (UI)
ui <- navbarPage(
  title = "Dashboard de Análisis",
  useShinyjs(),
  # Pestaña 1: Análisis Exploratorio
  tabPanel(
    "Análisis Univariado",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "columna",
          label = "Selecciona una Variable:",
          choices = columnas_numericas # Solo variables numéricas
        ),
        uiOutput("binwidthBar"), 
        selectInput(
          inputId = "continente",
          label = "Seleccione una Region:",
          choices = rbind(continentes_unicos, data.frame(Region = "Todos"))
        ),
      ),
      mainPanel(
        fluidRow(
          column(6, plotOutput(outputId = "barplot", height = "300px")), # Histograma
          column(6, plotOutput(outputId = "boxplot", height = "300px"))    # Boxplot
        ),
        fluidRow(
          column(6, plotOutput(outputId = "qqplot", height = "300px")), #qqplot
          column(6, verbatimTextOutput(outputId = "summary")) # summary
          
        )
      )
    )
  ),
  
  # Pestaña 2: Analisis Exploratorio Bivariado
  tabPanel(
    "Análisis Bivariado",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "var1",
          label = "Variable eje X:",
          choices = columnas_numericas
        ),
        selectInput(
          inputId = "var2",
          label = "Variable eje Y:",
          choices = columnas_numericas
        ),
      ),
      mainPanel(
        fluidRow(
          column(8, plotOutput(outputId = "scatterplot", height = "300px")),
          column(10, plotOutput(outputId = "corrplot", height = "800px")),
          
        ),
      )
    )
  ),
  
  
  # Pestaña 3: Regresión Lineal
  tabPanel(
    "Regresión Lineal",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            id = "regressionTabs",
            # Tab para selección de modelo
            tabPanel(
              "Selección de modelo",
              selectInput(
                inputId = "regDepen",
                label = "Elegir variable Dependiente:",
                choices = columnas_numericas
              ), 
              checkboxGroupInput(
                inputId = "items",
                label = "Variables independientes del modelo:",
                choices = NULL
              ),
              actionButton(
                inputId = "calReg",
                label = "Calcular Regresión"
              )
            ),
            # Tab para tests
            tabPanel(
              "Tests",
              h4("Análisis de Supuestos y tests"),
              verbatimTextOutput("testResults")
            ),
            # Tab para predicción
            tabPanel(
              "Predicción",
              h4("Ingresar valores para predicción:"),
              uiOutput("inputPredictionFields"),
              actionButton("predictButton", "Predecir")
            )
          )
        ),
        mainPanel(
          h3("Resultados"),
          conditionalPanel(
            condition = "input.regressionTabs == 'Selección de modelo'",
            textOutput("regFormula"),
            verbatimTextOutput("regSummary"),
            plotOutput("regPlot")
          ),
          conditionalPanel(
            condition = "input.regressionTabs == 'Tests'",
            h4("Criterio de Información de Akaike"),
            verbatimTextOutput("aicTest"),
            h4("VIF: Verificacion multicolinealidad"),
            verbatimTextOutput("vifTest"),
            h4("Test de Shapiro-Wilk"),
            verbatimTextOutput("saphiroTest"),
            h4("Distancia de Cooks"),
            plotOutput("cooksDistance"),
            h4("Analisis de residuos"),
            plotOutput("residuals")
          ),
          conditionalPanel(
            condition = "input.regressionTabs == 'Predicción'",
            h4("Resultado de la predicción"),
            textOutput("predictionResult")
          )
        )
      )
    )
  )
  
)



######################################## Definición de la lógica del servidor ####################################


server <- function(input, output, session) {
  
  # Sidebar checkbox
  observe({
    indepenVars <- setdiff(columnas_numericas, input$regDepen)
    updateCheckboxGroupInput(
      session,
      "items",
      choices = indepenVars,
      selected = NULL
    )
  })
  
  
  
  # Filtro para regiones
  filteredData <- reactive({
    if (input$continente == "Todos") {
      data 
    } else {
      sqldf(sprintf("SELECT * FROM data WHERE Region = '%s'", input$continente))
    } 
  })
  
  # Agrega el slider para la longitud de los intervalos  
  output$binwidthBar <- renderUI({
    sliderInput("bins", "Modificar intervalo:",
                min = 1, max = 50, value = 30) 
  })  
  
  
  
  output$barplot <- renderPlot({
    req(input$columna)
    datos <- filteredData()[[input$columna]]
    
    if (is.null(datos) || length(datos) == 0) {
      return(NULL)
    }
    
    if (is.numeric(datos)) {
      binwidth <- (max(datos, na.rm = TRUE) - min(datos, na.rm = TRUE)) / input$bins
      ggplot(filteredData(), aes_string(x = input$columna)) +
        geom_histogram(binwidth = binwidth, fill = "blue", color = "white") +
        labs(title = paste("Histograma de", input$columna), x = input$columna, y = "Frecuencia") +
        theme_minimal()
    } else {
      ggplot(filteredData(), aes_string(x = input$columna)) +
        geom_bar(fill = "blue", color = "black") +
        labs(title = paste("Frecuencia de", input$columna), x = input$columna, y = "Frecuencia") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$summary <- renderPrint({
    if(!is.null(input$columna) && input$columna %in% colnames(data)) {
      stats <- summary(filteredData()[[input$columna]])
      stats_1 <- stats[1:3]
      stats_2 <- stats[4:6]
      list(firstRow = stats_1, secondRow = stats_2)
    } else
      "Selecciona una columna válida para ver su resumen."
  })
  
  output$boxplot <- renderPlot({
    columna_seleccionada <- input$columna 
    boxplot(
      filteredData()[[columna_seleccionada]],
      main = paste("Boxplot de", columna_seleccionada),
      col = "skyblue",
      border = "darkblue",
      ylab = "Valores"
    )
  })
  
  output$corrplot <- renderPlot({
    correlaciones <- cor(dataNumeric)
    corrplot(correlaciones,method = "number", ,hc.order = TRUE, outline.color = "white")
    
  })
  
  output$qqplot <- renderPlot({
    if(!is.null(input$columna) && input$columna %in% colnames(data)) {
      columna_datos <- as.numeric(filteredData()[[input$columna]])
      columna_datos <- na.omit(columna_datos)
      
      qqnorm(columna_datos, main= paste("Q-Q norm de", input$columna))
      qqline(columna_datos, col = "red")
    } else {
      plot.new()
      text(0.5, 0.5, "Selecciona una columna válida.", cex = 1.5)
    }
  })
  
  output$scatterplot <- renderPlot({
    req(input$var1, input$var2)
    
    ggplot(data,
           aes_string(x = input$var1, y = input$var2)) +
      geom_point() +
      labs(title = paste("Gráfico de Dispersión de", input$var1, "vs", input$var2), x = input$var1, y = input$var2)
  })
  
  
  # Calcula y muestra la fórmula y resultados del modelo
  observeEvent(input$calReg, {
    req(input$regDepen, input$items)
    
    datos_filtrados <- data %>%
      select(all_of(c(input$regDepen, input$items))) %>%
      na.omit()
    
    formula <- as.formula(
      paste(input$regDepen, "~", paste(input$items, collapse = " + "))
    )
    
    modelo <- lm(formula, data = datos_filtrados)
    
    output$regFormula <- renderText({
      paste("Fórmula del modelo:", deparse(formula))
    })
    
    output$regSummary <- renderPrint({
      summary(modelo)
    })
  })
  
  
  ## Prediccion
  
  output$inputPredictionFields <- renderUI({
    req(input$regDepen, input$items)
    
    indepVars <- setdiff(columnas_numericas, input$regDepen)
    
    lapply(input$items, function(var) {
      numericInput(
        inputId = paste0("input_", var),
        label = paste("Valor para", var),
        value = NA
      )
    })
  })
  
  observeEvent(input$predictButton, {
    req(input$regDepen, input$items)

    predValues <- sapply(input$items, function(var) {
      input[[paste0("input_", var)]]
    })
    
    if (any(is.na(predValues))) {
      output$predictionResult <- renderText("Por favor, complete todos los campos.")
      return()
    }
    
    predData <- as.data.frame(as.list(predValues))
    colnames(predData) <- input$items
    
    modelo <- lm(
      as.formula(paste(input$regDepen, "~", paste(input$items, collapse = " + "))),
      data = data
    )
    
    # Realiza la predicción
    pred <- predict(modelo, newdata = predData)
    
    output$predictionResult <- renderText({
      paste("Predicción para", input$regDepen, ":", round(pred, 2))
    })
  })
  
  ## TEST
  
  output$aicTest <- renderPrint({
    req(input$regDepen, input$items)
    
    datos_filtrados <- data %>%
      select(all_of(c(input$regDepen, input$items))) %>%
      na.omit()
    
    formula <- as.formula(
      paste(input$regDepen, "~", paste(input$items, collapse = " + "))
    )
    
    modelo <- lm(formula, data = datos_filtrados)
    
    
    AIC(modelo)
  })
  
  output$vifTest <- renderPrint({
    req(input$regDepen, input$items)
    
    datos_filtrados <- data %>%
      select(all_of(c(input$regDepen, input$items))) %>%
      na.omit()
    
    formula <- as.formula(
      paste(input$regDepen, "~", paste(input$items, collapse = " + "))
    )
    
    modelo <- lm(formula, data = datos_filtrados)
    
    
    vif(modelo)
  })
  
  output$saphiroTest <- renderPrint({
    
    req(input$regDepen, input$items)
    
    datos_filtrados <- data %>%
      select(all_of(c(input$regDepen, input$items))) %>%
      na.omit()
    
    formula <- as.formula(
      paste(input$regDepen, "~", paste(input$items, collapse = " + "))
    )
    
    modelo <- lm(formula, data = datos_filtrados)
    
    residuos <- residuals(modelo)
    
    shapiro.test(residuos)
  })
  
  output$cooksDistance <- renderPlot({
    req(input$regDepen, input$items)
    
    datos_filtrados <- data %>%
      select(all_of(c(input$regDepen, input$items))) %>%
      na.omit()
    
    formula <- as.formula(
      paste(input$regDepen, "~", paste(input$items, collapse = " + "))
    )
    
    modelo <- lm(formula, data = datos_filtrados)
    
    cooks = cooks.distance(modelo)
    plot(cooks)
    abline(a=0.5,b=0)
  })
  
  output$residuals <- renderPlot({
    req(input$regDepen, input$items)
    
    datos_filtrados <- data %>%
      select(all_of(c(input$regDepen, input$items))) %>%
      na.omit()
    
    formula <- as.formula(
      paste(input$regDepen, "~", paste(input$items, collapse = " + "))
    )
    
    modelo <- lm(formula, data = datos_filtrados)
    
    residuos <- residuals(modelo)
    par(mfrow= c(2,2))
    plot(modelo)
    par(mfrow=c(1,1))
    
    
  })
  
  
}


######################################## Shiny App ########################################

shinyApp(ui = ui, server = server)
