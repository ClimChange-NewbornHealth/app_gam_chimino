library(shiny)
library(mgcv)
library(bslib)
library(tidyverse)
library(readxl)
library(DT)
library(janitor)
library(easystats)
library(patchwork)

# Cargar y preprocesar los datos antes de definir la UI
diccionario <- read_xlsx("input/excel/dict_work_dataset.xlsx") %>% 
    janitor::clean_names() %>% 
    arrange(nombre_de_la_variable) %>% 
    dplyr::select(-c(id, nombre_de_la_variable_old))

no_incluir <- diccionario %>% 
    dplyr::filter(inclusion == "n") %>% 
    pull(nombre_de_la_variable)

extra_variables <- read_csv("input/csv/work_data_chim_new_variables.csv") %>% 
  dplyr::select(-c(1, date_nac, date_start, date_start_week1)) %>% 
  distinct(folio, .keep_all = T)

# Cargar datos base
datos_embarazos_base <- read_csv("input/csv/work_data_chim.csv") %>% 
  left_join(extra_variables) %>% 
    mutate(across(
        -c(folio, 
           birthweight,
           edad_madre,
           eg_parto,
           n_hogar,
           eg_ingreso,
           w_gain,
           eg_control2,
           vuln,
           weeks, 
           date_start_week,
           date_start_month,
           date_start_year), 
        as.factor),
        fecha_nac = mdy(fecha_nac),
    ) %>% 
    dplyr::filter(eg_parto > 36,
                  gc_ex == 1) %>% 
    dplyr::select(-all_of(no_incluir)) %>% 
  distinct(folio, .keep_all = T)

datos_embarazos_con_na <- datos_embarazos_base

datos_embarazos_con_na_count <- datos_embarazos_con_na %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "NA_count") %>% 
    arrange(desc(NA_count))

filas_antes_na <- nrow(datos_embarazos_con_na)

datos_temp <- read_xlsx("input/excel/temperatura_completo_embarazo__cohorte.xlsx")
temp_vars <- names(datos_temp)
temp_vars <- temp_vars[temp_vars != 'folio']

datos_ndvi <- read_xlsx("input/excel/ndvi_agregado_embarazo.xlsx")  %>% 
  distinct(folio, .keep_all = T)

# Definir la UI
ui <- page_sidebar(
    
    tags$style(HTML("
        .custom-hr {
          border: 2px solid black;
          margin-top: 3px;
          margin-bottom: 3px;
        }
      ")),
    
    title = h1(strong("Explorador de Modelos GAM")),
    sidebar = sidebar(
        width = "350px",  # Aumenta el ancho del sidebar aquí
        h4("Seleccionar opciones"),
        hr(class = "custom-hr"),
        HTML("<strong>Variable de Respuesta:</strong> birthweight"),
        hr(class = "custom-hr"),
        selectInput("temp_var", 
                    HTML("<strong>Seleccionar Variable de Temperatura:</strong> <br> <br>
                         Ejemplo de variable y su interpretación: <br> <br>
                         <em>tmin_min_100_emb_sd</em> <br> <br>
                         - <em>tmin</em>: temperatura mínima (del raster de tmin) <br> <br>
                         - <em>min</em>: valor mínimo dentro del buffer <br> <br>
                         - <em>100</em>: buffer de 100 metros <br> <br>
                         - <em>emb</em> y <em>sd</em>: desviación estándar durante el embarazo <br> <br> 
                         "),
                    choices = c("None", temp_vars),
                    selected = "None",
                    width = "100%"),  # Asegura que el selectInput ocupe todo el ancho disponible
        hr(class = "custom-hr"),
        checkboxGroupInput("predictors", HTML("<strong>Variables Predictoras:</strong>"),
                           choices = NULL, selected = NULL),
        hr(class = "custom-hr"),
        HTML("<strong>Familia de Distribución:</strong> gaussian"),
        hr(class = "custom-hr"),
        numericInput("k", HTML("<strong>Dimensión de la Base (k):</strong> <br> k controla la flexibilidad de los términos de suavizado en el modelo.
        <br> - Un valor más bajo de k resulta en una curva más simple (menos flexible).
        <br> - Un valor más alto de k permite que la curva sea más flexible y capture más variaciones en los datos."), 
                     value = 3, min = 3, max = 50),
        hr(class = "custom-hr"),
        actionButton("fit", "Ajustar GAM", class = "btn-primary")
    ),
    layout_columns(
        fill = FALSE,
        value_box(
            title = "R-cuadrado",
            value = textOutput("rsq"),
            theme = "primary"
        ),
        value_box(
            title = "Puntaje GCV",
            value = textOutput("gcv"),
            theme = "secondary"
        ),
        value_box(
            title = "AIC",
            value = textOutput("aic"),
            theme = "info"
        )
    ),
    navset_card_tab(
        nav_panel(
            "Variable Dictionary",
            DTOutput("table_diccionario")
        ),
        nav_panel(
            "NAs inspection",
            HTML("<h2><strong>NAs antes y después de eliminarlas:</strong></h2>"),
            HTML("<strong>Filas antes de eliminar NAs:</strong>"),
            textOutput("filas_antes_na"),
            HTML("<strong>Filas después de eliminar NAs:</strong>"),
            textOutput("filas_despues_na"),
            HTML("<h2><strong>Cantidad de NAs por variable:</strong></h2>"),
            DTOutput("table_na")
        ),
        nav_panel(
            "Response Variable Analysis",
            h4(HTML("<strong>Gráfico de Densidad de birthweight</strong>")),
            plotOutput("birthweight_density"),
            h4(HTML("<strong>Estadísticas Resumen de birthweight</strong>")),
            DTOutput("birthweight_summary_table")
        ),
        nav_panel(
            "Exploratory Analysis",
            h4(HTML("<strong>Gráficos de Dispersión (birthweight vs Variables Numéricas)</strong>")),
            div(
                uiOutput("scatter_plots"),
                style = "max-height: 80vh; overflow-y: auto; padding: 10px; border: 1px solid #ccc;"
            ),
            h4(HTML("<strong>Diagramas de Caja (birthweight vs Variables Categóricas)</strong>")),
            div(
                uiOutput("boxplots"),
                style = "max-height: 80vh; overflow-y: auto; padding: 10px; border: 1px solid #ccc;"
            ),
            h4(HTML("<strong>Gráficos de Densidad (Variables Numéricas)</strong>")),
            div(
                uiOutput("density_plots"),
                style = "max-height: 80vh; overflow-y: auto; padding: 10px; border: 1px solid #ccc;"
            )
        ),
        nav_panel(
            "Model Diagnostics",
            plotOutput("diagnostic_plots", height = "600px")
        ),
        nav_panel(
            "Partial Effects",
            plotOutput("partial_effects", height = "600px")
        ),
        nav_panel(
            "Model Performance",
            plotOutput("check_model_plot", height = "600px")
        ),
        nav_panel(
            "Summary",
            verbatimTextOutput("model_summary")
        )
    )
)

# Definir el servidor
server <- function(input, output, session) {
    # Expresión reactiva para la variable de temperatura seleccionada
    datos_temp_selected <- reactive({
        if (!is.null(input$temp_var) && input$temp_var != "None") {
            datos_temp %>%
                dplyr::select(folio, all_of(input$temp_var))
        } else {
            NULL
        }
    })
    
    # Expresión reactiva para combinar los datos
    datos_embarazos <- reactive({
        datos <- datos_embarazos_base %>% 
            {if (!is.null(datos_temp_selected())) left_join(., datos_temp_selected(), by = 'folio') else .} %>% 
            left_join(datos_ndvi, by = 'folio') %>% 
            na.omit()
        datos
    })
    
    # Actualizar las opciones de variables predictoras cuando cambien los datos
    observeEvent(datos_embarazos(), {
        predictors_choices <- datos_embarazos() %>%
            select(-c(folio, birthweight, fecha_nac)) %>%
            names() %>%
            sort()
        
        # Mantener las variables seleccionadas previamente que aún están disponibles
        current_selected <- input$predictors
        new_selected <- current_selected[current_selected %in% predictors_choices]
        
        updateCheckboxGroupInput(session, "predictors",
                                 choices = predictors_choices,
                                 selected = new_selected)
    })
    
    # Agregar o eliminar la variable de temperatura de las variables seleccionadas
    observeEvent(input$temp_var, {
        if (!is.null(input$temp_var) && input$temp_var != "None") {
            temp_var_selected <- input$temp_var
            
            # Agregar la variable de temperatura a las seleccionadas si no está ya incluida
            if (!(temp_var_selected %in% input$predictors)) {
                new_selected <- c(input$predictors, temp_var_selected)
                updateCheckboxGroupInput(session, "predictors",
                                         selected = new_selected)
            }
        } else {
            # Si se selecciona 'None', eliminar cualquier variable de temperatura de las seleccionadas
            temp_vars_in_selected <- intersect(input$predictors, temp_vars)
            if (length(temp_vars_in_selected) > 0) {
                new_selected <- setdiff(input$predictors, temp_vars_in_selected)
                updateCheckboxGroupInput(session, "predictors",
                                         selected = new_selected)
            }
        }
    })
    
    # Expresiones reactivas para variables numéricas y categóricas
    numeric_vars <- reactive({
        datos_embarazos() %>%
            select(where(is.numeric), -birthweight) %>%
            names()
    })
    categorical_vars <- reactive({
        datos_embarazos() %>%
            select(where(is.factor)) %>%
            names()
    })
    
    # Expresión reactiva para filas después de eliminar NAs
    filas_despues_na <- reactive({
        nrow(datos_embarazos())
    })
    
    # Mostrar filas antes y después de eliminar NAs
    output$filas_antes_na <- renderText({
        filas_antes_na
    })
    output$filas_despues_na <- renderText({
        filas_despues_na()
    })
    
    # Gráficos exploratorios
    output$scatter_plots <- renderUI({
        vars <- selected_vars()$numeric
        req(vars)  
        plot_list <- lapply(vars, function(var) {
            plotname <- paste0("scatter_", var)
            plotOutput(plotname, height = "300px")
        })
        do.call(tagList, plot_list)
    })
    
    observe({
        vars <- selected_vars()$numeric
        lapply(vars, function(var) {
            output[[paste0("scatter_", var)]] <- renderPlot({
                ggplot(datos_embarazos(), aes_string(x = var, y = "birthweight")) +
                    geom_point(alpha = 0.7) +
                    labs(title = paste("Gráfico de Dispersión de birthweight vs", var),
                         x = var, y = "birthweight") +
                    geom_smooth(se = FALSE) +
                    theme_bw() +
                    theme(legend.background = element_rect(color = "black"),
                          plot.title = element_text(face="bold", size=14, hjust = 0.5),
                          panel.grid.minor = element_blank(),
                          axis.title.y = element_text(face = "bold"),
                          axis.title.x = element_text(face = "bold"),
                          axis.text.y = element_text(face="bold", size=11, color = "black"),
                          axis.text.x = element_text(face="bold", size=11,color = "black"))
            })
        })
    })
    
    output$boxplots <- renderUI({
        vars <- selected_vars()$categorical
        req(vars)
        plot_list <- lapply(vars, function(var) {
            plotname <- paste0("boxplot_", var)
            plotOutput(plotname, height = "300px")
        })
        do.call(tagList, plot_list)
    })
    
    observe({
        vars <- selected_vars()$categorical
        lapply(vars, function(var) {
            output[[paste0("boxplot_", var)]] <- renderPlot({
                ggplot(datos_embarazos(), aes_string(x = var, y = "birthweight")) +
                    geom_boxplot() +
                    theme_bw() +
                    theme(legend.background = element_rect(color = "black"),
                          plot.title = element_text(face="bold", size=14, hjust = 0.5),
                          panel.grid.minor = element_blank(),
                          axis.title.y = element_text(face = "bold"),
                          axis.title.x = element_text(face = "bold"),
                          axis.text.y = element_text(face="bold", size=11, color = "black"),
                          axis.text.x = element_text(face="bold", size=11, color = "black"))
            })
        })
    })
    
    output$density_plots <- renderUI({
        vars <- selected_vars()$numeric
        req(vars)
        plot_list <- lapply(vars, function(var) {
            plotname <- paste0("density_", var)
            plotOutput(plotname, height = "300px")
        })
        do.call(tagList, plot_list)
    })
    
    observe({
        vars <- selected_vars()$numeric
        lapply(vars, function(var) {
            output[[paste0("density_", var)]] <- renderPlot({
                ggplot(datos_embarazos(), aes_string(x = var)) +
                    geom_density(fill = "blue", alpha = 0.5) +
                    theme_bw() +
                    theme(legend.background = element_rect(color = "black"),
                          plot.title = element_text(face="bold", size=14, hjust = 0.5),
                          panel.grid.minor = element_blank(),
                          axis.title.y = element_text(face = "bold"),
                          axis.title.x = element_text(face = "bold"),
                          axis.text.y = element_text(face="bold", size=11, color = "black"),
                          axis.text.x = element_text(face="bold", size=11, color = "black"))
            })
        })
    })
    
    # Filtrar variables seleccionadas
    selected_vars <- reactive({
        req(input$predictors)
        selected <- input$predictors
        list(
            numeric = selected[selected %in% numeric_vars()],
            categorical = selected[selected %in% categorical_vars()]
        )
    })
    
    # Validar datos para el modelo
    validate_data <- reactive({
        req(input$predictors)
        datos <- datos_embarazos() %>%
            select(all_of(c("birthweight", input$predictors))) %>%
            na.omit()
        validate(
            need(nrow(datos) > 0, "Las variables seleccionadas contienen solo valores NA.")
        )
        datos
    })
    
    # Construir fórmula dinámica
    formula_reactive <- reactive({
        vars <- selected_vars()
        terms <- c()
        if (length(vars$numeric) > 0) {
            numeric_terms <- paste0("s(", vars$numeric, ", k=", input$k, ")")
            terms <- c(terms, numeric_terms)
        }
        if (length(vars$categorical) > 0) {
            terms <- c(terms, vars$categorical)
        }
        validate(
            need(length(terms) > 0, "Por favor, selecciona al menos una variable predictora.")
        )
        formula_text <- paste("birthweight ~", paste(terms, collapse = " + "))
        as.formula(formula_text)
    })
    
    # Ajustar modelo GAM
    gam_model <- eventReactive(input$fit, {
        req(formula_reactive())
        datos <- validate_data()
        gam(formula_reactive(), data = datos, family = gaussian)
    })
    
    output$rsq <- renderText({
        req(gam_model())
        round(summary(gam_model())$r.sq, 3)
    })
    
    output$gcv <- renderText({
        req(gam_model())
        round(gam_model()$gcv.ubre, 3)
    })
    
    output$aic <- renderText({
        req(gam_model())
        round(AIC(gam_model()), 1)
    })
    
    output$diagnostic_plots <- renderPlot({
        req(gam_model())
        par(mfrow = c(2, 2))
        gam.check(gam_model())
    })
    
    output$partial_effects <- renderPlot({
        req(gam_model())
        plot(gam_model(), pages = 1)
    })
    
    output$model_summary <- renderPrint({
        req(gam_model())
        summary(gam_model())
    })
    
    output$table_diccionario <- renderDT({
        datatable(diccionario, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$table_na <- renderDT({
        datatable(datos_embarazos_con_na_count, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # Gráfico de densidad de birthweight
    output$birthweight_density <- renderPlot({
        ggplot(datos_embarazos(), aes(x = birthweight)) +
            geom_density(fill = "blue", alpha = 0.5) +
            labs(
                title = "Gráfico de Densidad de Birthweight",
                x = "Birthweight",
                y = "Densidad"
            ) +
            theme_bw() +
            theme(legend.background = element_rect(color = "black"),
                  plot.title = element_text(face="bold", size=14, hjust = 0.5),
                  panel.grid.minor = element_blank(),
                  axis.title.y = element_text(face = "bold"),
                  axis.title.x = element_text(face = "bold"),
                  axis.text.y = element_text(face="bold", size=11, color = "black"),
                  axis.text.x = element_text(face="bold", size=11, color = "black"))
    })
    
    # Tabla de estadísticas resumen de birthweight
    output$birthweight_summary_table <- renderDT({
        summary_stats <- datos_embarazos() %>%
            summarise(
                Min = min(birthweight, na.rm = TRUE),
                Mean = round(mean(birthweight, na.rm = TRUE), 2),
                Median = median(birthweight, na.rm = TRUE),
                Max = max(birthweight, na.rm = TRUE),
                SD = round(sd(birthweight, na.rm = TRUE), 2)
            )
        datatable(
            summary_stats,
            options = list(
                paging = FALSE,
                searching = FALSE,
                info = FALSE
            )
        )
    })
    
    output$check_model_plot <- renderPlot({
        req(gam_model())  # Asegurarse de que el modelo esté ajustado
        check_model(gam_model())  # Generar gráficos de diagnóstico
    })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
