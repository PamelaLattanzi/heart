library(shiny)
library(bslib) # For theming with Bootstrap
library(DT) #For rendering data table
library(ggplot2) #For static plotting
library(plotly)

source("R/helpers.R")
source("R/mod_download_plot.R")

heart <- readRDS("data/heart.rds")

ui <- page_sidebar(
  title = tags$span(
    tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px; filter: invert(1);"),
    "Heart Attack Dashboard"
  ),
  theme = bs_theme(bootswatch = "pulse"),
  sidebar = sidebar(
    selectInput(
      inputId = "outcome",
      label = "Outcome:",
      choices = c("All", "Survived", "Died")
    ),
    selectInput(
      inputId = "diagnosis",
      label = "Diagnosis:",
      choices = c("All", sort(unique(as.character(heart$DIAGNOSIS)))),
      selected = "All"
    ),
    selectInput(
      inputId = "drg",
      label = "DRG:",
      choices = c("All", sort(unique(as.character(heart$DRG)))),
      selected = "All"
    ),
    sliderInput(
      inputId = "age_range",
      label = "Age Range:",
      min = min(heart$AGE),
      max = max(heart$AGE),
      value = c(min(heart$AGE), max(heart$AGE))
    ),
    actionButton(
      inputId = "reset",
      label = "Reset",
      # icon = bsicons::bs_icon("arrow-counterclockwise")
      icon = icon("arrow-counterclockwise")
    )
  ),
  
  #------------------------------------------------------------------
  navset_tab(
    nav_panel(
      "Overview",
      layout_column_wrap(
        width = 1/2,
        value_box(
          title = "Female Mortality",
          value = textOutput("f_mortality"),
          theme = "danger",
          showcase = bsicons::bs_icon("gender-female")
        ),
        value_box(
          title = "Male Mortality",
          value = textOutput("m_mortality"),
          theme = "primary",
          showcase = bsicons::bs_icon("gender-male")
        )
      ),
      card(
        card_header("Age Distribution"),
        plotOutput("age_hist"),
        mod_download_plot_ui("dl_age", label = "Download")
      ),
      card(
        card_header("Summary by Sex"),
        DT::dataTableOutput("sex_summary")
      )
    ),
    nav_panel(
      "Explore", 
      plotlyOutput("scatter_plot")
      ),
    
    nav_panel(
      "Length of Stay",
      layout_column_wrap(
        width = 1/2,
        value_box(
          title = "Avg LOS (Female)",
          value = textOutput("f_avg_los"),
          theme = "danger",
          showcase = bsicons::bs_icon("gender-female")
        ),
        value_box(
          title = "Avg LOS (Male)",
          value = textOutput("m_avg_los"),
          theme = "primary",
          showcase = bsicons::bs_icon("gender-male")
        )
      ),
      card(
        card_header("LOS Distribution"),
        plotOutput("los_density"),
        mod_download_plot_ui("dl_los", label = "Download")
      )
    ),
    
    nav_panel(
      "Charges",
      layout_column_wrap(
        width = 1/3,
        value_box(
          title = "Avg Charges (per stay)",
          value = textOutput("avg_charges"),
          theme = "success",
          showcase = bsicons::bs_icon("currency-dollar")
        ),
        value_box(
          title = "Avg Length of Stay",
          value = textOutput("avg_los"),
          theme = "primary",
          showcase = bsicons::bs_icon("clock")
        ),
        value_box(
          title = "Avg Cost per Day",
          value = textOutput("cost_per_day"),
          theme = "warning",
          showcase = bsicons::bs_icon("receipt")
        )
      ),
      
      card(
        card_header("Daily Charges (Cost per Day)"),
        plotlyOutput("charges_boxplot", height = "420px"),
        mod_download_plot_ui("dl_charges", label = "Download")
      )
    ),
    
    nav_panel(
      "Data", 
      card(
        card_header("Filtered Dataset"),
        downloadButton("download_data", "Download CSV"),
        DT::dataTableOutput("data_table")
      )
    ),
    
    nav_panel(
      "About",
      card(
        card_header("About This Dashboard"),
        p("This dashboard explores outcomes and costs for 12,844 heart attack patients from New York State (1993). It lets you filter by outcome, diagnosis, DRG, and age to view distributions, trends, and costs."),
        p("Research suggests higher female mortality after heart attack is multifactorial, driven by a combination of:"),
        tags$ul(
          tags$li("Older age and higher comorbidity burden at presentation."),
          tags$li("More non-classic symptoms, leading to delays in recognition and treatment."),
          tags$li("Higher prevalence of non-obstructive disease (e.g., MINOCA and microvascular dysfunction), which can complicate diagnosis."),
          tags$li("Treatment gaps or lower use of guideline therapies in some settings."),
          tags$li("Psychosocial and socioeconomic factors that affect recovery and adherence.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    d <- heart
    if (input$outcome != "All") {
      d <- d[d$DIED == input$outcome, ]
    }
    if (input$diagnosis != "All") {
      d <- d[as.character(d$DIAGNOSIS) == input$diagnosis, ]
    }
    if (input$drg != "All") {
      d <- d[as.character(d$DRG) == input$drg, ]
    }
    d <- d[d$AGE >= input$age_range[1] & d$AGE <= input$age_range[2], ]
    d
  })
  
  output$data_table <- DT::renderDataTable({
    filtered_data()
  })

  output$sex_summary <- DT::renderDataTable({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    summary_tbl <- aggregate(
      cbind(AGE, LOS, CHARGES) ~ SEX,
      data = df,
      FUN = function(x) mean(x, na.rm = TRUE)
    )
    
    deaths <- aggregate(DIED ~ SEX, data = df, FUN = function(x) mean(x == "Died", na.rm = TRUE))
    counts <- aggregate(ID ~ SEX, data = df, FUN = length)
    
    summary_tbl <- merge(summary_tbl, deaths, by = "SEX")
    summary_tbl <- merge(summary_tbl, counts, by = "SEX")
    
    names(summary_tbl) <- c(
      "Sex",
      "Avg Age",
      "Avg LOS",
      "Avg Charges",
      "Mortality Rate",
      "Patients"
    )
    
    summary_tbl$`Avg Age` <- format_num(summary_tbl$`Avg Age`, digits = 1)
    summary_tbl$`Avg LOS` <- paste0(format_num(summary_tbl$`Avg LOS`, digits = 1), " days")
    summary_tbl$`Avg Charges` <- format_money(summary_tbl$`Avg Charges`)
    summary_tbl$`Mortality Rate` <- paste0(format_num(summary_tbl$`Mortality Rate` * 100, digits = 1), "%")
    
    DT::datatable(
      summary_tbl,
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE,
        pageLength = 5
      )
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("heart_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- filtered_data()
      req(nrow(df) > 0)  # ensures there is data
      write.csv(df, file, row.names = FALSE, na = "")
    }
  )
  
  # Female stats
  output$f_mortality <- renderText({
    d <- compute_mortality(filtered_data()[filtered_data()$SEX == "Female", ])
  })
  
  # Male stats
  output$m_mortality <- renderText({
    d <- compute_mortality(filtered_data()[filtered_data()$SEX == "Male", ])
  })
  
  # Create the age plot as a reactive (reusable)
  age_plot <- reactive({
    req(nrow(filtered_data()) >= 2)
    ggplot(filtered_data(), aes(x = AGE, fill = DIED)) +
      geom_density(alpha = 0.5) +
      labs(x = "Age", y = "Density", fill = "DIED") +
      facet_wrap(~ SEX) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
  
  # Display the plot
  output$age_hist <- renderPlot({
    age_plot()
  })
  
  # Recall mod_download_plot() functon
  mod_download_plot_server("dl_age", filename = "age_distribution", figure = age_plot)
  
  # Avg LOS by Sex
  output$f_avg_los <- renderText({
    df <- filtered_data()[filtered_data()$SEX == "Female", ]
    x <- mean(df$LOS, na.rm = TRUE)
    paste0(format_num(x, digits = 1), " days")
  })
  
  output$m_avg_los <- renderText({
    df <- filtered_data()[filtered_data()$SEX == "Male", ]
    x <- mean(df$LOS, na.rm = TRUE)
    paste0(format_num(x, digits = 1), " days")
  })
  
  # LOS density plot by Sex and DIED
  los_plot <- reactive({
    df <- filtered_data()
    df <- df[!is.na(df$LOS), ]
    req(nrow(df) >= 2)
    ggplot(df, aes(x = LOS, fill = DIED)) +
      geom_density(alpha = 0.5) +
      labs(x = "Length of Stay (days)", y = "Density", fill = "DIED") +
      facet_wrap(~ SEX) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
  
  output$los_density <- renderPlot({
    los_plot()
  })
  
  mod_download_plot_server("dl_los", filename = "los_distribution", figure = los_plot)
  
  output$scatter_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) >= 1)
    if(nrow(df) > 1000) {
      df <- df[sample(nrow(df), 1000), ] # just a sample to make the plotting faster
    }
    p <- ggplot(df, aes(x = AGE, y = LOS, color = SEX)) +
      geom_point(alpha = 0.3) +
      labs(x = "Age", y = "Length of Stay (days)", color = "Sex") +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal()
    ggplotly(p)
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "outcome", selected = "All")
    updateSelectInput(session, "diagnosis", selected = "All")
    updateSelectInput(session, "drg", selected = "All")
    updateSliderInput(session, "age_range",
                      value = c(min(heart$AGE), max(heart$AGE)))
  })
  
  format_money <- function(x) {
    bad <- is.na(x) | is.nan(x) | is.infinite(x)
    out <- paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","))
    out[bad] <- "—"
    out
  }
  
  format_num <- function(x, digits = 1) {
    bad <- is.na(x) | is.nan(x) | is.infinite(x)
    out <- formatC(x, format = "f", digits = digits, big.mark = ",")
    out[bad] <- "—"
    out
  }
  
  # Avg Charges (ignore missing charges)
  output$avg_charges <- renderText({
    df <- filtered_data()
    x <- mean(df$CHARGES, na.rm = TRUE)
    format_money(x)
  })
  
  # Avg LOS
  output$avg_los <- renderText({
    df <- filtered_data()
    x <- mean(df$LOS, na.rm = TRUE)
    paste0(format_num(x, digits = 1), " days")
  })
  
  # Avg Cost per Day: sum(CHARGES) / sum(LOS) using LOS > 0 and non-missing charges
  output$cost_per_day <- renderText({
    df <- filtered_data()
    df <- df[!is.na(df$CHARGES) & df$LOS > 0, ]
    
    if (nrow(df) < 1) return("—")
    
    x <- sum(df$CHARGES) / sum(df$LOS)
    format_money(x)
  })
  
  # Row-level dataset for Charges boxplot: compute cost per day, filter invalid rows
  charges_plot_data <- reactive({
    df <- filtered_data()
    
    # Need columns present
    req(!is.null(df$CHARGES), !is.null(df$LOS), !is.null(df$SEX), !is.null(df$DRG))
    
    # Keep only rows with non-missing charges and LOS > 0
    df <- df[!is.na(df$CHARGES) & !is.na(df$LOS) & df$LOS > 0, ]
    req(nrow(df) >= 1)
    
    # Compute cost per day per stay
    df$COST_PER_DAY <- df$CHARGES / df$LOS
    
    # Optional: drop non-finite values (in case of weird data)
    df <- df[is.finite(df$COST_PER_DAY), ]
    req(nrow(df) >= 1)
    
    df
  })
  
  charges_plot <- reactive({
    df <- charges_plot_data()
    req(nrow(df) >= 1)
    
    ggplot(df, aes(x = SEX, y = COST_PER_DAY, fill = SEX, text = paste0(
      "Sex: ", SEX,
      "<br>DRG: ", DRG,
      "<br>Cost/Day: $", formatC(COST_PER_DAY, format = "f", digits = 0, big.mark = ",")
    ))) +
      geom_boxplot(na.rm = TRUE) +
      facet_wrap(~ DRG) +
      labs(
        x = "Sex",
        y = "Charges per Day",
        title = "Daily Charges by Sex, Faceted by DRG"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none"
      )
  })
  
  output$charges_boxplot <- renderPlotly({
    ggplotly(charges_plot(), tooltip = "text")
  })
  
  mod_download_plot_server("dl_charges", filename = "daily_charges", figure = charges_plot)
  
}

shinyApp(ui = ui, server = server)
