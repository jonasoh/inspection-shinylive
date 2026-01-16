library(shiny)
library(bslib)
library(reactable)
library(data.table)
library(plotly)

# Load prepreprocessed data
metadata <- readRDS("data/metadata.rds")
stats_model_year <- readRDS("data/stats_model_year.rds")
stats_by_fault <- readRDS("data/stats_by_fault.rds")
avg_stats_by_fault <- readRDS("data/avg_stats_by_fault.rds")
stats_age <- readRDS("data/stats_age.rds")
stats_age_by_year <- readRDS("data/stats_age_by_year.rds")
brand_stats_by_age <- readRDS("data/brand_stats_by_age.rds")

# Extract metadata
years <- metadata$years
ages <- metadata$ages
cars <- metadata$cars

# UI using bslib
ui <- page_navbar(
  title = "Car inspection statistics",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  # custom js to auto-collapse navbar on link click (for mobile)
  header = tags$script(HTML("
    $(document).on('click', '.navbar-collapse.show .nav-link', function() {
      $('.navbar-collapse').collapse('hide');
    });
  ")),

  nav_panel(
    title = "Info",
    icon = icon("circle-info"),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Car inspection stats"),
        card_body(
          p("This website presents Finnish car inspection stats from the",
            a("Traficom Statistics Database",
              href = "https://trafi2.stat.fi/PXWeb/pxweb/en/TraFi/TraFi__Katsastuksen_vikatilastot/?tablelist=true",
              target = "_blank"),
            "which details the results of periodic inspections for all car models with over 100 inspected cars per year."),
          p("For the ranking by registration year and age, as well as for the brand leaderboard, only model-related errors which cause a demand for repair is accounted for,",
            "i.e., broken parking lights or slightly rusted brake discs do not affect the ratings."),
          p("As statistics are aggregated by model and fault category, some models will have over 100% fault rating. This is a necessary consequence of how the data are delivered by Traficom, and arguably the better way to present the data."),
          p("Use the navigation tabs above to choose which statistics to view.")
        )
      )
    )
  ),
  
  nav_panel(
    title = "By model and year",
    icon = icon("car"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Fault stats by model and registration year",
        sliderInput("reg_year", "Registration year:",
                    min = min(years), max = max(years), value = min(years),
                    step = 1, round = TRUE, sep = "", ticks = FALSE)
      ),
      reactableOutput("reg_year_table")
    )
  ),
  
  nav_panel(
    title = "By age",
    icon = icon("car"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Fault stats by vehicle age",
        sliderInput("vehicle_age", "Vehicle age:",
                    min = min(ages), max = max(ages), value = max(ages),
                    step = 1, round = TRUE, sep = "", ticks = FALSE)
      ),
      reactableOutput("age_table")
    )
  ),
  
  nav_panel(
    title = "Car model overview",
    icon = icon("car"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Car model overview",
        selectInput("car_model", "Car model:",
                    cars, selected = "Honda CIVIC"),
        sliderInput("model_reg_year", "Registration year:",
                    min = min(years), max = max(years), value = min(years),
                    step = 1, round = TRUE, sep = "", ticks = FALSE)
      ),
      reactableOutput("model_table")
    )
  ),
  
  nav_panel(
    title = "Brand leaderboard",
    icon = icon("trophy"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Brand leaderboard",
        sliderInput("brand_model_reg_year", "Registration year:",
                    min = min(years), max = max(years), value = min(years),
                    step = 1, round = TRUE, sep = "", ticks = FALSE)
      ),
      reactableOutput("brand_leaderboard_table")
    )
  ),

  nav_panel(
    title = "Compare models",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Compare car models",
        selectizeInput("compare_models", "Select car models:",
                       choices = cars, selected = c("Volvo V40", "Toyota COROLLA"),
                       multiple = TRUE,
                       options = list(maxItems = 10, placeholder = "Select models to compare"))
      ),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Fault % by Registration Year"),
          card_body(plotlyOutput("compare_by_year_plot", height = "400px"))
        ),
        card(
          card_header("Fault % by Vehicle Age"),
          card_body(plotlyOutput("compare_by_age_plot", height = "400px"))
        )
      )
    )
  )

  # nav_panel(
  #   title = "Cars by age",
  #   icon = icon("chart-bar"),
  #   layout_sidebar(
  #     sidebar = sidebar(
  #       title = "Cars on the road by age",
  #       selectInput("age_dist_model", "Select car model:",
  #                   choices = cars, selected = "Toyota COROLLA")
  #     ),
  #     card(
  #       card_header("Number of Inspected Cars by Vehicle Age"),
  #       card_body(plotlyOutput("cars_by_age_plot", height = "500px"))
  #     )
  #   )
  # )
)

# Server logic
server <- function(input, output) {
  output$reg_year_table <- renderReactable({
    dt <- stats_model_year[registration_year == input$reg_year]
    dt <- dt[, .(
      Model = brand_and_model_series,
      `Fault%` = round(fault_pct * 100, 1),
      `Avg. mileage (km)` = average_mileage,
      n = number_of_inspections
    )]

    reactable(
      dt,
      defaultPageSize = 200,
      defaultSorted = list(`Fault%` = "asc"),
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      columns = list(
        `Fault%` = colDef(format = colFormat(suffix = "%")),
        `Avg. mileage (km)` = colDef(format = colFormat(separators = TRUE)),
        n = colDef(format = colFormat(separators = TRUE))
      )
    )
  })

  output$age_table <- renderReactable({
    dt <- stats_age[vehicle_age == input$vehicle_age]
    dt <- dt[, .(
      Model = brand_and_model_series,
      `Fault%` = round(fault_pct * 100, 1),
      `Avg. mileage (km)` = average_mileage,
      n = number_of_inspections
    )]

    reactable(
      dt,
      defaultPageSize = 200,
      defaultSorted = list(`Fault%` = "asc"),
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      columns = list(
        `Fault%` = colDef(format = colFormat(suffix = "%")),
        `Avg. mileage (km)` = colDef(format = colFormat(separators = TRUE)),
        n = colDef(format = colFormat(separators = TRUE))
      )
    )
  })

  output$model_table <- renderReactable({
    model_dt <- stats_by_fault[brand_and_model_series == input$car_model &
                                registration_year == input$model_reg_year]
    avg_dt <- avg_stats_by_fault[registration_year == input$model_reg_year]

    model_dt <- model_dt[, .(main_fault_object, fault_pct, number_of_inspections)
    ][, fault_pct := fault_pct * 100
    ][, .(model_value = weighted.mean(fault_pct, number_of_inspections)),
      by = "main_fault_object"]

    avg_dt <- avg_dt[, .(main_fault_object, fault_pct, number_of_inspections)
    ][, fault_pct := fault_pct * 100
    ][, .(avg_value = weighted.mean(fault_pct, number_of_inspections)),
      by = "main_fault_object"]

    model_table <- model_dt[avg_dt, on = "main_fault_object"]
    model_table[, diff := fcase(
      is.na(model_value) | is.na(avg_value), "N/A",
      model_value < avg_value, 
      paste0(round((1 - (model_value / avg_value)) * 100), "% better"),
      model_value > avg_value, 
      paste0(round((1 - (avg_value / model_value)) * 100), "% worse"),
      default = "Same as average"
    )]

    result <- model_table[, .(
      `Fault type` = main_fault_object,
      `This model (%)` = round(model_value, 1),
      `Average (%)` = round(avg_value, 1),
      `This model compared to average` = diff
    )]

    reactable(
      result,
      striped = TRUE,
      highlight = TRUE,
      columns = list(
        `This model (%)` = colDef(format = colFormat(suffix = "%")),
        `Average (%)` = colDef(format = colFormat(suffix = "%"))
      )
    )
  })

  output$brand_leaderboard_table <- renderReactable({
    dt <- brand_stats_by_age[registration_year == input$brand_model_reg_year]
    dt <- dt[, .(
      Rank = rank,
      Brand = brand,
      `Avg. mileage (km)` = average_mileage
    )]

    reactable(
      dt,
      defaultPageSize = 50,
      defaultSorted = list(Rank = "asc"),
      searchable = TRUE,
      striped = TRUE,
      highlight = TRUE,
      columns = list(
        `Avg. mileage (km)` = colDef(format = colFormat(separators = TRUE))
      )
    )
  })

  output$compare_by_year_plot <- renderPlotly({
    req(input$compare_models)
    
    dt <- stats_model_year[brand_and_model_series %in% input$compare_models]
    dt <- dt[, .(
      Model = brand_and_model_series,
      Year = registration_year,
      Fault_pct = round(fault_pct * 100, 1)
    )]
    
    p <- plot_ly(dt, x = ~Year, y = ~Fault_pct, color = ~Model,
                 type = "scatter", mode = "lines+markers",
                 hovertemplate = "%{y:.1f}%<extra>%{fullData.name}</extra>") %>%
      layout(
        xaxis = list(title = "Registration Year"),
        yaxis = list(title = "Fault %", ticksuffix = "%"),
        hovermode = "x unified",
        legend = list(orientation = "h", y = -0.2)
      )
    
    p
  })

  output$compare_by_age_plot <- renderPlotly({
    req(input$compare_models)
    
    dt <- stats_age[brand_and_model_series %in% input$compare_models]
    dt <- dt[, .(
      Model = brand_and_model_series,
      Age = vehicle_age,
      Fault_pct = round(fault_pct * 100, 1)
    )]
    
    p <- plot_ly(dt, x = ~Age, y = ~Fault_pct, color = ~Model,
                 type = "scatter", mode = "lines+markers",
                 hovertemplate = "%{y:.1f}%<extra>%{fullData.name}</extra>") %>%
      layout(
        xaxis = list(title = "Vehicle Age (years)"),
        yaxis = list(title = "Fault %", ticksuffix = "%"),
        hovermode = "x unified",
        legend = list(orientation = "h", y = -0.2)
      )
    
    p
  })

  # need to investigate data issue here
  # output$cars_by_age_plot <- renderPlotly({
  #   req(input$age_dist_model)
    
  #   dt <- stats_age_by_year[brand_and_model_series == input$age_dist_model]
  #   dt <- dt[, .(
  #     Year = factor(registration_year),
  #     Age = vehicle_age,
  #     n = number_of_inspections
  #   )]
    
  #   p <- plot_ly(dt, x = ~Age, y = ~n, color = ~Year,
  #                type = "scatter", mode = "lines+markers",
  #                hovertemplate = "Age: %{x} years<br>Cars: %{y:,}<extra>%{fullData.name}</extra>") %>%
  #     layout(
  #       xaxis = list(title = "Vehicle Age (years)"),
  #       yaxis = list(title = "Number of Inspections", separatethousands = TRUE),
  #       hovermode = "x unified",
  #       legend = list(title = list(text = "Registration Year"), orientation = "v")
  #     )
    
  #   p
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
