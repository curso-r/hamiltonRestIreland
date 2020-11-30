#' first_version UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_first_version_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    bs4Dash::dashboardPage(
      sidebar_collapsed = TRUE,
      sidebar_mini = FALSE,
      body = bs4Dash::bs4DashBody(
        hamiltonThemes::use_bs4Dash_distill_theme(),
        br(),
        fluidRow(
          column(
            width = 3,
            shinyjs::useShinyjs(),
            dateInput(ns("date_end"), "End of two week period to estimate R:",
                      value = max(latest_covid$Date),
                      format = "dd/mm/yyyy"),
            
            actionButton(inputId = ns("button"), label = "show extra options"),
            
            shinyWidgets::pickerInput(ns("R_method"),
                        "Method for computing R", 
                        choices = c("EG", "ML", "SB"),
                        selected = c('SB'),
                        multiple = FALSE),
            
            shinyWidgets::pickerInput(ns("GD_dist"),
                        "Generation time distribution", 
                        choices = c("gamma", "weibull", "lognormal"),
                        selected = c('gamma'),
                        multiple = FALSE),
            
            numericInput(inputId = ns("GT_mean"),
                         label = "Generation time mean",
                         value = 3.0),
            
            numericInput(inputId = ns("GT_sd"),
                         label = "Generation time standard deviation",
                         value = 0.4),
            
            numericInput(inputId = ns("num_sim"),
                         label = "Number of simulations to run (higher = slower but more accurate)",
                         value = 200)
            
          ),
          bs4Dash::bs4TabCard(
            width = 9,
            closable = FALSE,
            collapsible = FALSE,
            id = "tabItem",
            title = "COVID-19 R at province level",
            bs4Dash::bs4TabPanel(
              tabName = "Estimation",
              plotly::plotlyOutput(ns("R_estim")) %>% hamiltonThemes::distill_load_spinner()
            ),
            bs4Dash::bs4TabPanel(
              tabName = "Assumptions",
              get_assumptions_text()
            )
          )
          
        ),
        hamiltonThemes:::bs4dash_distill_footer()
      )
    )
  )
}
    
#' first_version Server Function
#'
#' @noRd 
mod_first_version_server <- function(input, output, session){
  ns <- session$ns
 
  observeEvent(input$button, {
    shinyjs::toggle("R_method")
    shinyjs::toggle("GD_dist")
    shinyjs::toggle("GT_mean")
    shinyjs::toggle("GT_sd")
    shinyjs::toggle("num_sim")
  }, ignoreNULL = FALSE)
  
  output$R_estim <- plotly::renderPlotly({

    # Get the data
    data_use = latest_covid %>% 
      dplyr::select(Date, CountyName, Province, ConfirmedCovidCases, PopulationCensus16) %>% 
      dplyr::group_by(Province, Date) %>% 
      dplyr::summarise(cum_cases = sum(ConfirmedCovidCases),
                pop = sum(PopulationCensus16)) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(Province) %>% 
      dplyr::mutate(cases = c(cum_cases[1], pmax(0, diff(cum_cases)))) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(Date >= input$date_end - 14, Date <= input$date_end) #%>% 
    #na.omit()
    
    # COVID generation time
    GT = R0::generation.time(input$GD_dist, c(input$GT_mean, input$GT_sd))
    
    # Now get R0
    provinces = unique(data_use$Province)
    n_provinces = length(provinces)
    estR0 = vector('list', length = n_provinces)
    
    for(i in 1:n_provinces) {
      curr_data = data_use %>% dplyr::filter(Province == provinces[i])
      estR0[[i]] = try(R0::estimate.R(epid = curr_data$cases,
                                  t = curr_data$Date, 
                                  begin = as.integer(1),
                                  end = as.integer(length(curr_data$cases)),
                                  GT = GT, 
                                  methods = input$R_method, 
                                  pop.size = curr_data$pop[1], 
                                  nsim = input$num_sim), silent = TRUE)
      
      if(class(estR0[[i]]) != 'try-error') {
        
        if(input$R_method == "SB") {
          R_est = signif(utils::tail(estR0[[i]]$estimates[[input$R_method]]$R, 1), 3)
        } else {
          R_est = signif(estR0[[i]]$estimates[[input$R_method]]$R, 3)
        }
        new_province_name = paste0(provinces[i],"; R = ", R_est)
      } else {
        new_province_name = paste0(provinces[i],"; R0 not estimated")
      }
      data_use$Province2[data_use$Province == provinces[i]] = new_province_name
    }
    # Put it in as an ordered factor
    data_use$Province2 = factor(data_use$Province2,
                                levels = unique(data_use$Province2)[c(1,4,3,2)],
                                ordered = TRUE)
    
    # Put the R values into the province titles
    p = ggplot2::ggplot(data = data_use, ggplot2::aes(x = Date, y = cases)) + 
      ggplot2::geom_point() + 
      ggplot2::facet_wrap(~ Province2, nrow = 2) +
      ggplot2::labs(x = 'Date',
           y = 'Cases',
           title = paste('Cases from', 
                         format(input$date_end-14, '%d-%b'), 'to',
                         format(input$date_end, '%d-%b'))) + 
      ggplot2::theme_bw() + 
      ggplot2::geom_smooth(se = FALSE)
    
    plotly::ggplotly(p)
    
    
  })
}
    
## To be copied in the UI
# mod_first_version_ui("first_version_ui_1")
    
## To be copied in the server
# callModule(mod_first_version_server, "first_version_ui_1")
 
