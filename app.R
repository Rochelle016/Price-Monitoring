# Loading libraries -------------------------------------------------------
library(shiny)
library(DT)
library(tidyverse)
library(openxlsx)
library(lubridate)
library(bslib)
library(bsicons)
library(plotly)
library(dplyr)
library(httr)
library(jsonlite)
library(shinyMatrix)


# Price data --------------------------------------------------------------------

prices <-  list()
prices[["farmgate"]] <-  read.xlsx("Farmgate Prices, Monthly, 1990-2024.xlsx")

prices[["retail"]] <-  read.xlsx("Retail Prices, Monthly, 1990-2024.xlsx")

prices[["wsale"]] <-  read.xlsx("Wholesale Prices, Monthly, 1990-2024.xlsx")

CPI <-  read_csv("Rice-CPI.csv")


# Price deviation data ----------------------------------------------------

PriceDevIndex <- 
  prices[["retail"]] |> 
  filter(location == "PHILIPPINES") |> 
  mutate(id= 1:n(),
         month = match(month, month.name)) |> 
  arrange(month,id) |> 
  group_by(month) |> 
  mutate(year = seq(from = 1990, to =2024)) |> 
  arrange(year,month) |> 
  ungroup() |> 
  mutate(rmr_1 = lag(rmr),
         rmr_d = rmr - rmr_1,
         rmr_ratio = rmr_d/rmr_1,
         monthname = month.name[month],
         monthabb = month.abb[month]) |> 
  left_join(CPI,
            by = c("monthabb"="Period",
                   "year" = "Year")) |>
  filter(year>=1994) |> 
  ungroup() |> 
  mutate(rmr_real = (rmr/`Rice CPI`)*100) |> 
  group_by(month) |> 
  mutate(rmr_ave = mean(rmr_real,na.rm=TRUE),
         rmr_ave2 = mean(rmr,na.rm=TRUE),) |> 
  ungroup() |> 
  mutate(
    rmr_meas = (rmr_real - rmr_ave)/rmr_ave,
    rmr_meas2 = (rmr - rmr_ave2)/rmr_ave2,
    date = mdy(paste(month,"01",year,sep="-")),
    rmr_ratio = rmr_ratio*100) |> 
  ungroup() |> 
  select(date,RetNom=rmr,RetReal=rmr_real,RetHistAve=rmr_ave,PriceDev=rmr_meas,PriceDev2=rmr_meas2,TriggerPercent =rmr_ratio)


# Index list --------------------------------------------------------------

Index <- list()
Index[["01-RicePrice"]] <- PriceDevIndex
Index[["02-RiceInf"]] <-  read_csv("Rice Contri to Inflation.csv")
Index[["03-IppGap"]] <-  read.xlsx("IPP Wholesale Gap (anomalies).xlsx")  |>
  mutate(Month = match(month, month.name),
         date = mdy(paste(Month,"01",year,sep="-")),
         ipp_gap = ippwsgap*100
  )
Index[["04-WsaleGap"]] <-  read.xlsx("Wholesale-Retail-National-Level.xlsx")
Index[["05-SUR"]] <-  read.xlsx("stock_use_ratio (1).xlsx")



# CPSI (real prices) ------------------------------------------------------

CPSI <- Index[["01-RicePrice"]] |> 
  left_join(Index[["02-RiceInf"]] |>
              mutate(Month = match(Period,month.abb),
                     date = mdy(paste(Month,"01",Year,sep="-")),
                     Contri = Contri/100) |> 
              select(date,HeadInf=All,RiceInf=Rice,Contri)) |> 
  left_join(Index[["03-IppGap" ]] |> 
              select(date,IppReal=ippreal,IppNom =ippnom, WsaleReal=wholesale,IPPGap=ippwsgap)
  ) |> 
  left_join(Index[["04-WsaleGap"]] |>
              mutate(Month = match(month,month.name),
                     date = mdy(paste(Month,"01",year,sep="-"))
              ) |> 
              select(date,
                     WsaleNom = rmr_wholesale,
                     RetGap = RMR_Diff)
  ) |> 
  left_join(Index[["05-SUR"]] |> 
              mutate(Month = match(month,month.name),
                     date = mdy(paste(Month,"01",Year,sep="-"))
              ) |>
              ungroup() |> 
              arrange(date) |> 
              mutate(SURindex = (1- total.stocks/lag(total.use,12))
              ) |> 
              select(date,SURCurrent = total.stocks,
                     TotalUse = total.use,
                     SURindex)
  ) |> 
  na.omit() |> 
  mutate(CPSI =(abs(PriceDev) + abs(Contri) + abs(IPPGap) + abs(RetGap) + SURindex)*0.2,
         risk_categ = case_when(CPSI <0.15~ "Low Risk",
                                CPSI>=0.15 & CPSI<0.25~ "Moderate Risk",
                                CPSI>=0.25 ~ "High Risk")
  )


write.csv(CPSI,"CPSI.csv")

colnames(CPSI)



# Merging datasets on monthyr column ----------------------------------------
merged_data <- left_join(fob, exrates, by = "monthyr") %>%
  left_join(price, by = "monthyr") %>%
  left_join(retail, by = "monthyr") %>%
  left_join(farmgate, by = "monthyr") %>%
  select(-starts_with("created")) %>%
  select(-starts_with("updated")) %>%
  select(-starts_with("id")) %>%
  arrange(desc(monthyr))

# custom css --------------------------------------------------------------
css <- ".navbar-nav, .nav-link.active {
    --bs-navbar-hover-color: #C3C73C;
    --bs-navbar-active-color: #C3C73C;
}

.nav-underline {
    --bs-nav-underline-link-active-color:#0A9444;
}

.logo img {
max-height: 50px; /*Adjust logo size*/
}

.bslib-sidebar-layout>.main {
background-color: #11763b;
}

.irs-handle.single {
  background-color: #0A9444
}

.irs-bar.irs-bar--single {
  background: #0A9444;
  border-top: 1px solid #0A9444;
  border-bottom: 1px solid #0A9444;
  height: 2px;
  z-index: 2;
  top: 0 px;
  cursor: s-resize
}

.irs--shiny .irs-single {
background-color: #0A9444
}

.selectize-dropdown .selected {
  background-color: #0A9444;
  color: #ffffff;
}
.bslib-value-box .value-box-grid, .card {
background-color: #E3EADE;
}

"

# theme -------------------------------------------------------------------

theme <-  bs_theme(5,
                   font_scale = 0.9,
                   primary = "#0A9444")

bs_theme_update(theme, 
                primary = "#0A9444")

# UI for application ------------------------------------------------------
ui <- page_sidebar(fill=TRUE,
                   theme = theme,
                   tags$head(
                     tags$style(HTML(css))
                   ),
                   sidebar = sidebar(
                     width =300,
                     div(
                       img(src = "philricelogo2.png", height = "120px", 
                           width = "120px", left="30px"),
                       style = "text-align: center;"
                     ),
                     bg = "#E3EADE",
                     selectInput(
                       inputId = "rice_type", 
                       label = "Select for FOB:", 
                       choices = unique(merged_data$type), 
                       selected = "Vietnam 25%"
                     ),
                     numericInput(
                       inputId = "desired_fob", 
                       label = "Enter Desired FOB Price ($/ton):", 
                       value = 0,
                       min = 0, 
                       step = 1),
                     
                     
                     radioButtons("cost_choice", "Importation Cost ($/ton):",
                                  choices = list("Total" = "total",
                                                 "Sub-item costs" = "detailed"),
                                  selected = "total"),
                     
                     conditionalPanel(
                       condition = "input.cost_choice == 'total'",
                       numericInput("import", "Importation Cost:",
                                    min = 0, value = 62.42),
                       # pre = "$", post = "/ton", animate = TRUE)
                     ),
                     
                     conditionalPanel(
                       condition = "input.cost_choice == 'detailed'",
                       numericInput("freight_cost", "Freight Cost ($/ton):", value = 29.30, min = 0, step = 1),
                       numericInput("surveyor_fee", "Surveyor's Fee ($/ton):", value = 1.81, min = 0, step = 1),
                       numericInput("insurance_cost", "Insurance Cost ($/ton):", value = 2.95, min = 0, step = 1),
                       numericInput("cargo_handling_cost", "Integrated Cargo Handling Cost ($/ton):", 
                                    value = 28.36, min = 0, step = 1)
                     ),
                     
                     actionButton("add_monthly_tariff", "Set Monthly Tariff Rates"),
                     selectInput("tariff_year", "Select Year:", choices = 2016:2024), # Adjust as needed
                     uiOutput("monthly_tariff_ui"),
                     
                     sliderInput("tariff", "Tariff Rate (%):",
                                 min = 0, max = 100, value = 35, step = 5,
                                 post = "%", animate = TRUE),
                     numericInput(
                       inputId = "desired_exrate", 
                       label = "Enter Desired Exchange Rate (₱/$):", 
                       value = 0,
                       min = 0, 
                       step = 1
                     ),
                     numericInput("transpo", "Local Transportation Cost (₱/ton):",
                                  min = 0, value = 1443),
                     
                     # Marketing costs inputs
                     radioButtons("marketing_cost_choice", "Marketing Cost (₱/kg):",
                                  choices = list("Total" = "total",
                                                 "Sub-item costs" = "detailed"),
                                  selected = "total"),
                     
                     conditionalPanel(
                       condition = "input.marketing_cost_choice == 'total'",
                       numericInput("total_marketing_cost", "Total Marketing Cost:", value = 9.5)
                     ),
                     
                     conditionalPanel(
                       condition = "input.marketing_cost_choice == 'detailed'",
                       numericInput("drying_cost", "Drying Cost:", value = 0.30),
                       numericInput("milling_processing_cost", "Milling/Processing Cost:", value = 1.60),
                       numericInput("transportation_cost", "Transportation Cost:", value = 2.30),
                       numericInput("packaging_cost", "Packaging Cost:", value = 0.45),
                       numericInput("other_costs", "Other Costs:", value = 0.45),
                       numericInput("market_players_profit", "Market Players Profit:", value = 5.00)
                     ),
                     
                     selectInput("mill_type", "Select for Wholesale/Retail Price:", 
                                 choices = c("Well milled" = "wmr", "Regular milled" = "rmr"), selected = "wmr"),
                     
                     # Milling recovery slider
                     sliderInput("milling_recovery", "Milling Recovery (%):", 
                                 min = 0, max = 100, value = 65, step = .5,
                                 post = "%", animate = TRUE)
                   ),
                   
                   page_fillable(
                     div(
                       span(strong("Composite Price-Supply Index (CPSI)"), style = "font-size:50px;"),
                       em(" This dashboard is a joint effort by"), em(strong(" DA National Rice Program and DA PhilRice Data Analytics Center")),
                       style = "font-size: 15px; color: #FFFFFF;"
                     ),
                     bg = "#11763B",
                     
                     layout_column_wrap(
                       width = 2/8,
                       fill=FALSE,
                       value_box(title = "Price Deviation Index",
                                 value = uiOutput("valuetext1"),
                                 showcase = bsicons::bs_icon("coin"),
                                 p("%")),
                       value_box(title = "Rice Inflation Contribution",
                                 value = uiOutput("valuetext2"),
                                 showcase = bsicons::bs_icon("globe"),
                                 p("%")),
                       value_box(title = "Wholesale-IPP Gap",
                                 value = uiOutput("valuetext3"),
                                 showcase = bsicons::bs_icon("bag"),
                                 p("%")),
                       value_box(title = "Retail-Wholesale Gap",
                                 value = uiOutput("valuetext4"),
                                 showcase = bsicons::bs_icon("basket"),
                                 p("%")),
                       value_box(title = "Stocks-to-Use Ratio",
                                 value = uiOutput("valuetext5"),
                                 showcase = bsicons::bs_icon("box-arrow-in-down-right"),
                                 p("%")),
                       value_box(title = "CPSI (Equal weights)",
                                 value = uiOutput("valuetext6"),
                                 showcase = bsicons::bs_icon("pin-map"),
                                 p("%")),
                       value_box(title = "CPSI (Model)",
                                value = uiOutput("valuetext7"),
                                showcase = bsicons::bs_icon("box-arrow-in-down-right"),
                                p("%")),
                       value_box(title = "CPSI (Adjusted weights)",
                                 value = uiOutput("valuetext8"),
                                 showcase = bsicons::bs_icon("box-arrow-in-down-right"),
                                 p("%"))
                     ),
                     layout_column_wrap(
                       navset_card_tab(
                         nav_panel(
                           title = "Summary Table",
                           card(dataTableOutput("finaltable"),
                                rownames = FALSE,
                                class = 'cell-border stripe'
                           ),
                           br(), # Adds space between the table and the text box
                           card(
                             strong("Key Results:"),
                             textOutput("key_results"),
                             style = "font-size: 18px; color: #333; background-color: white; padding: 10px; border: 1px solid #ccc;",
                             class = 'cell-border stripe'
                           )
                         ),
                         nav_panel(
                           title = "Trend",
                           card(
                             plotlyOutput("pricePlot"),
                             br(), # Adds space between the plot and the text box
                             card(
                               strong("Key Results:"),
                               textOutput("key_results"),
                               style = "font-size: 18px; color: #333; background-color: white; padding: 10px; border: 1px solid #ccc;",
                               class = 'cell-border stripe'
                             )
                           )
                         )
                       )
                     )
                   )
)

# Server  -----------------------------------------------------------------
server <- function(input, output, session) {
  observe({
    req(nrow(final_tab()) > 0)
    
    # Extracting the first row of the final table for reference values
    first_row <- final_tab()[1, ]
    
    # Use the desired FOB from user input, if available, otherwise use the FOB from the first row
    fob_value <- ifelse(!is.null(input$desired_fob) && !is.na(input$desired_fob) && input$desired_fob > 0, 
                        as.numeric(input$desired_fob), as.numeric(first_row$fob))
    
    # Use the selected tariff rate from the slider input
    tariff_rate <- input$tariff
    
    # Use the desired FOB from user input, if available, otherwise use the FOB from the first row
    exrate_value <- ifelse(!is.null(input$desired_exrate) && !is.na(input$desired_exrate) && input$desired_exrate > 0, 
                           as.numeric(input$desired_exrate), as.numeric(first_row$exrates))
    
    # Calculate importation cost and marketing cost
    importation_cost <- as.numeric(total_import_cost())
    local_transpo <- as.numeric(input$transpo)
    milling_recovery <- as.numeric(input$milling_recovery)
    total_marketing_cost <- as.numeric(total_marketing_cost())
    
    # Use the helper function to compute IPP and Farmgate Price
    results <- calculate_ipp_and_farmgate(fob_value, importation_cost, 
                                          exrate_value, 
                                          as.numeric(first_row$tariff_rate), 
                                          local_transpo, milling_recovery,
                                          total_marketing_cost)
    
    output$valuetext1 <-  renderUI(
      strong(fob_value)
    )
    
    output$valuetext2 <-  renderUI(
      strong(round(results$ipp, 2))
    )
    
    output$valuetext3 <-  renderUI(
      strong(first_row$wholesale_price)
    )
    
    output$valuetext4 <-  renderUI(
      strong(first_row$retail_price)
    )
    
    output$valuetext5 <-  renderUI(
      strong(round(results$farmgate_price, 2))
    )
    
    output$valuetext6 <-  renderUI(
      strong(first_row$farmgate)
    )
    
  })
  
  # UI for monthly tariff rates
  observeEvent(input$add_monthly_tariff, {
    output$monthly_tariff_ui <- renderUI({
      selected_year <- input$tariff_year
      tagList(
        lapply(month.abb, function(month) {
          numericInput(paste0("tariff_", selected_year, "_", month), 
                       paste("Tariff Rate for", month, "in", selected_year, " (%)"),
                       min = 0, value = 35)
          # , max = 100, step = 5, post = "%", animate = TRUE)
        })
      )
    })
  })
  
  # Helper function to fetch monthly tariff rate or default tariff if none is set
  get_monthly_tariff <- function(month) {
    # Access the selected year from the reactive context
    selected_year <- input$tariff_year
    month_tariff <- input[[paste0("tariff_", selected_year, "_", month)]]
    
    if (!is.null(month_tariff)) {
      return(month_tariff)
    } else {
      return(input$tariff) # Default tariff rate
    }
  }
  
  #Function for calculating IPP and farmgate price
  calculate_ipp_and_farmgate <- function(fob, importation_cost, 
                                         exchange_rate, tariff_rate, 
                                         local_transpo, milling_recovery, 
                                         total_marketing_cost) 
  {cif <- fob + importation_cost
  tariff_amount <- (tariff_rate) * ((cif *exchange_rate) / 100)
  ipp_ton <- ((cif * exchange_rate) + tariff_amount + local_transpo)
  ipp <- ipp_ton / 1000
  farmgate_price <- compute_farmgate_price(ipp, milling_recovery, total_marketing_cost)
  return(list(
    ipp = ipp,
    farmgate_price = farmgate_price
  ))
  }
  
  #computation for importation costs
  total_import_cost <- reactive({
    if (input$cost_choice == "total") {
      as.numeric(input$import)
    } else {
      sum(as.numeric(input$freight_cost), 
          as.numeric(input$surveyor_fee), 
          as.numeric(input$insurance_cost), 
          as.numeric(input$cargo_handling_cost), 
          na.rm = TRUE)
    }
  })
  
  #computation for marketing costs
  total_marketing_cost <- reactive({
    if (input$marketing_cost_choice == "total") {
      as.numeric(input$total_marketing_cost)
    } else {
      sum(as.numeric(input$drying_cost), 
          as.numeric(input$milling_processing_cost), 
          as.numeric(input$transportation_cost),
          as.numeric(input$packaging_cost), 
          as.numeric(input$other_costs), 
          as.numeric(input$market_players_profit), 
          na.rm = TRUE)
    }
  })
  
  #farmgate prices
  compute_farmgate_price <- function(ipp, milling_recovery, total_marketing_cost) {
    
    # Convert milling recovery to a proportion
    milling_recovery <- milling_recovery / 100
    
    # Compute farmgate price
    farmgate_price <- (ipp - total_marketing_cost) * milling_recovery
    return(farmgate_price)
  }
  
  
  # wholesale price ---------------------------------------------------------
  start_data <- reactive({
    
    req(input$mill_type, input$rice_type)
    
    # Filter for selected wholesale type
    output <- 
      merged_data %>%
      filter(type == input$rice_type) %>%  #filter rice type
      select(monthyr,type,value, exrates, farmgate,starts_with(input$mill_type)) %>%  # filter mill type
      rename(wrice_price =!!sym(input$mill_type),
             rrice_price = !!sym(paste0(input$mill_type,"2")))
    
    return(output)
  })
  
  final_tab <- reactive({
    req(input$rice_type %in% merged_data$type)
    
    merged_filtered_data <- start_data() %>%
      mutate(
        monthyr = as_date(monthyr),
        month = format(monthyr, "%b"),  # Extracts the month name (e.g., "Jan")
        year = format(monthyr, "%Y"),    # Extracts the year (e.g., "2024")
        date= format(monthyr,"%b-%Y"),
        fob = as.numeric(value),
        importation_cost = as.numeric(total_import_cost()),
        cif = fob + importation_cost,
        tariff_rate = ifelse(year == input$tariff_year, 
                             sapply(month, get_monthly_tariff), 
                             input$tariff), 
        tariff_amount = tariff_rate * (cif * (as.numeric(exrates) / 100)),
        # tariff_amount = as.numeric(input$tariff) * (cif * (as.numeric(exrates) / 100)),
        local_transpo = as.numeric(input$transpo),
        ipp_ton = ((cif * as.numeric(exrates)) + tariff_amount + local_transpo),
        ipp = ((cif * as.numeric(exrates)) + tariff_amount + local_transpo) / 1000,
        # cif_at_zero = cif * as.numeric(exrates) / 1000,
        marketing_cost = as.numeric(total_marketing_cost()),
        farmgate_price = compute_farmgate_price(as.numeric(ipp), as.numeric(input$milling_recovery), total_marketing_cost())
        # farmgate_price_22 = compute_farmgate_price_22(farmgate_price, as.numeric(input$drying_cost))
        # farmgate_price_26 = compute_farmgate_price_26(farmgate_price, as.numeric(input$drying_cost))
      ) %>%
      rename(wholesale_price = wrice_price,
             retail_price = rrice_price) %>% 
      relocate(exrates, .after = cif) %>%
      relocate(c(wholesale_price,retail_price), .after =ipp) %>%
      relocate(farmgate, .after = farmgate_price) %>%
      mutate_if(is.numeric, ~format(., big.mark = ",", digits = 2, nsmall = 2)) %>% 
      select(-type, -value, -month, -year) 
    
    return(merged_filtered_data)
  })
  
  observe({
    print("start data")
    print(start_data())
    print("final table")
    print(final_tab())
  })
  
  output$finaltable <- renderDataTable({
    datatable(final_tab() %>% 
                select(-monthyr),
              colnames = c("Month/Year" = "date",
                           "FOB ($/ton)" = "fob",
                           "Importation Cost ($/ton)" = "importation_cost",
                           "CIF ($/ton)" = "cif",
                           "₱/$ Exchange Rate" = "exrates",
                           "Tariff Rate (%)" = "tariff_rate",
                           "Price with Tariff (₱/ton)" = "tariff_amount",
                           "Local Transpo (₱/ton)" = "local_transpo",
                           "IPP (₱/ton)" = "ipp_ton",
                           "IPP (₱/kg)" = "ipp",
                           "Wholesale Price (₱/kg)" = "wholesale_price",
                           "Retail Price (₱/kg)" = "retail_price",
                           # "CIF (₱/kg)" = "cif_at_zero",
                           "Marketing Cost (₱/kg)" = "marketing_cost",
                           "Equivalent Farmgate at 14% MC (₱/kg)" = "farmgate_price",
                           # "Computed Farmgate at 22% MC (₱/kg)" = "farmgate_price_22",
                           "Actual Farmgate at 14% MC (₱/kg)"= "farmgate"),
              # "Farmgate at 26% MC (₱/kg)" = "farmgate_price_26"),
              rownames = FALSE,
              extensions = 'Buttons',
              options = list(dom = 'Btp',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             pageLength = 20))
  })
  
  output$pricePlot <- renderPlotly({
    plot_data <- final_tab() %>%
      mutate_if(is.character, as.numeric)
    
    plot_ly(plot_data, x = ~monthyr) %>%
      add_lines(y = ~ipp, name = "IPP", line = list(color = 'blue')) %>%
      add_lines(y = ~wholesale_price, name = "Wholesale", line = list(color = 'red', dash = "dash")) %>%
      add_lines(y = ~retail_price, name = "Retail", line = list(color = 'orange')) %>%
      # add_lines(y = ~cif_at_zero, name = "CIF at 0% tariff", line = list(color = 'black')) %>%
      add_lines(y = ~farmgate, name = "Actual Farmgate at 14% MC", line = list(color = 'black')) %>%
      add_lines(y = ~farmgate_price, name = "Equivalent Farmgate at 14% MC", line = list(color = 'darkgreen', dash = "dash")) %>%
      # add_lines(y = ~farmgate_price_22, name = "Computed Farmgate at 22% MC", line = list(color = 'darkgreen')) %>%
      # add_lines(y = ~farmgate_price_26, name = "Farmgate at 26% MC", line = list(color = 'darkgreen', dash = "dot")) %>%
      layout(title = "Price Comparison",
             hovermode = "x unified",
             xaxis = list(
               title = "Month/Year",
               rangeselector = list(
                 buttons = list(
                   list(count = 3, label = "3 mo", step = "month", stepmode = "backward"),
                   list(count = 6, label = "6 mo", step = "month", stepmode = "backward"),
                   list(count = 1, label = "1 yr", step = "year", stepmode = "backward"),
                   list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
                   list(step = "all")
                 )),
               rangeslider = list(type = "date")),
             yaxis = list(title = "Price (₱/kg)"),
             plot_bgcolor = 'rgb(229,229,229)')
  })
  
  observe({
    req(nrow(final_tab()) > 0)
    
    # Extract the first row for reference values
    first_row <- final_tab()[1, ]
    
    # Retrieve and calculate required values
    fob_value <- ifelse(!is.null(input$desired_fob) && !is.na(input$desired_fob) && input$desired_fob > 0, 
                        as.numeric(input$desired_fob), as.numeric(first_row$fob))
    exrate_value <- ifelse(!is.null(input$desired_exrate) && !is.na(input$desired_exrate) && input$desired_exrate > 0, 
                           as.numeric(input$desired_exrate), as.numeric(first_row$exrates))
    importation_cost <- as.numeric(total_import_cost())
    local_transpo <- as.numeric(input$transpo)
    milling_recovery <- as.numeric(input$milling_recovery)
    total_marketing_cost <- as.numeric(total_marketing_cost())
    tariff_rate <- input$tariff
    
    # Calculate IPP and Farmgate Price
    results <- calculate_ipp_and_farmgate(fob_value, importation_cost, 
                                          exrate_value, tariff_rate, 
                                          local_transpo, milling_recovery, 
                                          total_marketing_cost)
    
    # Construct the key results text
    key_results_text <- paste0(
      "At an FOB price of ", fob_value, " $/ton, the Import Parity Price (IPP) of rice is ",
      round(results$ipp, 2), " ₱/kg. The wholesale price of rice stands at ", 
      first_row$wholesale_price, " ₱/kg. 
      The difference between the IPP and the wholesale price is ",
      round(results$ipp - as.numeric(first_row$wholesale_price), 2), " ₱/kg. ",
      "The equivalent farmgate price for imported rice is ", 
      round(results$farmgate_price, 2), " ₱/kg, compared to the local farmgate price of ", 
      first_row$farmgate, " ₱/kg. This results in a difference of ", 
      round(results$farmgate_price - as.numeric(first_row$farmgate), 2), " PHP/kg between the equivalent and actual farmgate prices."
    )
    
    # Render the key results text
    output$key_results <- renderText({ key_results_text })
  })
  
}

# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
