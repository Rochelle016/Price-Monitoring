# Libraries -------------------------------------------------------
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
library(tsibble)
library(modeltime)
library(yardstick)
library(shinycssloaders)
library(shinyjs)


# Data --------------------------------------------------------------------
##  Price deviation ---------------------------------------------------------

prices <-  list()
prices[["farmgate"]] <-  read.xlsx("Farmgate Prices, Monthly, 1990-2024.xlsx")

prices[["retail"]] <-  read.xlsx("Retail Prices, Monthly, 1990-2024.xlsx")

prices[["wsale"]] <-  read.xlsx("Wholesale Prices, Monthly, 1990-2024.xlsx")

CPI <-  read_csv("Rice-CPI.csv")

PriceDevIndex <- 
  prices[["retail"]] |> 
  filter(location == "PHILIPPINES") |> 
  mutate(id = row_number(),
         month = match(month, month.name)) |> 
  arrange(month,id) |> 
  group_by(month) |> 
  mutate(year = 1990 + row_number() - 1) |> 
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


## CPSI computation --------------------------------------------------------

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

cpsi_data <- Index[["01-RicePrice"]] |> 
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
  mutate(Equal_CPSI =(abs(PriceDev) + abs(Contri) + abs(IPPGap) + abs(RetGap) + SURindex)*0.2,
         risk_categ = case_when(Equal_CPSI<0.15~ "Low Risk",
                                Equal_CPSI>=0.15 & Equal_CPSI<0.25~ "Moderate Risk",
                                Equal_CPSI>=0.25 ~ "High Risk")
  )

# Custom css --------------------------------------------------------------
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

# Theme -------------------------------------------------------------------

theme <-  bs_theme(5,
                   font_scale = 0.9,
                   primary = "#0A9444")

bs_theme_update(theme, 
                primary = "#0A9444")

# UI ----------------------------------------------------------------------

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
               selectInput(inputId = "cpsi_type", 
                           label = "Select CPSI Type:",
                            choices = list("Equal Weights CPSI" = "equal",
                                           "Modelled CPSI" = "model",
                                           "Adjusted CPSI" = "adjusted"),
                            selected = "equal"),
               
               conditionalPanel(
                 condition = "input.cpsi_type == 'equal'",
                 numericInput("weights", "Weights:",
                              value = 0.2),
               ),
               
               conditionalPanel(
                 condition = "input.cpsi_type == 'adjusted'",
                 numericInput("pricedev_wt", "Price Deviation Weights:", value = 0.2, min = 0),
                 numericInput("contri_wt", "Rice Inflation Contribution Weights:", value = 0.2, min = 0),
                 numericInput("ippgap_wt", "IPP Gap Weigths:", value = 0.2, min = 0),
                 numericInput("retgap_wt", "Retail Gap Weigths:", value = 0.2, min = 0),
                 numericInput("sur_wt", "Stocks-to-Use Weights:", value = 0.2, min = 0)
               )
               ),
               
               page_fillable(
                 div(
                   span(strong("Composite Price and Supply Index (CPSI)"), style = "font-size:50px;"),
                   em(" This dashboard is a joint effort by"), em(strong(" DA National Rice Program and DA PhilRice Data Analytics Center")),
                   style = "font-size: 15px; color: #FFFFFF;"
                 ),
                 bg = "#11763B",
                 
                 layout_column_wrap(
                   width = 2/8,
                   fill=FALSE,
                   value_box(title = "Price Deviation Index",
                             value = uiOutput("price_dev"),
                             showcase = bsicons::bs_icon("coin"),
                             p("%")),
                   value_box(title = "Rice Inflation Contribution",
                             value = uiOutput("contri"),
                             showcase = bsicons::bs_icon("globe"),
                             p("%")),
                   value_box(title = "Wholesale-IPP Gap",
                             value = uiOutput("ipp_gap"),
                             showcase = bsicons::bs_icon("bag"),
                             p("%")),
                   value_box(title = "Retail-Wholesale Gap",
                             value = uiOutput("ret_gap"),
                             showcase = bsicons::bs_icon("basket"),
                             p("%")),
                   value_box(title = "Stocks-to-Use Ratio",
                             value = uiOutput("sur_index"),
                             showcase = bsicons::bs_icon("box-arrow-in-down-right"),
                             p("%")),
                   value_box(title = "CPSI (Equal weights)",
                             value = uiOutput("equal_cpsi"),
                             showcase = bsicons::bs_icon("pin-map"),
                             p("%")),
                   value_box(title = "CPSI (Model)",
                             value = uiOutput("modelled_cpsi"),
                             showcase = bsicons::bs_icon("box-arrow-in-down-right"),
                             p("%")),
                   value_box(title = "CPSI (Adjusted weights)",
                             value = uiOutput("adjusted_cpsi"),
                             showcase = bsicons::bs_icon("box-arrow-in-down-right"),
                             p("%"))
                 ),
                 layout_column_wrap(
                   navset_card_tab(
                     nav_panel(
                       title = "Summary Table",
                       card(dataTableOutput("cpsi_table"),
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
                         plotlyOutput("cpsi_plot"),
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

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # observe({
  #   req(nrow(cpsi_table()) > 0)
  #   
  #   # Extract the first row of the dataset for calculations
  #   first_row <- cpsi_table()[1, ]
  #   
  #   # Compute the weighted indices
  #   price_dev_index <- abs(first_row$PriceDev) * input$pricedev_wt
  #   contri_index <- abs(first_row$Contri) * input$contri_wt
  #   ipp_gap_index <- abs(first_row$IPPGap) * input$ippgap_wt
  #   ret_gap_index <- abs(first_row$RetGap) * input$retgap_wt
  #   sur_index <- first_row$SURindex * input$sur_wt
  #   
  #   # Compute CPSI values
  #   equal_cpsi <- mean(c(price_dev_index, contri_index, ipp_gap_index, ret_gap_index, sur_index))
  #   adjusted_cpsi <- price_dev_index + contri_index + ipp_gap_index + ret_gap_index + sur_index
  #   modelled_cpsi <- model_preds |> filter(date == first_row$date) |> pull(.pred) |> mean(na.rm = TRUE)
  #   
  #   # Determine Risk Category
  #   risk_categ <- case_when(
  #     adjusted_cpsi < 0.15 ~ "Low Risk",
  #     adjusted_cpsi >= 0.15 & adjusted_cpsi < 0.25 ~ "Moderate Risk",
  #     adjusted_cpsi >= 0.25 ~ "High Risk"
  #   )
  #   
  #   # Dynamically generate UI for value boxes
  #   output$value_boxes <- renderUI({
  #     tagList(
  #       valueBox(round(price_dev_index, 2), "Price Deviation Index", icon = icon("chart-line")),
  #       valueBox(round(contri_index, 2), "Rice Inflation Contribution Index", icon = icon("percentage")),
  #       valueBox(round(ipp_gap_index, 2), "Wholesale-IPP Gap Index", icon = icon("balance-scale")),
  #       valueBox(round(ret_gap_index, 2), "Retail-Wholesale Gap Index", icon = icon("shopping-cart")),
  #       valueBox(round(sur_index, 2), "Stocks-to-Use Ratio Index", icon = icon("warehouse"))
  #     )
  #   })
  #   
  #   # Render UI for CPSI value and Risk Category
  #   output$selected_cpsi_ui <- renderUI({
  #     cpsi_value <- if (input$cpsi_type == "equal") {
  #       equal_cpsi
  #     } else if (input$cpsi_type == "adjusted") {
  #       adjusted_cpsi
  #     } else {
  #       modelled_cpsi
  #     }
  #     
  #     tagList(
  #       valueBox(round(cpsi_value, 3), paste(input$cpsi_type, "CPSI"), 
  #                icon = icon(ifelse(input$cpsi_type == "modelled", "robot", "sliders-h"))),
  #       valueBox(risk_categ, "Risk Category", 
  #                color = ifelse(risk_categ == "High Risk", "danger",
  #                               ifelse(risk_categ == "Moderate Risk", "warning", "success")),
  #                icon = icon("exclamation-triangle"))
  #     )
  #   })
  # })
  
  # Reactive weights based on CPSI type
  # weights <- reactive({
  #   if (input$cpsi_type == "equal") {
  #     rep(input$weights, 5)
  #   } else if (input$cpsi_type == "adjusted") {
  #     raw_weights <- c(input$pricedev_wt, input$contri_wt, input$ippgap_wt, 
  #                      input$retgap_wt, input$sur_wt)
  #     if (sum(raw_weights) == 0) return(rep(0, 5)) # avoid division by zero
  #     raw_weights / sum(raw_weights)
  #   } else {
  #     NULL
  #   }
  # })
  weights <- reactive({
    if (input$cpsi_type == "equal") {
      rep(0.2, 5)
    } else if (input$cpsi_type == "adjusted") {
      raw_weights <- c(input$pricedev_wt, input$contri_wt, input$ippgap_wt, 
                       input$retgap_wt, input$sur_wt)
      if (sum(raw_weights) == 0) {
        rep(0.2, 5)  # fallback to equal weights if all adjusted inputs are zero
      } else {
        raw_weights / sum(raw_weights)
      }
    } else {
      rep(0.2, 5)  # default fallback
    }
  })
  
  
  # # Reactive CPSI based on selected type
  # cpsi_selected <- reactive({
  #   df <- cpsi_data
  #   if (input$cpsi_type == "equal") {
  #     df$CPSI <- with(df, (abs(PriceDev) + abs(Contri) + abs(IPPGap) + abs(RetGap) + SURindex) * input$weights)
  #   } else if (input$cpsi_type == "adjusted") {
  #     w <- weights()
  #     df$CPSI <- with(df,
  #                     abs(PriceDev) * w[1] +
  #                       abs(Contri)    * w[2] +
  #                       abs(IPPGap)   * w[3] +
  #                       abs(RetGap)   * w[4] +
  #                       SURindex      * w[5]
  #     )
  #   } else if (input$cpsi_type == "model") {
  #     df$CPSI <- df$Equal_CPSI # use modelled CPSI (or replace with model output if available)
  #   }
  #   df
  # })
  
  df <- cpsi_selected() %>%
    arrange(desc(date)) %>%
    slice(1) %>%
    mutate(
      PriceDev_w = round(PriceDev * w[1], 3),
      Contri_w = round(Contri * w[2], 3),
      IPPGap_w = round(IPPGap * w[3], 3),
      RetGap_w = round(RetGap * w[4], 3),
      Sur_w = round(Surplus * w[5], 3),
      CPSI = round(PriceDev_w + Contri_w + IPPGap_w + RetGap_w + Sur_w, 3)
    )
  
  df
})
  
  # Latest value from the selected CPSI data
  latest_row <- reactive({
    cpsi_selected() |> 
      filter(date == max(date))
  })
  
  # Output value boxes
  output$price_dev <- renderUI({
    HTML(paste0(round(latest_row()$PriceDev * 100, 2), "%"))
  })
  
  output$contri <- renderUI({
    HTML(paste0(round(latest_row()$Contri * 100, 2), "%"))
  })
  
  output$ipp_gap <- renderUI({
    HTML(paste0(round(latest_row()$IPPGap * 100, 2), "%"))
  })
  
  output$ret_gap <- renderUI({
    HTML(paste0(round(latest_row()$RetGap * 100, 2), "%"))
  })
  
  output$sur_index <- renderUI({
    HTML(paste0(round(latest_row()$SURindex * 100, 2), "%"))
  })
  
  output$equal_cpsi <- renderUI({
    eq_val <- cpsi_data |> filter(date == max(date)) |> pull(Equal_CPSI)
    HTML(paste0(round(eq_val * 100, 2), "%"))
  })
  
  output$modelled_cpsi <- renderUI({
    model_val <- cpsi_data |> filter(date == max(date)) |> pull(Equal_CPSI) # replace with model output if available
    HTML(paste0(round(model_val * 100, 2), "%"))
  })
  
  output$adjusted_cpsi <- renderUI({
    if (input$cpsi_type != "adjusted") return(HTML("—"))
    HTML(paste0(round(latest_row()$CPSI * 100, 2), "%"))
  })
  
#   # Modelled CPSI Training
#   set.seed(1417)
#   ts_cpsidata <- cpsi_data %>% as_tsibble(index = date)
#   train_data <- filter(ts_cpsidata, date < ymd("2023-01-01"))
#   test_data  <- filter(ts_cpsidata, date >= ymd("2023-01-01"))
#   
#   recipe <- recipe(PriceDev ~ Contri + IPPGap + RetGap + SURindex, data = train_data)
#   rf_model <- rand_forest(mode = "regression", trees = 500) %>% set_engine("ranger")
#   rf_wf <- workflow() %>% add_model(rf_model) %>% add_recipe(recipe)
#   rf_fit <- rf_wf %>% fit(data = train_data)
#   model_preds <- predict(rf_fit, test_data) %>% bind_cols(test_data)
#   
#   output$modelled_cpsi <- renderValueBox({
#     valueBox(round(mean(model_preds$.pred), 3), "Modelled CPSI", icon = icon("robot"))
#   })
#   
#   output$cpsi_plot <- renderPlotly({
#     CPSI |> 
#       plot_ly() |> 
#       add_trace(x = ~date,
#                 y = ~RetReal,
#                 type = 'scatter',
#                 mode = 'lines',
#                 yaxis = "y1",
#                 name = 'retail price (real)') |> 
#       add_trace(x = ~date,
#                 y = ~WsaleReal,
#                 type = 'scatter',
#                 mode = 'lines',
#                 yaxis = "y1",
#                 name = 'Wholesale price (real)') |> 
#       add_trace(x = ~date,
#                 y = ~RetNom,
#                 type = 'scatter',
#                 mode = 'lines',
#                 yaxis = "y1",
#                 name = 'retail price (Nominal)') |> 
#       add_trace(x = ~date,
#                 y = ~WsaleNom,
#                 type = 'scatter',
#                 mode = 'lines',
#                 yaxis = "y1",
#                 name = 'Wholesale price (Nominal)') |> 
#       add_trace(x = ~date,
#                 y = ~IppReal,
#                 type = 'scatter',
#                 mode = 'lines',
#                 yaxis = "y1",
#                 name = 'Import Parity price') |>
#       add_trace(x = ~date,
#                 y = ~CPSI,
#                 yaxis = "y3",
#                 type = 'scatter',
#                 mode = 'lines',
#                 name = "CPSI") |>
#       add_trace(x = ~date,
#                 y = ~PriceDev,
#                 yaxis = "y2",
#                 type = 'scatter',
#                 mode = 'lines',
#                 name ="Price deviation") |>
#       add_trace(x = ~date,
#                 y = ~Contri,
#                 yaxis = "y2",
#                 type = 'scatter',
#                 mode = 'lines',
#                 name ="Rice Inflation Contri") |>
#       add_trace(x = ~date,
#                 y = ~IPPGap,
#                 yaxis = "y2",
#                 type = 'scatter',
#                 mode = 'lines',
#                 name ="IPP and Wsale Gap") |>
#       add_trace(x = ~date,
#                 y = ~`RetGap`,
#                 yaxis = "y2",
#                 type = 'scatter',
#                 mode = 'lines',
#                 name ="Wsale and Ret Gap") |>
#       add_trace(x = ~date,
#                 y = ~SURindex,
#                 yaxis = "y2",
#                 type = 'scatter',
#                 mode = 'lines',
#                 name ="Change in Stocks to Use") |> 
#       add_trace(x = ~date,
#                 y = ~TriggerPercent,
#                 yaxis = "y2",
#                 type = 'scatter',
#                 mode = 'lines',
#                 name ="TriggPerc") |> 
#       
#       layout(yaxis2 = list(overlaying ="y",
#                            side = 'right'
#       ),
#       yaxis3 = list(overlaying ="y",
#                     side = 'right'
#       ),
#       hovermode = "x unified",
#       xaxis = list(title = "Date",
#                    rangeselector =  list(
#                      buttons = list(
#                        list(count = 3, label = "3 mo", step = "month", stepmode ="backward"),
#                        list(count = 6, label = "6 mo", step = "month", stepmode ="backward"),
#                        list(count = 1, label = "1 yr", step = "year", stepmode ="backward"),
#                        list(count = 1, label = "YTD", step = "year", stepmode ="todate"),
#                        list(step = "all")
#                      )),
#                    rangeslider = list(type ="date")
#       )
#       )
#   })
#   
#   # Compute Adjusted CPSI based on user input
#   adjusted_cpsi <- reactive({
#     req(input$cpsi_type == "adjusted")
#     cpsi_data |> 
#       mutate(Adjusted_CPSI = (abs(PriceDev) * input$pricedev_wt +
#                                 abs(Contri) * input$contri_wt +
#                                 abs(IPPGap) * input$ippgap_wt +
#                                 abs(RetGap) * input$retgap_wt +
#                                 SURindex * input$sur_wt))
#   })
#   
#   # Compute Modelled CPSI using Machine Learning Model
#   modelled_cpsi <- reactive({
#     # Assuming model_preds already exists in your code
#     cpsi_data |> 
#       left_join(model_preds |> select(date, Modelled_CPSI = .pred), by = "date")
#   })
#   
#   # Render DataTable with Equal, Adjusted, and Modelled CPSI
#   output$cpsi_table <- DT::renderDataTable({
#     req(nrow(cpsi_data) > 0)
#     
#     data <- cpsi_data |> 
#       left_join(adjusted_cpsi(), by = "date") |> 
#       left_join(modelled_cpsi(), by = "date")
#     
#     datatable(
#       data |> 
#         select(date, PriceDev, Contri, IPPGap, RetGap, SURindex, 
#                Equal_CPSI, Adjusted_CPSI, Modelled_CPSI, risk_categ),
#       options = list(pageLength = 10, autoWidth = TRUE),
#       rownames = FALSE
#     )
#   })
# }

  # Table output
  # output$cpsi_table <- renderDataTable({
  #   df <- cpsi_selected() |>
  #     select(date, PriceDev, Contri, IPPGap, RetGap, SURindex, CPSI) |>
  #     arrange(desc(date)) |>  # Sort dates in descending order
  #     mutate(across(-date, ~ round(.x, 3)))  # Round numeric columns to 3 decimal places
  #   
  #   datatable(
  #     df,
  #     options = list(pageLength = 10, scrollX = TRUE),
  #     rownames = FALSE
  #   )
  # })
  output$summary_table <- DT::renderDataTable({
    req(cpsi_selected(), weights())
    w <- weights()
    
    df <- cpsi_selected() %>%
      arrange(desc(date)) %>%
      mutate(
        PriceDev_w = round(PriceDev * w[1], 3),
        Contri_w = round(Contri * w[2], 3),
        IPPGap_w = round(IPPGap * w[3], 3),
        RetGap_w = round(RetGap * w[4], 3),
        Sur_w = round(Surplus * w[5], 3),
        CPSI = round(PriceDev_w + Contri_w + IPPGap_w + RetGap_w + Sur_w, 3)
      ) %>%
      select(date, PriceDev, Contri, IPPGap, RetGap, Surplus,
             PriceDev_w, Contri_w, IPPGap_w, RetGap_w, Sur_w, CPSI)
    
    DT::datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  # Trend plot
  output$cpsi_plot <- renderPlotly({
    df <- cpsi_selected()
    plot_ly(df, x = ~date, y = ~CPSI, type = 'scatter', mode = 'lines',
            name = 'CPSI', line = list(color = '#0A9444')) |>
      layout(title = paste("CPSI Trend -", input$cpsi_type),
             xaxis = list(title = "Date"),
             yaxis = list(title = "CPSI"))
  })
  
  # Key results
  output$key_results <- renderText({
    df <- latest_row()
    paste0("As of ", format(df$date, "%B %Y"), 
           ", the CPSI is ", round(df$CPSI * 100, 2), "%. ",
           "Risk level: ", df$risk_categ)
  })
  
}

shinyApp(ui, server)
