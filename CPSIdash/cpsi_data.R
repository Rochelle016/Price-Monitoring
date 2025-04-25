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
  select(year, month, wmr, rmr, monthname, monthabb) |> 
  write.xlsx("Monthly Retail Prices, 1990-2024.xlsx")

#Data

index <- list()
index[["prices"]] <- read.xlsx("Wholesale-Retail-National-Level.xlsx")
index[["cpi"]] <- read.xlsx("CPI.xlsx")
index[["inflation"]] <- read_csv("Rice Contri to Inflation.csv",
                                 show_col_types = FALSE)
index[["ipp"]] <- read.xlsx("IPP Wholesale Gap (anomalies).xlsx") |> 
  mutate(year = as.numeric(year)) 
index[["sur"]] <- read.xlsx("stock_use_ratio (1).xlsx") |> 
  mutate(year = as.numeric(year))

cpsi <- index[["prices"]] |> 
  left_join(index[["cpi"]],
            by = c("month"="month",
                   "year" = "year")) |> 
  mutate(year = as.numeric(year),  
         month_num = match(month, month.abb),  
         date = mdy(paste(month,"01",year,sep="-")),
         RealWSale = (rmr_wholesale/Rice.CPI)*100,
         RealRet = (rmr_retail/Rice.CPI)*100) |> 
  filter(year>=1994) |> 
  ungroup() |>
  group_by(month) |> 
  mutate(AveRealRet = mean(RealRet,na.rm=TRUE),
         AveNomRet = mean(rmr_retail,na.rm=TRUE),) |> 
  ungroup() |> 
  mutate(RealPriceDev = (RealRet - AveRealRet)/AveRealRet,
         NomPriceDev = (rmr_retail - AveNomRet)/AveNomRet,
         RealRetGap = (RealRet - RealWSale)/RealWSale,
         NomRetGap = (rmr_retail - rmr_wholesale)/rmr_wholesale) |> 
  ungroup() |> 
  select(date,year,month,rmr_retail,RealRet,rmr_wholesale,RealWSale,Rice.CPI,
         AveRealRet,AveNomRet,RealPriceDev,NomPriceDev,RealRetGap,NomRetGap) |> 
  left_join(index[["inflation"]],
         by = c("year", "month")) |> 
 mutate(contri = (8.87/100)*RiceInflation) |> 
  left_join(index[["ipp"]],
            by = c("year", "month")) |> 
  mutate(year = as.numeric(year)) |> 
  left_join(index[["sur"]],
            by = c("year", "month")) |> 
  mutate(SURindex = (1- total.stocks/lag(total.use,12)),
                     year = as.numeric(year)) |>  
  filter(year>=2016)
  



