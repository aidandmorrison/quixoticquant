library(tidyverse)
library(stringr)
library(gdata)
library(lubridate)
library(plotly)
library(UtilsQQ)
library(ggplot2)
library(nnls)


sheet_310101 <- readxl::read_xls("data/310101.xls", sheet = 2)
sheet_310101 <- good_names(sheet = sheet_310101)
meta <- stash_meta(sheet_310101)
meta <- good_names(meta, meta = TRUE)
sheet_310101 <- cut_meta(sheet_310101)
sheet_310101 <- good_date(sheet_310101)
sheet_310101 <- to_numeric(sheet_310101)
sheet_310101 <- nom_units(sheet_310101, meta)
sheet_310101 <- inst_ann(sheet_310101, meta)
sheet_310101 <- prev_12_month(sheet_310101, meta)

sheet_340101 <- readxl::read_xls("data/340101.xls", sheet = 2)

sheet_340101 <- good_names(sheet = sheet_340101)
meta1 <- stash_meta(sheet_340101)
meta1 <- good_names(meta1, meta = TRUE)
meta <- bind_rows(meta, meta1)
sheet_340101 <- cut_meta(sheet_340101)
sheet_340101 <- good_date(sheet_340101)
sheet_340101 <- to_numeric(sheet_340101)
sheet_340101 <- nom_units(sheet_340101, meta)
sheet_340101 <- inst_ann(sheet_340101, meta)
sheet_340101 <- prev_12_month(sheet_340101, meta)

sheet_340102 <- readxl::read_xls("data/340102.xls", sheet = 2)

sheet_340102 <- good_names(sheet = sheet_340102)
meta2 <- stash_meta(sheet_340102)
meta2 <- good_names(meta2, meta = TRUE)
meta <- bind_rows(meta, meta2)
sheet_340102 <- cut_meta(sheet_340102)
sheet_340102 <- good_date(sheet_340102)
sheet_340102 <- to_numeric(sheet_340102)
sheet_340102 <- nom_units(sheet_340102, meta)
sheet_340102 <- inst_ann(sheet_340102, meta)
sheet_340102 <- prev_12_month(sheet_340102, meta)


sheet_310101 <- sheet_310101 %>% 
  mutate(NOM.12.12.Pre.12 = (case_when(Date < "2006-09-01 GMT" ~ Net.Ove.Mig.Aus.Pre.12m)),
         NOM.12.16.Pre.12 = (case_when(Date >= "2006-09-01 GMT" ~ Net.Ove.Mig.Aus.Pre.12m)),
         NOM.12.16.Pre.12.Pre.3yr = (case_when(Date <= "2006-09-01 GMT" & Date >= "2003-12-01 GMT" ~ Net.Ove.Mig.Aus.Pre.12m*1.25)),
         NOM.12.12.Pre.12.Projection = (case_when(Date >= "2006-09-01 GMT" ~ Net.Ove.Mig.Aus.Pre.12m*0.8)))

sheet_arr_dep <- bind_cols(sheet_340101, sheet_340102)
sheet_arr_dep <- sheet_arr_dep %>% 
  mutate(net.movements = (Num.of.mov.Tot.Arr - Num.of.mov.Tot.Dep),
         net.per.long.term = (Num.of.mov.Per.and.Lon.ter.Arr - Num.of.mov.Per.and.Lon.ter.Dep),
         net.movements.Pre.12m = (Num.of.mov.Tot.Arr.Pre.12m - Num.of.mov.Tot.Dep.Pre.12m)
  )

match_sheet_arr_dep <- sheet_arr_dep %>% 
  filter(Date %in% sheet_310101$Date)

joint_sheet_310101 <- sheet_310101 %>% 
  inner_join(match_sheet_arr_dep, by = "Date")

joint_sheet_310101 <- joint_sheet_310101 %>% 
  mutate(Net.Ove.Mig.Aus.Pre.12m.per.ERP = (Net.Ove.Mig.Aus.Pre.12m/Est.Res.Pop.ERP.Aus),
         net.movements.Pre.12m.per.ERP = (net.movements.Pre.12m/Est.Res.Pop.ERP.Aus),
         cum.net.movements = cumsum(net.movements.Pre.12m/4),
         cum.Net.Ove.Mig.Aus = cumsum(Net.Ove.Mig.Aus),
         cum.discrepancy = cum.Net.Ove.Mig.Aus - cum.net.movements,
         Phy.Pre.Pop.PPP.Aus = Est.Res.Pop.ERP.Aus - cum.discrepancy,
         Net.Ove.Mig.Aus.Pre.12m.per.PPP = Net.Ove.Mig.Aus.Pre.12m/Phy.Pre.Pop.PPP.Aus,
         net.movements.Pre.12m.per.PPP = net.movements.Pre.12m/Phy.Pre.Pop.PPP.Aus
  )

datevec <- sheet_340101 %>% 
  filter(Date < "2006-09-01 GMT")%>% 
  select(Date)

date_change <- length(datevec$Date) +1


plot <- joint_sheet_310101 %>% 
  #filter(Date < "2006-06-02 GMT") %>% 
  ggplot(aes(x = Date, text = as_date(Date)))+
  geom_rect(xmin = as.numeric(as.POSIXct(as_date("2009-12-01 GMT"))), 
            xmax = as.numeric(as.POSIXct(as_date("2016-06-01 GMT"))), 
            ymin = 100000, ymax = 200000,
            fill = "yellow", alpha = 0.1)+
  geom_line(aes(y = NOM.12.12.Pre.12, col = "NOM 12/12 Definition"), size = .8)+
  geom_line(aes(y = NOM.12.16.Pre.12, col = "NOM 12/16 Definition"), size = .8)+
  geom_line(aes(y = net.movements.Pre.12m, col = "Net Movements"), size =.8)+
  geom_line(aes(y = NOM.12.16.Pre.12.Pre.3yr, col = "NOM 12/16 Rule Backcast"), size =.8)+
  scale_colour_manual("", 
                      breaks = c("12/12 Rule Definition of NOM", "12/16 Rule Definition of NOM", "Net Movements", "NOM 12/16 Rule Backcast"),
                      values = c( "blue", "red",  "magenta1", "orange"))+
  labs(title = "Net Movements vs Net Overseas Migration")+
  geom_vline(xintercept = as.numeric(sheet_340101$Date[date_change]), linetype = 4) +
  scale_y_continuous("Number")+
  scale_y_continuous(labels = scales::comma) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

ggplotly(plot, tooltip = c("text", "y")) %>% 
  layout(margin=list(l=100, t = 100, b = 70, r = 100),
         legend = list(y = 0.95, x = 0.3))

plot <- joint_sheet_310101 %>% 
  #filter(Date < "2006-06-02 GMT") %>% 
  ggplot(aes(x = Date, text = as_date(Date)))+
  geom_rect(xmin = as.numeric(as.POSIXct(as_date("2009-12-01 GMT"))), 
            xmax = as.numeric(as.POSIXct(as_date("2016-06-01 GMT"))), 
            ymin = 100000, ymax = 200000,
            fill = "yellow", alpha = 0.1)+
  geom_line(aes(y = NOM.12.12.Pre.12, col = "NOM 12/12 Definition"), size = .8)+
  geom_line(aes(y = NOM.12.16.Pre.12, col = "NOM 12/16 Definition"), size = .8)+
  geom_line(aes(y = net.movements.Pre.12m, col = "Net Movements"), size =.8)+
  geom_line(aes(y = NOM.12.12.Pre.12.Projection, col = "NOM 12/12 Rule Projection"), size =.8)+
  scale_colour_manual("", 
                      breaks = c("12/12 Rule Definition of NOM", "12/16 Rule Definition of NOM", "Net Movements", "NOM 12/16 Rule Backcast"),
                      values = c( "blue", "red",  "orange", "magenta1"))+
  labs(title = "Net Movements vs Net Overseas Migration")+
  geom_vline(xintercept = as.numeric(sheet_340101$Date[date_change]), linetype = 4) +
  scale_y_continuous("Number")+
  scale_y_continuous(labels = scales::comma) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

ggplotly(plot, tooltip = c("text", "y")) %>% 
  layout(margin=list(l=100, t = 100, b = 70, r = 100),
         legend = list(y = 0.95, x = 0.3))

