library(tidyverse)
library(dplyr)

schema_url <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/datastructure/ESTAT/DSD_prc_hicp_manr"

#install.packages("eurostat")
library("eurostat")
library("data.table")
library("zoo")
library("pdfetch")

library(eurostat)
library(rvest)
library(knitr)

toc <- get_eurostat_toc()

search_eurostat("Harmonised index of consumer")

dat <- get_eurostat("ei_cphi_m", time_format = "date")

countries=c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV",
            "LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE")

datt = filter(dat, geo %in% countries & time > "2007-01-01")

data= filter(datt, indic == "CP-HI00" & unit== "RT12")

stats <- data.table(data)

quantile(stats$values, probs = 0.25, na.rm = TRUE, names = TRUE)

stats[,list(quant.0 = quantile(values, probs = 0, na.rm = TRUE, names = TRUE)[[1]]), by = list(time)]

data_quantiles <- stats[, list(
              quant.0 = quantile(values, probs = 0, na.rm = TRUE, names = TRUE)[[1]],
              quant.25 = quantile(values, probs = 0.25, na.rm = TRUE, names = TRUE)[[1]],
              quant.50 = quantile(values, probs = 0.50, na.rm = TRUE, names = TRUE)[[1]],
              quant.75 = quantile(values, probs = 0.75, na.rm = TRUE, names = TRUE)[[1]],
              quant.100 = quantile(values, probs = 1, na.rm = TRUE, names = TRUE)[[1]]), by = list(time)]

ggplot(data = data_quantiles, aes(x = time)) +
  geom_ribbon(aes(ymin = quant.0, ymax = quant.25, fill = '0% — 25%'), alpha=0.3) +
  geom_ribbon(aes(ymin = quant.25, ymax = quant.50, fill = '25% — 50%'), alpha=0.3) +
  geom_ribbon(aes(ymin = quant.50, ymax = quant.75, fill = '50% — 75%'), alpha=0.3) +
  geom_ribbon(aes(ymin = quant.75, ymax = quant.100, fill = '75% — 100%'), alpha=0.3)

ggplot(data = data_quantiles, aes(x = time)) +
  geom_ribbon(aes(ymin = quant.0, ymax = quant.25, fill = '0% — 25%'), alpha=0.3) +
  geom_ribbon(aes(ymin = quant.25, ymax = quant.50, fill = '25% — 50%'), alpha=0.3) +
  geom_ribbon(aes(ymin = quant.50, ymax = quant.75, fill = '50% — 75%'), alpha=0.3) +
  geom_ribbon(aes(ymin = quant.75, ymax = quant.100, fill = '75% — 100%'), alpha=0.3)+
  geom_line(data = stats[geo == 'FR'], 
            aes(x = time, y = values, fill='France'), size = 1, alpha=0.7, color="blue")+
  geom_line(data = stats[geo == 'ES'], 
            aes(x = time, y = values, fill='Spain'), size = 1, alpha=0.7, color="red")



ggplot(data = data_quantiles, aes(x = time)) +
  
  # Sets data_quantiles as the dataset to use and the date as the x axis
  geom_ribbon(aes(ymin = quant.0, ymax = quant.25, fill = '0% — 25%'), alpha=0.3) +
  geom_ribbon(aes(ymin = quant.25, ymax = quant.50, fill = '25% — 50%'), alpha=0.3) +
  geom_ribbon(aes(ymin = quant.50, ymax = quant.75, fill = '50% — 75%'), alpha=0.3) +
  geom_ribbon(aes(ymin = quant.75, ymax = quant.100, fill = '75% — 100%'), alpha=0.3)+
  # The four lines above create filled in areas to visualize the quantile distribution
  # while the alpha sets the graph objects semi-transparent
  
  # Create a line for Spain and France in order to compare with the quantile distribution
  geom_line(data = stats[geo == 'FR'], aes(x = time, y = values, fill='France'), size = 1, alpha=0.7, color="blue")+
  geom_line(data = stats[geo == 'ES'], aes(x = time, y = values, fill='Spain'), size = 1, alpha=0.7, color="red")+
  
  # Customize colors for ribbons and countries
  scale_fill_manual(values = c("#01A9DB", "#086A87", "#086A87", "#01A9DB", "blue","red"))+
  
  # Display y=0 line
  geom_hline(aes(yintercept=0), colour="black",size=1, alpha=0.5)+
  
  # Display light gray grid
  theme_bw()+theme(panel.grid.major=element_line(size=0.3, colour='grey92'))+
  
  # Remove fill header from legend
  guides(fill=guide_legend(title=NULL))+
  
  # Set Y title
  ylab("Annual % Change")+
  
  # Remove outer frame
  theme(axis.title.x = element_blank(),panel.border = element_blank())+
  
  # Set plot title
  ggtitle("HICP")+
  
  # Center title
  theme(plot.title = element_text(hjust = 0.5))




























