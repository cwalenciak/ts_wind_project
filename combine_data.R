library(tswge)
library(ncdf4)


#setwd("C:/Users/cwale/OneDrive/Desktop/TS Project")

# Call support function file

source("Support/wind_support_functions.R")
source("Support/ggplot_corr_plot.R")

# retrieve a list of nc files in my data folder
flist <- list.files(path = "Data", pattern = "^.*\\.(nc|NC|Nc|Nc)$")



keep_cols <- c( "horizontal_wspd", 
                "horizontal_wdir", 
                "air_pressure", 
                "relative_humidity"
                )



time_span = (24 * 60 * 60)

# flist, fname, time_col, time_span, time_interval, keep_cols
df <- combine_files(flist, "Data", "time_offset", time_span, 60, 4, keep_cols)


hist(df$horizontal_wspd)
plot(df$horizontal_wspd, type = "l")

apr1 = df[which(df$date == "2016-04-01 EDT"),]
apr2 = df[which(df$date == "2016-04-02 EDT"),]
apr3 = df[which(df$date == "2016-04-03 EDT"),]
apr20 = df[which(df$date == "2016-04-20 EDT"),]

library(ggplot2)
p1 = ggplot(data = apr20, aes(x = seq(1,1440,1), y = horizontal_wspd)) + 
  geom_line()+ 
  geom_hline(yintercept = mean(apr20$horizontal_wspd), color = "red", show.legend = TRUE)+
  xlab("Time (minutes)")+
  ylab("Wind Speed (m/s)")+
  ggtitle("Horizontal Wind Speed in m/s 20 April 2016",subtitle = "Altitude of 110m and 1 minute Intervals")+
  theme_minimal()

p2 = ggplot(data = df, aes(x = group, y = horizontal_wspd, group = as.POSIXct(date))) + 
  geom_line(aes(color = as.POSIXct(date)))+
  geom_hline(yintercept = mean(df$horizontal_wspd), color = "red", show.legend = TRUE)+
  xlab("Time (minutes)")+
  ylab("Wind Speed (m/s)")+
  labs(color = "Date")+
  ggtitle("Horizontal Wind Speed in m/s 01-20 April 2016 (By Day)",subtitle = "Altitude of 110m and 1 minute Intervals")+
  theme_minimal()



acf1_12 = acf(apr20$horizontal_wspd[1:720])
acf12_24 = acf(apr20$horizontal_wspd[720:1440])


ggplot.corr(data = apr20$horizontal_wspd[720:1440], lag.max = 24, ci= 0.95, large.sample.size = FALSE, horizontal = FALSE)


forecast_arma_1_2 = fore.arma.wge(apr20$horizontal_wspd, phi = arma1_2$phi, theta = arma1_2$theta, n.ahead = 10, lastn = TRUE)

plot(x = seq(1400,1440,1), y = apr20$horizontal_wspd[1400:1440], ylim = c(5,7))
lines(x = seq(1400,1440,1), y = apr20$horizontal_wspd[1400:1440])
lines(x = seq(1431,1440,1), y = forecast_arma_1_2$f, color = "red")
ASE1_2 = mean((tail(apr20$horizontal_wspd,10)-forecast_arma_1_2$f)^2)

forecast_arma_3_1 = fore.arma.wge(apr20$horizontal_wspd, phi = arma3_1$phi, theta = arma3_1$theta, n.ahead = 10, lastn = TRUE)

plot(x = seq(1400,1440,1), y = apr20$horizontal_wspd[1400:1440], ylim = c(5,7))
lines(x = seq(1400,1440,1), y = apr20$horizontal_wspd[1400:1440])
lines(x = seq(1431,1440,1), y = forecast_arma_3_1$f)
ASE3_1 = mean((tail(apr20$horizontal_wspd,10)-forecast_arma_3_1$f)^2)
