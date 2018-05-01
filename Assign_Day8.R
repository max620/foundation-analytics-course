setwd("C:/Users/Max/Documents/R")

pacman::p_load(tidyverse,reshape2,magrittr,lubridate,data.table,plotly,mice,VIM,gridExtra,missMDA) 

DIR = 'C:/Users/Max/Documents/R'
file1 = 'Day8_Assign_Data.csv'
file2 = 'Day8_Assign_Data2.csv'

data = read.csv(file.path(DIR, file1)) 
data2 = read.csv(file.path(DIR, file2)) 

str(data)
dim(data)
names(data)
head(data)
d.miss.1 = melt(apply(data[, -2], 2, function(x) sum(is.na(x) | x=="")))
sum(duplicated(data))

str(data2)
dim(data2)
names(data2)
head(data2)
d.miss.2 = melt(apply(data2[, -2], 2, function(x) sum(is.na(x) | x=="")))
sum(duplicated(data2))

#data$date_time_format = as.Date(data$date_time, format = "%Y-%m-%d") #only show YMD
data$date_time_format_2 = as.POSIXct(data$date_time, format = "%Y-%m-%d %H:%M:%S")
data$date_time_format = ymd_hms(data$date_time)
data$month = month(data$date_time)
data$day = day(data$date_time)
data$hour = hour(data$date_time)
data$min = minute(data$date_time)
data$sec = second(data$date_time)

#data_merge = merge(data,data2, all=TRUE)
data_merge = rbind(data,data2)
dim(data_merge)
#data_merge$date_time_format = as.Date(data_merge$date_time, format = "%Y-%m-%d") #only show YMD
data_merge$date_time_format_2 = as.POSIXct(data_merge$date_time, format = "%Y-%m-%d %H:%M:%S")
data_merge$date_time_format = ymd_hms(data_merge$date_time)
data_merge$month = month(data_merge$date_time)
data_merge$day = day(data_merge$date_time)
data_merge$hour = hour(data_merge$date_time)
data_merge$min = minute(data_merge$date_time)
data_merge$sec = second(data_merge$date_time)
#data$unit_abb = substring(data$unitid,1,2)

data_merge %>% 
  group_by(unitid) %>%
  filter(unitid == 'SS0031') %>%
ggplot(aes(x=hour,y=Temperature,fill=factor(day)))+geom_boxplot()

#plot_ly(data_merge, x = ~hour, type='scatter', mode = 'lines') %>%
# add_trace(y = ~Light, mode='lines+markers') 

#plot_ly(data, x = ~hour, mode = 'lines') %>%
#  add_trace(y = ~Light, mode='markers') 

#plot_ly(data, x = ~date_time_format, mode = 'lines') %>%
#  add_trace(y = ~Light, mode='markers') 

plotly_co2 = plot_ly(data_merge, x = ~date_time_format_2, y = ~Co2, mode = 'lines') %>%
  add_trace(y = ~Co2, color = ~unitid, mode='markers') %>%
  layout(title = 'Co2 and Date',
        xaxis = list(title = 'Date'),
        yaxis = list(title = 'Co2 Reading'))

plotly_noise = plot_ly(data_merge, x = ~date_time_format_2, y = ~Noise, mode = 'lines') %>%
  add_trace(y = ~Noise, color = ~unitid, mode='markers')

plotly_light = plot_ly(data_merge, x = ~date_time_format_2, y = ~Light, mode = 'lines') %>%
  add_trace(y = ~Light, color = ~unitid, mode='markers')

plotly_ln = plot_ly(data, x = ~date_time_format_2, y = ~Humidity, z = ~Light, color = ~unitid, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Date'),
                      yaxis = list(title = 'Humidity'),
                      zaxis = list(title = 'Light')))

LND = plot_ly(data_merge,x = ~date_time_format_2,  y = ~Light, z = ~Noise, color = ~unitid, colors = c("#BF382A", "#0C4B8E", "#7FFF00", "#FFB90F")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Date'),
                      yaxis = list(title = 'Light'),
                      zaxis = list(title = 'Noise')))

CVD = plot_ly(data_merge,x = ~date_time_format_2,  y = ~Co2, z = ~VOC, color = ~unitid, colors = c("#BF382A", "#0C4B8E", "#7FFF00", "#FFB90F")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Date'),
                      yaxis = list(title = 'CO2'),
                      zaxis = list(title = 'VOC')))

HTD = plot_ly(data_merge,x = ~date_time_format_2,  y = ~Humidity, z = ~Temperature, color = ~unitid, colors = c("#BF382A", "#0C4B8E", "#7FFF00", "#FFB90F")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Date'),
                      yaxis = list(title = 'HUMID'),
                      zaxis = list(title = 'TEMP')))


# ts = plot.ts(ts(data_merge$Temperature, start=c(2017,3), frequency=1440))
# 
# ts3 = filter(data_merge, unitid=='SS0050')
# plot.ts(ts(ts3$Temperature, frequency=1440, start=c(2017,3)))
# plot(stl(ts(ts3$Temperature, frequency=1440, start=c(2017,3)), s.window="periodic"))

data_ss0029 = subset(data_merge, unitid=='SS0029')
data_ss0031 = subset(data_merge, unitid=='SS0031')
data_ss0036 = subset(data_merge, unitid=='SS0036')
data_ss0050 = subset(data_merge, unitid=='SS0050')

#ts_plot = par(mfrow=c(2,3))
#facet_wrap for int value
ts_temp = ggplot(data_merge, aes(date_time_format, Temperature, color=unitid)) + geom_line() #or + geom_point()
ts_noise = ggplot(data_merge, aes(date_time_format, Noise, color=unitid)) + geom_line()
ts_light = ggplot(data_merge, aes(date_time_format, Light, color=unitid)) + geom_line()
ts_co2 = ggplot(data_merge, aes(date_time_format, Co2, color=unitid)) + geom_line()
ts_voc = ggplot(data_merge, aes(date_time_format, VOC, color=unitid)) + geom_line()
ts_humid = ggplot(data_merge, aes(date_time_format, Humidity, color=unitid)) + geom_line()
ts_plot = grid.arrange(ts_temp, ts_noise, ts_light, ts_co2, ts_voc, ts_humid, nrow=6)

ts_temp_2 = ggplot(data_merge, aes(date_time_format, Temperature, color=unitid)) + geom_line() + facet_grid(unitid ~.)
ts_light_2 = ggplot(data_merge, aes(date_time_format, Light, color=unitid)) + geom_line() + facet_grid(unitid ~.)
ts_co2_2 = ggplot(data_merge, aes(date_time_format, Co2, color=unitid)) + geom_line() + facet_grid(unitid ~.)

ts_co2_3 = ggplot(data_ss0029, aes(day, Co2, color=unitid)) + geom_point() + facet_wrap(~hour)
ts_light_3 = ggplot(data, aes(x=hour, y=Light, color=factor(unitid))) + geom_point() + facet_wrap(~date_time_format)
ts_noise_3 = ggplot(data, aes(x=hour, y=Noise, color=factor(unitid))) + geom_point() + facet_wrap(~date_time_format)

md.pattern(data_merge)
md.pairs(data_merge)
methods(mice)

plot(zoo(data$date_time_format_2))

data_merge$date_time_format = format(data$dateTime,tz="Asia/Kuala_Lumpur")
with_tz(data_merge, "Asia/Kuala_Lumpur")