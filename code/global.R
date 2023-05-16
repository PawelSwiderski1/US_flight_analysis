library("dplyr")
library("ggplot2")
library("ggrepel")
library("stringr")
library("ggpubr")
library("tidyr")
library("scales")

airports <- read.csv("dataverse_files/airports.csv")
carriers <- read.csv("dataverse_files/carriers.csv")
plane_data <- read.csv("dataverse_files/plane-data.csv")
variable_descriptions <- read.csv("dataverse_files/variable-descriptions.csv")


#df_2008 <- read.csv("dataverse_files/2008.csv")
df_2007 <- read.csv("dataverse_files/2007.csv")
#df_2006 <- read.csv("dataverse_files/2006.csv")

df <- df_2007

df$DayOfWeek_name <- recode(df$DayOfWeek, 
                        "7"="Sunday",
                        "1"="Monday",
                        "2"="Tuesday",
                        "3"="Wednesday",
                        "4"="Thursday",
                        "5"="Friday",
                        "6"="Saturday")
df$DayOfWeek_name <- factor(df$DayOfWeek_name,
                                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

df$Month_name <- recode(df$Month, 
                       "1"="January",
                       "2"="February",
                       "3"="March",
                       "4"="April",
                       "5"="May",
                       "6"="June",
                       "7"="July",
                       "8"="August",
                       "9"="September",
                       "10"="October",
                       "11"="November",
                       "12"="December")
df$Month_name <- factor(df$Month_name, 
                        levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

df$CRSDepTime <- str_pad(df$CRSDepTime, 4, side="left", pad="0")
df$CRSHour <- substring(df$CRSDepTime, 1, 2) 

#df$CRSHour <- ifelse(strtoi(df$CRSHour) > 23, substring(df$CRSDepTime, 1, 1), df$CRSHour)  
#df$CRSHour <- factor(df$CRSHour, levels =
                       #c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))

df$AvgDelay <- rowMeans(df[,c("DepDelay", "ArrDelay")])

carriers$Description[carriers$Code == "HP"] <- "America West Airlines Inc."
carriers$Description[carriers$Code == "US"] <- "US Airways Inc."

df_carrierDelay <- merge(df, carriers, by.x = "UniqueCarrier", by.y = "Code", all.x = TRUE)

delay_weekday <- df %>% 
  group_by(DayOfWeek) %>% 
  summarize(avg_delay = round(mean(AvgDelay, na.rm=TRUE),2))

delay_hour <- df %>% 
  group_by(CRSHour) %>% 
  summarize(avg_delay = round(mean(AvgDelay, na.rm=TRUE),2))

delay_month <- df %>% 
  group_by(Month_name) %>% 
  summarize(avg_delay = round(mean(AvgDelay, na.rm=TRUE),2))

# write.csv(delay_weekday, "Studia/PDU_Projekt2/delay_weekday.csv", row.names = FALSE)
# write.csv(delay_hour, "Studia/PDU_Projekt2/delay_hour.csv", row.names = FALSE)
# write.csv(delay_month, "Studia/PDU_Projekt2/delay_month.csv", row.names = FALSE)

df %>% 
  group_by(DayOfWeek) %>% 
  summarize(n = n()) %>% 
  arrange(n)

df %>% 
  group_by(Month_name) %>% 
  summarize(n = n()) %>% 
  arrange(n)

df %>% 
  group_by(CRSHour) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  print(n=24)

df %>% 
  group_by(Month_name) %>% 
  summarize(avg_delay = mean(AvgDelay, na.rm=TRUE), avg_weather_delay = mean(WeatherDelay, na.rm=TRUE)) %>% 
  pivot_longer(cols = c("avg_delay","avg_weather_delay"), values_to = "delay", names_to = "delay_type") %>% 
  mutate(delay = ifelse(delay_type == "avg_weather_delay",delay*10,delay)) %>% 
  arrange(delay) %>% 
  print(n=24)

weekdayDelayPlot <- df %>% 
  group_by(DayOfWeek_name) %>% 
  summarize(avg_delay = mean(AvgDelay, na.rm=TRUE)) %>% 
  ggplot(aes(x=DayOfWeek_name, y=avg_delay)) +
  geom_bar(stat='identity', aes(fill = avg_delay == min(avg_delay))) +
  scale_x_discrete("Day") +
  scale_y_continuous("Average delay", expand = expansion(mult = c(0,0.1))) +
  theme_classic() +
  theme(plot.margin = margin(1,0.5,1,0, unit = 'cm'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 14, color="#000000",margin=margin(0,8,0,0)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, face='bold', margin=margin(5,0,0,0)),
        axis.ticks.x = element_line(linewidth = 1.4),
        legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill='transparent')) +
  guides(fill = guide_legend(byrow = TRUE))
weekdayDelayPlot


weekdayTrafficPlot <- df %>% 
  group_by(DayOfWeek_name) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x=DayOfWeek_name, y=count)) +
  geom_bar(stat='identity', aes(fill = count == min(count))) +
  scale_x_discrete("Day") +
  scale_y_continuous("Number of flights", expand = expansion(mult = c(0,0.1))) +
  theme_classic() +
  theme(plot.margin = margin(1,0,1,0, unit = 'cm'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 14, color="#000000",margin=margin(0,8,0,0)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, face='bold', margin=margin(5,0,0,0)),
        axis.ticks.x = element_line(linewidth = 1.4),
        legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill='transparent')) +
  guides(fill = guide_legend(byrow = TRUE))
weekdayTrafficPlot

weekday_delay_comb <- ggarrange(weekdayDelayPlot, weekdayTrafficPlot)

monthDelayPlot <- df %>% 
  group_by(Month_name) %>% 
  summarize(avg_delay = mean(AvgDelay, na.rm=TRUE), avg_weather_delay = mean(WeatherDelay, na.rm=TRUE)) %>% 
  pivot_longer(cols = c("avg_delay","avg_weather_delay"), values_to = "delay", names_to = "delay_type") %>% 
  mutate(delay = ifelse(delay_type == "avg_weather_delay",delay*12,delay)) %>% 
  ggplot(aes(x=Month_name, y=delay)) +
  geom_bar(aes(fill = delay_type), stat='identity',position = 'dodge') +
  scale_x_discrete("Month") +
  scale_y_continuous("Average delay", trans=pseudo_log_trans(base = 2), expand = expansion(mult = c(0,0.1))) +
  scale_fill_discrete(labels = c("delay", "weather delay")) +
  theme_classic() +
  theme(plot.margin = margin(1,0,1,0, unit = 'cm'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 14, color="#000000",margin=margin(0,8,0,0)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8, face='bold', margin=margin(5,0,0,0)),
        axis.ticks.x = element_line(linewidth = 1.4),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.margin = margin(1,0,1,1),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill='transparent')) +
  guides(fill = guide_legend(byrow = TRUE)) 

monthDelayPlot

monthWeatherDelayPlot <- df %>% 
  group_by(Month_name) %>% 
  summarize(avg_weather_delay = mean(WeatherDelay, na.rm=TRUE)) %>% 
  ggplot(aes(x=Month_name, y=avg_weather_delay)) +
  geom_bar(stat='identity', aes(fill = avg_weather_delay == min(avg_weather_delay))) +
  scale_x_discrete("Month") +
  scale_y_continuous("Average weather delay (in minutes)", expand = expansion(mult = c(0,0.1))) +
  theme_classic() +
  theme(plot.margin = margin(1,1,1,1, unit = 'cm'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 20, color="#000000",margin=margin(0,15,0,0)),
        axis.title.x = element_text(size = 25, color="#000000",margin=margin(15,0,0,0)),
        axis.text.x = element_text(size = 15, face='bold', margin=margin(5,0,0,0)),
        axis.ticks.x = element_line(linewidth = 1.4),
        legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_rect(fill='transparent')) +
  guides(fill = guide_legend(byrow = TRUE))
monthWeatherDelayPlot

month_delay_comb <- ggarrange(monthDelayPlot, monthWeatherDelayPlot)
