library("dplyr")
library("ggplot2")
library("ggrepel")
library("stringr")
library("ggpubr")
library("tidyr")
library("scales")
library("rgdal")
library("maptools")
library("usmap")
library("maps")

airports <- read.csv("dataverse_files/airports.csv")
carriers <- read.csv("dataverse_files/carriers.csv")
plane_data <- read.csv("dataverse_files/plane-data.csv")
variable_descriptions <- read.csv("dataverse_files/variable-descriptions.csv")



df_2007 <- read.csv("dataverse_files/2007.csv")
df_2006 <- read.csv("dataverse_files/2006.csv")

df <- df_2007
#df <- rbind(df_2007,df_2006)

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

routes<-df
routes$route<-paste(df$Origin,df$Dest,sep="-")
routes_delays_number<-filter(routes,routes$AvgDelay>0)
routes_delays_number<-group_by(routes_delays_number,route)
routes_delays_number<-summarise(routes_delays_number,Mean_delay=mean(AvgDelay),Delays_number=length(AvgDelay))
routes2<-group_by(routes,route)
routes2<-summarise(routes2,flights_number=length(route))
routes3<-distinct(routes[,c('route','Origin','Dest')])

routes<-merge(routes3,routes2,by='route')
route_delays<-merge(routes, routes_delays_number,by='route')
head(route_delays,10)
route_delays$delays_ratio<-route_delays$Delays_number/route_delays$flights_number
routes_top_10_mean_delays<-head(as.data.frame(arrange(filter(route_delays,flights_number>100),-Mean_delay)),10)
routes_top_10_mean_delays<-merge(routes_top_10_mean_delays,airports,by.x='Origin',by.y='iata')
routes_top_10_mean_delays<-routes_top_10_mean_delays[,c('Origin','route','Dest','Mean_delay','lat','long','state')]
colnames(routes_top_10_mean_delays)[c(5,6,7)]<-c('Origin_lat','Origin_long','Origin_state')
routes_top_10_mean_delays<-merge(routes_top_10_mean_delays,airports,by.x='Dest',by.y='iata')
routes_top_10_mean_delays<-routes_top_10_mean_delays[,c('Origin','route','Dest','Mean_delay','Origin_lat','Origin_long','Origin_state','lat','long','state')]
colnames(routes_top_10_mean_delays)[c(8,9,10)]<-c('Dest_lat','Dest_long','Dest_state')
routes_top_10_mean_delays<-as.data.frame(lapply(routes_top_10_mean_delays,rep,2))
routes_top_10_mean_delays[seq(11,20,1),'Origin_lat']<-routes_top_10_mean_delays[seq(1,10,1),'Dest_lat']
routes_top_10_mean_delays[seq(11,20,1),'Origin_long']<-routes_top_10_mean_delays[seq(11,20,1),'Dest_long']
routes_top_10_mean_delays[seq(11,20,1),'Dest_lat']<-routes_top_10_mean_delays[seq(1,10,1),'Origin_lat']
routes_top_10_mean_delays[seq(11,20,1),'Dest_long']<-routes_top_10_mean_delays[seq(1,10,1),'Origin_long']

routes_top_10_mean_delays<-usmap_transform(routes_top_10_mean_delays,input_names=c("Origin_long","Origin_lat"),output_names=c("x","y"))
routes_top_10_mean_delays<-usmap_transform(routes_top_10_mean_delays,input_names=c("Dest_long","Dest_lat"),output_names=c("x_2","y_2"))

routesDelayPlot <- plot_usmap()+geom_point(aes(x,y),colour='red',data=routes_top_10_mean_delays)+geom_segment(aes(x=x,y=y,xend=x_2,yend=y_2),colour='red',data=routes_top_10_mean_delays)


# routes_top_10_ratio_arr_delays<-head(as.data.frame(arrange(filter(route_delays,flights_number>100),-delays_ratio)),10)
# routes_top_10_ratio_arr_delays<-merge(routes_top_10_ratio_delays,airports,by.x='Origin',by.y='iata')
# routes_top_10_ratio_arr_delays<-routes_top_10_ratio_delays[,c('Origin','route','Dest','Arr_delays_ratio','lat','long','state')]
# colnames(routes_top_10_ratio_arr_delays)[c(5,6,7)]<-c('Origin_lat','Origin_long','Origin_state')
# routes_top_10_ratio_arr_delays<-merge(routes_top_10_ratio_arr_delays,airports,by.x='Dest',by.y='iata')
# routes_top_10_ratio_arr_delays<-routes_top_10_ratio_arr_delays[,c('Origin','route','Dest','Arr_delays_ratio','Origin_lat','Origin_long','Origin_state','lat','long','state')]
# colnames(routes_top_10_ratio_arr_delays)[c(8,9,10)]<-c('Dest_lat','Dest_long','Dest_state')
# routes_top_10_ratio_arr_delays<-as.data.frame(lapply(routes_top_10_ratio_arr_delays,rep,2))
# routes_top_10_ratio_arr_delays[seq(11,20,1),'Origin_lat']<-routes_top_10_ratio_arr_delays[seq(1,10,1),'Dest_lat']
# routes_top_10_ratio_arr_delays[seq(11,20,1),'Origin_long']<-routes_top_10_ratio_arr_delays[seq(11,20,1),'Dest_long']
# routes_top_10_ratio_arr_delays[seq(11,20,1),'Dest_lat']<-routes_top_10_ratio_arr_delays[seq(1,10,1),'Origin_lat']
# routes_top_10_ratio_arr_delays[seq(11,20,1),'Dest_long']<-routes_top_10_ratio_arr_delays[seq(1,10,1),'Origin_long']
# 
# routes_top_10_ratio_arr_delays<-usmap_transform(routes_top_10_ratio_arr_delays,input_names=c("Origin_long","Origin_lat"),output_names=c("x","y"))
# routes_top_10_ratio_arr_delays<-usmap_transform(routes_top_10_ratio_arr_delays,input_names=c("Dest_long","Dest_lat"),output_names=c("x_2","y_2"))
# 
# plot_usmap()+geom_point(aes(x,y),colour='red',data=routes_top_10_ratio_arr_delays)+geom_segment(aes(x=x,y=y,xend=x_2,yend=y_2),colour='red',data=routes_top_10_ratio_arr_delays)
# 

#Opóźnienia na lotniskach
airports2<-df
enplanments<-length(df$Year)
airports_departures<-group_by(airports2,Origin)
airports_departures<-summarise(airports_departures,departing_flights_number=length(Origin),mean_dep_delay=mean(DepDelay,na.rm=TRUE))
airports_arrivals<-group_by(airports2,Dest)
airports_arrivals<-summarise(airports_arrivals,arriving_flights_number=length(Dest),mean_arr_delay=mean(ArrDelay,na.rm=TRUE))
airports_dep_delays_number<-filter(airports2,DepDelay>0)
airports_dep_delays_number<-group_by(airports_dep_delays_number,Origin)
airports_dep_delays_number<-summarise(airports_dep_delays_number,dep_delayed_flights_number=length(Origin))
airports_arr_delays_number<-filter(airports2,ArrDelay>0)
airports_arr_delays_number<-group_by(airports_arr_delays_number,Dest)
airports_arr_delays_number<-summarise(airports_arr_delays_number,arr_delayed_flights_number=length(Origin))
airports_delays_number<-merge(airports_arr_delays_number,airports_dep_delays_number,by.x='Dest',by.y='Origin')
airports_delays_number<-merge(airports_delays_number,airports_arrivals,by='Dest')
airports_delays_number<-merge(airports_delays_number,airports_departures,by.x='Dest',by.y='Origin')
head(airports_delays_number,10)
colnames(airports_delays_number)[1]<-"Airport"
airports_delays_number$flights_number<-airports_delays_number$arriving_flights_number+airports_delays_number$departing_flights_number
airports_delays_number$dep_delays_ratio<-airports_delays_number$dep_delayed_flights_number/airports_delays_number$departing_flights_number
airports_delays_number$arr_delays_ratio<-airports_delays_number$arr_delayed_flights_number/airports_delays_number$arriving_flights_number
airports_delays_number$all_delays_number<-airports_delays_number$arr_delayed_flights_number+airports_delays_number$dep_delayed_flights_number
airports_delays_number$all_delays_ratio<-airports_delays_number$all_delays_number/airports_delays_number$flights_number
airports_delays_number$airport_category<-"0"
airports_delays_number1<-filter(airports_delays_number, flights_number<1000)
airports_delays_number1$airport_category<-"Nonprimary"
airports_delays_number2<-filter(airports_delays_number,flights_number>=1000 & flights_number<0.0005*enplanments)
airports_delays_number2$airport_category<-"Primary Nonhub"
airport_delays<-merge(airports_delays_number1,airports_delays_number2,all.x=TRUE,all.y=TRUE)
airports_delays_number3<-filter(airports_delays_number,flights_number>=enplanments*0.0005 & flights_number<enplanments*0.0025)
airports_delays_number3$airport_category<-"Small Hub"
airports_delays_number4<-filter(airports_delays_number,flights_number>=enplanments*0.0025 & flights_number<0.01*enplanments)
airports_delays_number4$airport_category<-"Medium Hub"
airports_delays_number5<-filter(airports_delays_number,flights_number>=enplanments*0.01)
airports_delays_number5$airport_category<-"Large Hub"
airport_delays<-merge(airport_delays,airports_delays_number3,all.x=TRUE,all.y=TRUE)
airport_delays<-merge(airport_delays,airports_delays_number4,all.x=TRUE,all.y=TRUE)
airport_delays<-merge(airport_delays,airports_delays_number5,all.x=TRUE,all.y=TRUE)
# airport_codes<-airport_codes[,c('iso_country','iso_region','iata_code','latitude_deg','longitude_deg')]
# airport_codes<-filter(airport_codes,iata_code!='')
airport_codes<-airports[,c('iata','country','state','lat','long')]
airport_codes<-filter(airport_codes,iata!='',country=='USA')
# airport_delays<-merge(airport_delays,airport_codes,by.x='Airport',by.y='iata_code')
airport_delays<-merge(airport_delays,airport_codes,by.x='Airport',by.y='iata')


#write.csv(airport_delays,'C:/mojepliki/PW/PDU/dataverse_files/2000_lotniska_opoznienia.csv')
#airport_delays<-read.csv('G:/dataverse_files/2000_lotniska_opoznienia.csv')
airport_delays_by_category<-group_by(airport_delays,airport_category)
airport_delays_by_category<-summarise(airport_delays_by_category,dep_delay_ratio=sum(dep_delayed_flights_number)/sum(departing_flights_number),arr_delay_ratio=sum(arr_delayed_flights_number)/sum(arriving_flights_number),overall_delay_ratio=sum(all_delays_number)/sum(flights_number))
#barplot(airport_delays_by_category$overall_delay_ratio, main="Opóźnienia według kategorii lotniska w 1990r.", xlab="Odsetek lotów opóźnionych",ylab="Kategoria lotniska",names.arg = airport_delays_by_category$airport_category,horiz = TRUE,las=1,xlim=c(0,0.5),col=topo.colors(5))
#legend("topright",legend=c("Nonprimary","Primary Nonhub","Small Hub","Medium Hub","Large Hub"),fill=TRUE,col=topo.colors(5))
airports_top_10_dep_delays_ratio<-head(as.data.frame(arrange(airport_delays,-dep_delays_ratio)),10)
airports_top_10_arr_delays_ratio<-head(as.data.frame(arrange(airport_delays,-arr_delays_ratio)),10)
airports_top_10_all_delays_ratio<-head(as.data.frame(arrange(airport_delays,-all_delays_ratio)),10)
airports_delays_ratio_by_category_2000<-airport_delays_by_category
colnames(airports_delays_ratio_by_category_2000)<-c("Airport_category","dep_delay_ratio_2000","arr_delay_ratio_2000","overall_delay_ratio_2000")

airports_top_10_dep_delays_mean<-head(as.data.frame(arrange(airport_delays,-mean_dep_delay)),10)
airports_top_10_arr_delays_mean<-head(as.data.frame(arrange(airport_delays,-mean_arr_delay)),10)
airport_mean_delays_by_category<-group_by(airport_delays,airport_category)
airport_mean_delays_by_category2<-group_by(airport_delays,airport_category)

airport_mean_delays_by_state<-group_by(airport_delays,state)
airport_mean_delays_by_state<-summarise(airport_mean_delays_by_state,mean_dep_delay=sum(departing_flights_number*mean_dep_delay)/sum(departing_flights_number),mean_arr_delay=sum(arriving_flights_number*mean_arr_delay)/sum(arriving_flights_number))

# airport_mean_delays_by_category<-summarise(airport_mean_delays_by_category,mean_dep_delay=sum(departing_flights_number*mean_dep_delay)/sum(departing_flights_number))
# airport_mean_delays_by_category2<-summarise(airport_mean_delays_by_category2, mean_arr_delay=sum(arriving_flights_number*mean_arr_delay)/sum(arriving_flights_number))
# airport_mean_delays_by_category<-merge(airport_mean_delays_by_category,airport_mean_delays_by_category2,by="airport_category")
# ggplot(airport_delays_by_category)+geom_bar(data=airport_delays_by_category,mapping = aes(x=airport_category,y=overall_delay_ratio),inherit.aes = FALSE,stat="identity")
# ggplot(airport_delays_by_category)+geom_bar(data=airport_delays_by_category,mapping = aes(x=airport_category,y=dep_delay_ratio),inherit.aes = FALSE,stat="identity")
# ggplot(airport_delays_by_category)+geom_bar(data=airport_delays_by_category,mapping = aes(x=airport_category,y=arr_delay_ratio),inherit.aes = FALSE,stat="identity")
# 
# 
# ggplot(airport_mean_delays_by_category)+geom_bar(data=airport_mean_delays_by_category,mapping = aes(x=airport_category,y=mean_arr_delay),inherit.aes = FALSE,stat="identity")
# 
# 
# top_10_arr_delays_mean<-usmap_transform(airports_top_10_arr_delays_mean,input_names=c("long","lat"),output_names=c("x","y"))
# plot_usmap()+geom_point(aes(x,y),colour='red',data=top_10_arr_delays_mean)
# 
# top_10_dep_delays_mean<-usmap_transform(airports_top_10_dep_delays_mean,input_names=c("long","lat"),output_names=c("x","y"))
# plot_usmap()+geom_point(aes(x,y),colour='red',data=top_10_dep_delays_mean)
# 
# top_10_arr_delays_ratio<-usmap_transform(airports_top_10_arr_delays_ratio,input_names=c("long","lat"),output_names=c("x","y"))
# plot_usmap()+geom_point(aes(x,y),colour='red',data=top_10_arr_delays_ratio)
# 
# top_10_dep_delays_ratio<-usmap_transform(airports_top_10_dep_delays_ratio,input_names=c("long","lat"),output_names=c("x","y"))
# plot_usmap()+geom_point(aes(x,y),colour='red',data=top_10_dep_delays_ratio)
# 
# plot_usmap()+geom_point(aes(x=longitude_deg,y=latitude_deg),data=airport_delays)

stateDelayPlot <- plot_usmap(data=airport_mean_delays_by_state,values="mean_dep_delay",color="red")+
  scale_fill_continuous(name="Mean departure delay",low="light blue",high="dark blue", label=scales::comma)+
  theme(legend.position = "right",
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.key.size = unit(1, 'cm') ,
        legend.background = element_rect(fill = 'transparent'))
stateDelayPlot
# plot_usmap(data=airport_mean_delays_by_state,values="mean_arr_delay",color="red")+scale_fill_continuous(name="Mean arrival delay in 2000",label=scales::comma)+theme(legend.position = "right")
# 
# plot_usmap()+geom_point(aes(x=long,y=lat),colour='red',data=airports_top_10_dep_delays_ratio)


plane_data_1<- plane_data[, c("tailnum", "year")]
plane_data_1 <- plane_data[grepl("^1|2\\d{3}$", plane_data$year), ]

data <- df[, c("TailNum", "Cancelled")]
merged_data <- merge(data, plane_data_1, by.x = "TailNum", by.y = "tailnum", all.x = TRUE)
merged_data <- na.omit(merged_data)
unique(df$TailNum)

cancelled_flights <- merged_data[merged_data$Cancelled == 1, ]
cancelled_flights <- na.omit(cancelled_flights)

grupped_cancelled <- group_by(cancelled_flights, year)
grupped_cancelled <- grupped_cancelled[grepl("^1|2\\d{3}$", grupped_cancelled$year), ]
grupped_cancelled <- na.omit(grupped_cancelled)
grupped_cancelled <- summarise(grupped_cancelled, TotalCancelled = sum(Cancelled))

flights_by_year <- group_by(merged_data, year)
flights_by_year <- summarise(flights_by_year, CalkowitaLiczbaLotow = sum(n()))
flights_by_year <- flights_by_year[grepl("^1|2\\d{3}$", flights_by_year$year), ]

results <- merge(flights_by_year, grupped_cancelled, by = "year")
results$TotalCancelled <- replace(results$TotalCancelled, is.na(results$TotalCancelled), 0)
final_results <- transform(results, cancelled_flights = TotalCancelled/CalkowitaLiczbaLotow * 100)

srednia <- mean(final_results$cancelled_flights)


cancelledPlot <- ggplot(data = final_results, aes(year,cancelled_flights)) +
  geom_bar(stat = "identity", fill = "deepskyblue2") +
  labs(x = "plane's year of production", y = "cancelled flights (%)") + 
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, margin=margin(5,0,0,0)),
    axis.text.y = element_text(size = 10, margin=margin(5,0,0,0)),
    axis.title.y = element_text(size = 15, color="#000000", margin=margin(0,10,0,0)),
    axis.title.x = element_text(size = 15, color="#000000",margin=margin(10,0,0,0)),
    axis.ticks.x = element_line(linewidth = 1)
  ) +
  geom_hline(yintercept = srednia, color = "chocolate",
             linetype = "dashed", size = 1, aes(color = "Średnia"))


################################################################################
################################################################################


plane_data_2 <- plane_data[, c("tailnum", "engine_type")]
plane_data_2 <- plane_data[plane_data$engine_type != "" & plane_data$engine_type!= "None", ]

all_engine_data <- df[, c("TailNum", "ArrDelay")]


merged_engine_data <- merge(all_engine_data, plane_data_2, by.x = "TailNum", by.y = "tailnum", all.x = TRUE)
merged_engine_data <- na.omit(merged_engine_data)
merged_engine_data <- group_by(merged_engine_data, engine_type)

avg_del_by_engine <- summarise(merged_engine_data, Avg_del_min = mean(ArrDelay))

avd_del <- mean(merged_engine_data$ArrDelay)

ggplot(data = avg_del_by_engine, aes(engine_type,Avg_del_min)) +
  geom_bar(stat = "identity", fill = "deepskyblue2") +
  labs(x = "Typ silnika", y = "Średnie opóźnienie (min)") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 10, margin=margin(5,0,0,0)),
    axis.text.y = element_text(size = 10, margin=margin(5,0,0,0)),
    axis.title.y = element_text(size = 15, color="#000000", margin=margin(0,10,0,0)),
    axis.title.x = element_text(size = 15, color="#000000",margin=margin(10,0,0,0)),
    axis.ticks.x = element_line(linewidth = 1)
  ) +
  geom_hline(yintercept = avd_del, color = "chocolate",
             linetype = "dashed", size = 1)


################################################################################
################################################################################


df2 <- filter(df, DepDelay >0 & !is.na(DepDelay) & !is.na(ArrDelay))
df2$czy_nadrabia <- ifelse(df2$DepDelay-df2$ArrDelay > 0, TRUE, FALSE)
n <- nrow(df2)
res <- group_by(df2, czy_nadrabia) %>% summarize(wartosc = round(sum(n())*100 /n, 1))

ggplot(res, aes(x = "", y = wartosc, fill = czy_nadrabia)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Udział samolotów, które zmniejszyły opóźnienie \nwśród startujacych z opóźnieniem", fill="Czy zmniejszono\nopoznienie:") +
  theme_void() +
  scale_fill_manual(values = c("#417Ae1", "#FFE5B4"), labels = c("Nie", "Tak")) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 15),
  ) +
  geom_text(aes(label = paste0(wartosc, "%")),
            position = position_stack(vjust = 0.5), 
            size = 6)

################################################################################

df_3 <- filter(df, DepDelay <= 0 & !is.na(DepDelay) & !is.na(ArrDelay))
df_3$czy_nadrabia <- ifelse(df_3$DepDelay-df_3$ArrDelay > 0, TRUE, FALSE)

n <- nrow(df_3)
res <- group_by(df_3, czy_nadrabia) %>% summarize(wartosc = round(sum(n())*100 /n, 1))

ggplot(res, aes(x = "", y = wartosc, fill = czy_nadrabia)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Udział samolotów, które zmniejszyły opóźnienie \nwśród startujacych bez opóźnienia", fill="Czy zmniejszono\nopoznienie:") +
  theme_void() +
  scale_fill_manual(values = c("#417Ae1", "#FFE5B4"), labels = c("Nie", "Tak")) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 15)
  ) +
  geom_text(aes(label = paste0(wartosc, "%")),
            position = position_stack(vjust = 0.5),
            size = 6)




