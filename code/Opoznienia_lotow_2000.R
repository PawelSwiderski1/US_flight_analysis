loty2000<-read.csv('G:/dataverse_files/2000.csv.bz2')
airport_codes2<-read.csv('G:/dataverse_files/airports.csv')
install.packages("rgdal")
library(rgdal)
library(dplyr)
install.packages("ggplot2")
install.packages("maptools")
library(maptools)
library(ggplot2)
install.packages("maps")
install.packages("usmap")
library(usmap)
library(maps)
head(loty2000,10)
#Opóźnienia na poszczególnych trasach
# Na razie brak interpretacji

routes<-loty2000
routes$route<-paste(loty2000$Origin,loty2000$Dest,sep="-")
routes_arr_delays_number<-filter(routes,routes$ArrDelay>0)
routes_dep_delays_number<-filter(routes,DepDelay>0)
routes_dep_delays_number<-group_by(routes_dep_delays_number,route)
routes_dep_delays_number<-summarise(routes_dep_delays_number,Mean_dep_delay=mean(DepDelay),Dep_Delays_number=length(DepDelay))
routes_arr_delays_number<-group_by(routes_arr_delays_number,route)
routes_arr_delays_number<-summarise(routes_arr_delays_number,Mean_arr_delay=mean(ArrDelay),Arr_Delays_number=length(ArrDelay))
routes2<-group_by(routes,route)
routes2<-summarise(routes2,flights_number=length(route))
routes3<-distinct(routes[,c('route','Origin','Dest')])

routes<-merge(routes3,routes2,by='route')
route_delays<-merge(routes,routes_dep_delays_number,by='route')
route_delays<-merge(route_delays,routes_arr_delays_number,by='route')
head(route_delays,10)
route_delays$Dep_delays_ratio<-route_delays$Dep_Delays_number/route_delays$flights_number
route_delays$Arr_delays_ratio<-route_delays$Arr_Delays_number/route_delays$flights_number
routes_top_10_mean_arr_delays<-head(as.data.frame(arrange(filter(route_delays,flights_number>100),-Mean_arr_delay)),10)
routes_top_10_mean_arr_delays<-merge(routes_top_10_mean_arr_delays,airport_codes2,by.x='Origin',by.y='iata')
routes_top_10_mean_arr_delays<-routes_top_10_mean_arr_delays[,c('Origin','route','Dest','Mean_arr_delay','lat','long','state')]
colnames(routes_top_10_mean_arr_delays)[c(5,6,7)]<-c('Origin_lat','Origin_long','Origin_state')
routes_top_10_mean_arr_delays<-merge(routes_top_10_mean_arr_delays,airport_codes2,by.x='Dest',by.y='iata')
routes_top_10_mean_arr_delays<-routes_top_10_mean_arr_delays[,c('Origin','route','Dest','Mean_arr_delay','Origin_lat','Origin_long','Origin_state','lat','long','state')]
colnames(routes_top_10_mean_arr_delays)[c(8,9,10)]<-c('Dest_lat','Dest_long','Dest_state')
routes_top_10_mean_arr_delays<-as.data.frame(lapply(routes_top_10_mean_arr_delays,rep,2))
routes_top_10_mean_arr_delays[seq(11,20,1),'Origin_lat']<-routes_top_10_mean_arr_delays[seq(1,10,1),'Dest_lat']
routes_top_10_mean_arr_delays[seq(11,20,1),'Origin_long']<-routes_top_10_mean_arr_delays[seq(11,20,1),'Dest_long']
routes_top_10_mean_arr_delays[seq(11,20,1),'Dest_lat']<-routes_top_10_mean_arr_delays[seq(1,10,1),'Origin_lat']
routes_top_10_mean_arr_delays[seq(11,20,1),'Dest_long']<-routes_top_10_mean_arr_delays[seq(1,10,1),'Origin_long']

routes_top_10_mean_arr_delays<-usmap_transform(routes_top_10_mean_arr_delays,input_names=c("Origin_long","Origin_lat"),output_names=c("x","y"))
routes_top_10_mean_arr_delays<-usmap_transform(routes_top_10_mean_arr_delays,input_names=c("Dest_long","Dest_lat"),output_names=c("x_2","y_2"))

plot_usmap()+geom_point(aes(x,y),colour='red',data=routes_top_10_mean_arr_delays)+geom_segment(aes(x=x,y=y,xend=x_2,yend=y_2),colour='red',data=routes_top_10_mean_arr_delays)


routes_top_10_ratio_arr_delays<-head(as.data.frame(arrange(filter(route_delays,flights_number>100),-Arr_delays_ratio)),10)
routes_top_10_ratio_arr_delays<-merge(routes_top_10_ratio_arr_delays,airport_codes2,by.x='Origin',by.y='iata')
routes_top_10_ratio_arr_delays<-routes_top_10_ratio_arr_delays[,c('Origin','route','Dest','Arr_delays_ratio','lat','long','state')]
colnames(routes_top_10_ratio_arr_delays)[c(5,6,7)]<-c('Origin_lat','Origin_long','Origin_state')
routes_top_10_ratio_arr_delays<-merge(routes_top_10_ratio_arr_delays,airport_codes2,by.x='Dest',by.y='iata')
routes_top_10_ratio_arr_delays<-routes_top_10_ratio_arr_delays[,c('Origin','route','Dest','Arr_delays_ratio','Origin_lat','Origin_long','Origin_state','lat','long','state')]
colnames(routes_top_10_ratio_arr_delays)[c(8,9,10)]<-c('Dest_lat','Dest_long','Dest_state')
routes_top_10_ratio_arr_delays<-as.data.frame(lapply(routes_top_10_ratio_arr_delays,rep,2))
routes_top_10_ratio_arr_delays[seq(11,20,1),'Origin_lat']<-routes_top_10_ratio_arr_delays[seq(1,10,1),'Dest_lat']
routes_top_10_ratio_arr_delays[seq(11,20,1),'Origin_long']<-routes_top_10_ratio_arr_delays[seq(11,20,1),'Dest_long']
routes_top_10_ratio_arr_delays[seq(11,20,1),'Dest_lat']<-routes_top_10_ratio_arr_delays[seq(1,10,1),'Origin_lat']
routes_top_10_ratio_arr_delays[seq(11,20,1),'Dest_long']<-routes_top_10_ratio_arr_delays[seq(1,10,1),'Origin_long']

routes_top_10_ratio_arr_delays<-usmap_transform(routes_top_10_ratio_arr_delays,input_names=c("Origin_long","Origin_lat"),output_names=c("x","y"))
routes_top_10_ratio_arr_delays<-usmap_transform(routes_top_10_ratio_arr_delays,input_names=c("Dest_long","Dest_lat"),output_names=c("x_2","y_2"))

plot_usmap()+geom_point(aes(x,y),colour='red',data=routes_top_10_ratio_arr_delays)+geom_segment(aes(x=x,y=y,xend=x_2,yend=y_2),colour='red',data=routes_top_10_ratio_arr_delays)


#Opóźnienia na lotniskach
airports<-loty2000
enplanments<-length(loty2000$Year)*100
airports_departures<-group_by(airports,Origin)
airports_departures<-summarise(airports_departures,departing_flights_number=length(Origin),mean_dep_delay=mean(DepDelay,na.rm=TRUE))
airports_arrivals<-group_by(airports,Dest)
airports_arrivals<-summarise(airports_arrivals,arriving_flights_number=length(Dest),mean_arr_delay=mean(ArrDelay,na.rm=TRUE))
airports_dep_delays_number<-filter(airports,DepDelay>0)
airports_dep_delays_number<-group_by(airports_dep_delays_number,Origin)
airports_dep_delays_number<-summarise(airports_dep_delays_number,dep_delayed_flights_number=length(Origin))
airports_arr_delays_number<-filter(airports,ArrDelay>0)
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
airports_delays_number1<-filter(airports_delays_number,flights_number*100>2500 & flights_number*100<10000)
airports_delays_number1$airport_category<-"Nonprimary"
airports_delays_number2<-filter(airports_delays_number,flights_number*100>=10000 & flights_number*100<0.0005*enplanments)
airports_delays_number2$airport_category<-"Primary Nonhub"
airport_delays<-merge(airports_delays_number1,airports_delays_number2,all.x=TRUE,all.y=TRUE)
airports_delays_number3<-filter(airports_delays_number,flights_number*100>=enplanments*0.0005 & flights_number*100<enplanments*0.0025)
airports_delays_number3$airport_category<-"Small Hub"
airports_delays_number4<-filter(airports_delays_number,flights_number*100>=enplanments*0.0025 & flights_number*100<enplanments*0.01)
airports_delays_number4$airport_category<-"Medium Hub"
airports_delays_number5<-filter(airports_delays_number,flights_number*100>=enplanments*0.01)
airports_delays_number5$airport_category<-"Large Hub"
airport_delays<-merge(airport_delays,airports_delays_number3,all.x=TRUE,all.y=TRUE)
airport_delays<-merge(airport_delays,airports_delays_number4,all.x=TRUE,all.y=TRUE)
airport_delays<-merge(airport_delays,airports_delays_number5,all.x=TRUE,all.y=TRUE)
#airport_codes<-airport_codes[,c('iso_country','iso_region','iata_code','latitude_deg','longitude_deg')]
#airport_codes<-filter(airport_codes,iata_code!='')
airport_codes2<-airport_codes2[,c('iata','country','state','lat','long')]
airport_codes2<-filter(airport_codes,iata!='',country=='USA')
#airport_delays<-merge(airport_delays,airport_codes,by.x='Airport',by.y='iata_code')
airport_delays<-merge(airport_delays,airport_codes2,by.x='Airport',by.y='iata')


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

airport_mean_delays_by_category<-summarise(airport_mean_delays_by_category,mean_dep_delay=sum(departing_flights_number*mean_dep_delay)/sum(departing_flights_number))
airport_mean_delays_by_category2<-summarise(airport_mean_delays_by_category2, mean_arr_delay=sum(arriving_flights_number*mean_arr_delay)/sum(arriving_flights_number))
airport_mean_delays_by_category<-merge(airport_mean_delays_by_category,airport_mean_delays_by_category2,by="airport_category")
ggplot(airport_delays_by_category)+geom_bar(data=airport_delays_by_category,mapping = aes(x=airport_category,y=overall_delay_ratio),inherit.aes = FALSE,stat="identity")
ggplot(airport_delays_by_category)+geom_bar(data=airport_delays_by_category,mapping = aes(x=airport_category,y=dep_delay_ratio),inherit.aes = FALSE,stat="identity")
ggplot(airport_delays_by_category)+geom_bar(data=airport_delays_by_category,mapping = aes(x=airport_category,y=arr_delay_ratio),inherit.aes = FALSE,stat="identity")


ggplot(airport_mean_delays_by_category)+geom_bar(data=airport_mean_delays_by_category,mapping = aes(x=airport_category,y=mean_arr_delay),inherit.aes = FALSE,stat="identity")


top_10_arr_delays_mean<-usmap_transform(airports_top_10_arr_delays_mean,input_names=c("long","lat"),output_names=c("x","y"))
plot_usmap()+geom_point(aes(x,y),colour='red',data=top_10_arr_delays_mean)

top_10_dep_delays_mean<-usmap_transform(airports_top_10_dep_delays_mean,input_names=c("long","lat"),output_names=c("x","y"))
plot_usmap()+geom_point(aes(x,y),colour='red',data=top_10_dep_delays_mean)

top_10_arr_delays_ratio<-usmap_transform(airports_top_10_arr_delays_ratio,input_names=c("long","lat"),output_names=c("x","y"))
plot_usmap()+geom_point(aes(x,y),colour='red',data=top_10_arr_delays_ratio)

top_10_dep_delays_ratio<-usmap_transform(airports_top_10_dep_delays_ratio,input_names=c("long","lat"),output_names=c("x","y"))
plot_usmap()+geom_point(aes(x,y),colour='red',data=top_10_dep_delays_ratio)

plot_usmap()+geom_point(aes(x=longitude_deg,y=latitude_deg),data=airport_delays)
plot_usmap(data=airport_mean_delays_by_state,values="mean_dep_delay",color="red")+scale_fill_continuous(name="Mean departure delay in 2000",label=scales::comma)+theme(legend.position = "right")
plot_usmap(data=airport_mean_delays_by_state,values="mean_arr_delay",color="red")+scale_fill_continuous(name="Mean arrival delay in 2000",label=scales::comma)+theme(legend.position = "right")

plot_usmap()+geom_point(aes(x=long,y=lat),colour='red',data=airports_top_10_dep_delays_ratio)








