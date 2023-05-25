library("dplyr")
library(ggplot2)

data_1995 <- read.csv("dataverse_files//1995.csv.bz2")
data_2000 <- read.csv("dataverse_files//2000.csv.bz2")
data_2008 <- read.csv("dataverse_files//2008.csv.bz2")

plane_data <- read.csv("dataverse_files//plane-data.csv")

################################################################################

plane_data_1<- plane_data[, c("tailnum", "year")]
plane_data_1 <- plane_data[grepl("^1|2\\d{3}$", plane_data$year), ]

data <- data_1995[, c("TailNum", "Cancelled")]
merged_data <- merge(data, plane_data_1, by.x = "TailNum", by.y = "tailnum", all.x = TRUE)
merged_data <- na.omit(merged_data)
unique(data_1995$TailNum)

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

ggplot(data = final_results, aes(year,cancelled_flights)) +
  geom_bar(stat = "identity", fill = "deepskyblue2") +
  labs(x = "rok produkcji", y = "anulowane loty (%)") + 
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

engine_data_1995 <- data_1995[, c("TailNum", "ArrDelay")]
engine_data_2000 <- data_2000[, c("TailNum", "ArrDelay")]
engine_data_2008 <- data_2008[, c("TailNum", "ArrDelay")]

all_engine_data <- rbind(engine_data_1995, engine_data_2000, engine_data_2008)

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


df <- filter(data_2008, DepDelay >0 & !is.na(DepDelay) & !is.na(ArrDelay))
df$czy_nadrabia <- ifelse(df$DepDelay-df$ArrDelay > 0, TRUE, FALSE)
n <- nrow(df)
res <- group_by(df, czy_nadrabia) %>% summarize(wartosc = round(sum(n())*100 /n, 1))

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

df_2 <- filter(data_2008, DepDelay <= 0 & !is.na(DepDelay) & !is.na(ArrDelay))
df_2$czy_nadrabia <- ifelse(df_2$DepDelay-df_2$ArrDelay > 0, TRUE, FALSE)

n_2 <- nrow(df_2)
res_2 <- group_by(df_2, czy_nadrabia) %>% summarize(wartosc = round(sum(n())*100 /n_2, 1))

ggplot(res_2, aes(x = "", y = wartosc, fill = czy_nadrabia)) +
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
