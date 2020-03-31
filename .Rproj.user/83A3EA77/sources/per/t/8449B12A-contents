#this is older code, so be sure to run in a fresh environment
suppressWarnings(pacman::p_load(plyr, tidyverse, lubridate, scales, knitr, kableExtra))

#quick look at the temperature data from Thierola
thierola <- read.csv("ThieroRainWise_7 Mar 12 to 26 Jan 16.csv")
thierola <- thierola[,1:15]
thierola <- thierola[-c(55538:55546),] #removing dates with RH errors
colnames(thierola) <- c("Date", "Time", "T_Air", "Hum", "DEW", "BARO", "WDIR", "WSPD", "WS_MAX", "SRAD", "SR.SUM", "RAINF", "VOLTS", "UNITS", "CKSUM")

#format Date correctly
thierola$Date <- as.POSIXct(thierola$Date, format= "%m/%d/%Y")

#creating a few grouping columns (so I can extract weekly per half-hour means)
thierola$datetime <- with(thierola, paste(Date, Time)) %>%
  as.POSIXct(., format="%Y-%m-%d %H:%M")
thierola$day <- format(as.Date(thierola$datetime), "%j")
thierola$month <- month(as.Date(thierola$datetime))
thierola$year <- year(as.Date(thierola$datetime))

thierola <- na.omit(thierola)

#making a summary table of mean weekly per half-hour temperature and humidity
thierola.sum <- thierola %>%
  filter(month == "9" | month == "10") %>%
  group_by_at(vars(month, Time)) %>%
  summarize(mean.temp= mean(T_Air),
            mean.hum = mean(Hum))

thierola.sum$Time=as.POSIXct(thierola.sum$Time,format="%H:%M",tz="Africa/Bamako")

thierola.sum$month <- as.factor(thierola.sum$month)

#pull in incubator file
inc <- read.csv("21-Nov-17-waterprobe2.csv", check.names = TRUE)
inc <- inc[,1:6]
colnames(inc) <- c("Measurement.Number", "Date", "Temp.Air", "RH", "Lux", "Temp.Water")
inc$Date <- as.POSIXct(inc$Date, format="%m/%d/%Y %H:%M")
inc$Time <- strftime(inc$Date, format="%H:%M") 
inc$Time <- as.POSIXct(inc$Time,format="%H:%M",tz="Africa/Bamako")


inc.sum <- inc %>%
  group_by_at(vars(Time)) %>%
  summarize(mean.temp.air.inc= mean(Temp.Air),
            mean.temp.water.inc = mean(Temp.Water), 
            mean.hum.inc = mean(RH))

join.thierola.inc <- left_join(thierola.sum, inc.sum, by="Time") 

#doing some very awkward reshaping
join.thierola.inc2 <- join.thierola.inc %>%
  gather(variable, value, -(Time:month)) %>%
  unite(temp, month, variable) %>%
  spread(temp, value)

join.thierola.inc2 <- join.thierola.inc2[,-c(10,11)]



join.thierola.inc2 %>%
  ggplot() +
  geom_line(size=1.2, aes(x=Time, y=`9_mean.temp`, color="September Mean Temp")) +
  geom_line(size=1.2, aes(x=Time, y=`9_mean.hum` * (38/100), color="September Mean Humidity")) +
  geom_line(size=1.2, aes(x=Time, y=`10_mean.temp`, color="October Mean Temp")) +
  geom_line(size=1.2, aes(x=Time, y=`10_mean.hum` * (38/100), color="October Mean Humidity")) +
  geom_line(size=1.2, aes(x=Time, y=`10_mean.temp.air.inc`, color="Incubator Mean Temp")) +
  #geom_line(size=1.2, aes(x=Time, y=`10_mean.temp.water.inc`, color="Incubator water Temp")) +
  geom_line(size=1.2, aes(x=Time, y=`10_mean.hum.inc` * (38/100), color="Incubator Mean Humidity")) +
  scale_y_continuous(sec.axis = sec_axis(~.*(100/38), name = "Relative humidity (%)", breaks=pretty_breaks(n=10)), limits=c(12,38), breaks=pretty_breaks(n=5)) +
  scale_color_manual(values=c('#b2df8a','#1f78b4','#a6cee3','#e31a1c','#fb9a99','#ff9102','#fdbf6f')) +
  scale_x_datetime(labels=date_format("%H:%M", tz = "Africa/Bamako"),
              breaks = date_breaks("2 hour"), 
              minor_breaks=date_breaks("1 hour")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=16)) +
  labs(y="Temperature (C)")
ggsave("27-Mar-20-additional.file.1.pdf", device=cairo_pdf, dpi=300, height=4, width=8)

