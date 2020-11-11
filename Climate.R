# PREPARING DATA
climate_change <- read.csv(file = "climate_change.csv",TRUE, sep = ",", stringsAsFactors = FALSE)
str(climate_change)
dim(climate_change)

climate_change$Month[climate_change$Month == 1]<- "Jan"
climate_change$Month[climate_change$Month == 2]<- "Feb"
climate_change$Month[climate_change$Month == 3]<- "March"
climate_change$Month[climate_change$Month == 4]<- "April"
climate_change$Month[climate_change$Month == 5]<- "May"
climate_change$Month[climate_change$Month == 6]<- "June"
climate_change$Month[climate_change$Month == 7]<- "July"
climate_change$Month[climate_change$Month == 8]<- "Aug"
climate_change$Month[climate_change$Month == 9]<- "Sep"
climate_change$Month[climate_change$Month == 10]<- "Oct"
climate_change$Month[climate_change$Month == 11]<- "Nov"
climate_change$Month[climate_change$Month == 12]<- "Dec"

library(dplyr)
library(ggplot2)
library(plotly)
library("RColorBrewer")

# HANDLING MISSING DATA
new_climate_change <- na.omit(climate_change)
dim(new_climate_change)

# Exploratory Data Anlysis (EDA)

# Bar Plot
climate_change_chart <- ggplot(climate_change, aes(x = Year, y = Temp, fill = CO2)) + 
  xlab("Year") +
  ylab("Temperature") +
  theme_minimal(base_size = 14)
barplot <- climate_change_chart +
  geom_bar( position = "dodge", stat = "identity",color= "white")
ggplotly(barplot)

# Line Plot
library(lubridate)
# adding Year-Month variable as date
climate_change_ymd <- climate_change %>%
  mutate(year_month = ymd(paste(climate_change$Year, climate_change$Month, truncated = 1))) 
L1 <- ggplot(climate_change_ymd, aes(year_month, Temp)) + 
  geom_line() + 
  geom_smooth(se=FALSE, linetype = "dotted") + 
  labs(title = "Temperature (1983-2008)",
       x = "Year", 
       y = "Temperature") +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(L1)

# Temperature Month-wise plot for each year in data
Tg <- ggplot(climate_change, aes(as.factor(Month), Temp)) + 
  geom_point(aes(color = as.factor(Year))) + 
  geom_line(aes(group = as.factor(Year), 
                color = as.factor(Year)), 
            alpha = 0.7) + 
  labs(title = 'Temperature by month') +
  xlab("Months") +
  ylab("Temperature") +
  theme(axis.text.x = element_text(size = 6,angle = 90,hjust = 0.5, vjust = 0.5))
# theme(legend.position = "none")
ggplotly(Tg)

# Variations of CO2, N2O, CH4 and MEI by year
library(ggpubr)
#par(mfrow=c(2,2))
scat_plot1 <-  ggplot(climate_change_ymd, aes(year_month, CO2))+geom_line(colour="blueviolet")+geom_smooth(method = "lm")+ggtitle("Carbon Dioxide")
scat_plot2<-  ggplot(climate_change_ymd, aes(year_month, N2O))+geom_line()+geom_smooth(method = "lm")+ggtitle("Nitrous Oxide")
scat_plot3<-  ggplot(climate_change_ymd, aes(year_month, CH4))+geom_line(colour="springgreen4")+geom_smooth(method = "lm")+ggtitle("Methane")
scat_plot4 <-  ggplot(climate_change_ymd, aes(year_month, MEI))+geom_line(colour="mediumorchid4")+ggtitle("MEI")
grapgh_arrange<-ggarrange(scat_plot1, scat_plot2, scat_plot3, scat_plot4 + rremove("x.text"), 
                          labels = c("A", "B", "C", "D"),
                          ncol = 2, nrow = 2)
annotate_figure(grapgh_arrange,
                top = text_grob("Vartations of CO_2, N2O, CH4 and MEI by year", color = "red", face = "bold", size = 14)
)

# Variations of CFC11, CFC12, Total Solar Irradiance (TSI) and Aerosolos by year
scat_plot5 <-  ggplot(climate_change_ymd, aes(year_month, CFC_a))+geom_line(colour="blue")+ggtitle("CFC-11") +
  ylab("CFC-11")
scat_plot6<-  ggplot(climate_change_ymd, aes(year_month, CFC_b))+geom_line(colour="green")+ggtitle("CFC-12") +
  ylab("CFC-12")
scat_plot7<-  ggplot(climate_change_ymd, aes(year_month, TSI))+geom_line(colour="red")+ggtitle("TSI")
scat_plot8 <-  ggplot(climate_change_ymd, aes(year_month, Aerosols))+geom_line(colour="magenta")+ggtitle("Aerosols")
grapgh_arrange<-ggarrange(scat_plot5, scat_plot6, scat_plot7, scat_plot8+ rremove("x.text"), 
                          labels = c("A", "B", "C", "D"),
                          ncol = 2, nrow = 2)
annotate_figure(grapgh_arrange,
                top = text_grob("Vartations of CFC-11,CFC-12, TSI and Aerosols by year", color = "blue", face = "bold", size = 14)
)

# Linear Regression Model
fit=lm(Temp~CO2,data=climate_change)
summary(fit)
library(ggiraph)
library(ggiraphExtra)
ggPredict(fit,se=TRUE,interactive=TRUE)

# Multiple Regression Model
climate_multreg <- filter(climate_change, Month == "Jan" | Month == "Feb")
fit1=lm(Temp~Year+Month,data=climate_multreg)
summary(fit1)
ggPredict(fit1,se=TRUE, interactive=TRUE)

# Animation
library(gganimate)
ggplot(climate_change, aes(Year, Temp, size = CO2, colour = Month)) + geom_point(alpha = 0.2, show.legend = FALSE) +
  facet_wrap(~Month) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Year', y = 'Temperature') +
  transition_time(Year) +
  ease_aes('linear')

p <- ggplot(climate_change, aes(x = Year, y = Temp, color = Month, group = Month)) +
  geom_path() +
  geom_point() +
  facet_wrap(~ Month) +
  theme(legend.position = 'none') +
  labs(title = 'Temperature Variation, Year: {frame_along}') +
  transition_reveal(along = Year) +
  ease_aes('linear')
animate(p, 100, 10)
