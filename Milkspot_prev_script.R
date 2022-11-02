library(psych)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lubridate)
# makesure date in excel is yyyy-dd-mm

ms <- read.csv("FSA_milkspot_data.csv") #combinded datasets downloaded from FSA website
head(ms)
ms1 <- ms[, c("Year","Month","Country","PercentageOfThroughput","Month.name")]
head(ms1)
mse <- ms1 %>%
  filter(Country == "England")  ## seperates the data from each country ##
msw <- ms1 %>%
  filter(Country == "Wales")
mse1 <- mse %>%
  group_by(Month) %>% ## groups the data by month values ##
  mutate(month.avg = mean(PercentageOfThroughput, na.rm = TRUE), ## calculates the mean and sd of milkspot cases/per
         month.sd = sd(PercentageOfThroughput)) %>%
  data.frame()
mse2 <- mse1[!duplicated(mse1[,2]),] # removes the duplicates as the triplicate data for each sample now have the same Cq mean score
msw1 <- msw %>%
  group_by(Month) %>% ## groups the data by month values ##
  mutate(month.avg = mean(PercentageOfThroughput, na.rm = TRUE), ## calculates the mean and sd of milkspot cases/per
         month.sd = sd(PercentageOfThroughput)) %>%
  data.frame()
msw2 <- msw1[!duplicated(mse1[,2]),] # removes the duplicates as the triplicate data for each sample now have the same Cq mean score
ms2 <-rbind(mse2,msw2)
ms3 <- ms2 %>%
  select(Month, Country, month.avg, month.sd, Month.name)
ms3
ms3$Month.name = factor(ms3$Month.name, levels = month.name) ## lets R know that the names are months and to order chronologically ##
write.csv(ms3, "FSA_monthly_avg.csv")
p <- ggplot(ms3,aes(Month.name, month.avg, color = Country, group = Country)) + ## important to include "group = xxx" otherwise data cant be read ##
  geom_line(aes(color = Country)) +
  geom_errorbar(aes(ymin=month.avg-month.sd, ymax=month.avg+month.sd), width=.2) +
  scale_color_manual(values = c("red","green4")) +
  labs(title = "Average milkspot prevalence per month Jan 2017-Dec 2020", x="Month", y="Milkspot prevalence") + 
  theme_classic()
p
ggsave("Milkspot_prevalence_AVG_SD.tiff", dpi = 300)

mse4 <- mse%>%
  mutate(Test.Month = row_number())

msw4 <- msw%>%
  mutate(Test.Month = row_number())

ms4 <- rbind(mse4,msw4)

p2 <- ggplot(ms4,aes(Test.Month, PercentageOfThroughput, color = Country, group = Country)) +   #####graph of each month over the years.....not the average
  geom_line(aes(color = Country)) +
  scale_color_manual(values = c("red","green4")) +
  labs(title = "Milkspot prevalence per month Jan 2017-Dec 2020", x="Test month", y="Milkspot prevalence") + 
  theme_classic()
p2
ggsave("Milkspot_prevalence_2017-2020.tiff", dpi = 300)
ms4$Month.name = factor(ms4$Month.name, levels = month.name)

ms1$Month.name = factor(ms3$Month.name, levels = month.name)
mse1$Month.name = factor(ms3$Month.name, levels = month.name)
ms5 <-rbind(mse1,msw1)

p3 <- ggplot(ms5,aes(Month.name, PercentageOfThroughput, color = Country, group = Country)) +
  geom_point(aes(color = Country)) + 
  geom_line(aes(Month.name, month.avg, color = Country, group = Country)) +
  scale_color_manual(values = c("red","green4")) +
  labs(title = "Average milkspot prevalence per month Jan 2017-Dec 2020", x="Month", y="Milkspot prevalence") + 
  theme_classic()
p3
ggsave("Milkspot_AVG_points_2017-2020.tiff", dpi = 300)


sc <- read.csv("Scotland_Conditions_Data_2016_2020.csv") #combinded datasets downloaded from FSS website

head(sc)
sc1 <- sc[, c("Date", "Number.of.Conditions")]
head(sc1)

sc2 <- as_date(sc1$Date) # makesure date in excel is yyy-dd-mm

head(sc2)

sc3 <- cbind(sc1,sc2)
head(sc3)
sc4 <- sc3[, c("sc2", "Number.of.Conditions")]
head(sc4)

short.date = strftime(sc4$sc2, "%Y/%m")
sc5 <- aggregate(sc4$Number.of.Conditions ~ short.date, FUN = sum)%>%
  mutate(Test.month = row_number())%>%
  mutate(Country = "Scotland")%>%
  data.frame()
head(sc5)
write.csv(sc5, "Scotland_monthly_milkspot.csv")

sc6 <- read.csv("Scotland_monthly_milkspot_name.csv")
head(sc6)
sc7 <- sc6[, c("month.name", "sc4.Number.of.Conditions")]
head(sc7)



p4 <- ggplot(sc6,aes(x= fct_inorder(month.name), y=sc4.Number.of.Conditions, group = Country)) +   #####graph of each month over the years.....not the average
  geom_line(aes(color = Country)) +
  labs(title = "Milkspot cases per month Jan 2016-Dec 2020", x="Test month", y="Milkspot cases") +
  scale_color_manual(values = c("blue4")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p4
ggsave("scotland-milkspot-case-16-20.tiff", dpi = 300)

p5 <- ggarrange(p3, p4,labels = c("a", "b"), ncol = 1, nrow = 2) # combines all plots into one figure
p5
ggsave("UK-milkspot-comparison.tiff", dpi = 300)  







