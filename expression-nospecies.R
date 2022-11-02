library(psych)
library(tidyverse)
library(ggplot2)
library(ggpubr)
ec <- read.csv("Combined_gene_expression.csv")
head(ec)
ec1 <- ec[, c("Sample", "Isotype","Species","Sample.type", "relative.expression")]
head(ec1)
###### ADULTS #########
eca <- ec1 %>%
  arrange(Isotype)%>% # Arranges data 1st into species and then into isotypes 
  filter(Sample.type == "Adult")%>%
  data.frame()
head(eca)
eca2 <- eca %>%
 filter(relative.expression > 0)
#ecl <- ec3 %>%  # seperates al the A. lumbricoides data into 1 data frame
 # filter(Species == "A.lumbricoides")
#ecs <- ec3 %>%  # seperates al the A. suum data into 1 data frame
  #filter(Species == "A.suum")

gea <- eca2 %>%  
  group_by(Isotype)%>% # makes the data split into isotype groups rather than samples
  mutate(iso.mean = mean(relative.expression, na.rm = TRUE),
         iso.sd = sd(relative.expression))%>%
  data.frame()

gea1 <- gea %>%
  select(Isotype, iso.mean, iso.sd, Sample.type) # isolates the isotype mean data with species and isotype
gea2 <- gea1[!duplicated(gea1[,2]),] # removes duplicates as the mean for the isotype is assigned to each sample
### the 3 refers to the column that will be searched for duplicates, in this case the iso.mean ###
### if different column used it could remove to much data ###

write.csv(gea2, "Mean_adult_isotype_expression_nospecies.csv")
p1 <- ggplot(gea2, aes(Isotype, iso.mean, fill = Isotype)) + # plots mean expression isotypes
  geom_bar(mapping = NULL, data = NULL, stat = "identity",
           position = "stack", width = NULL,
           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  scale_fill_manual(values=c("A"="green2","B"= "goldenrod1","C"= "midnightblue","D"= "brown1","E"= "dodgerblue3","F"= "springgreen4","G"= "darkorchid1")) +
  geom_errorbar(aes(ymin=iso.mean-iso.sd, ymax=iso.mean+iso.sd), width=.2,
                position=position_dodge(.9)) +
  labs(title = "Adult isotype expression", x="Isotype", y="Relative expression") +
  theme_classic()
p1
ggsave("Adult_mean_expression_nospecies.jpg")

###### EGGS #######
ece <- ec1 %>%
  arrange(Isotype)%>% # Arranges data 1st into species and then into isotypes 
  filter(Sample.type == "Egg")%>%
  data.frame()
head(ece)
ece2 <- ece %>%
  filter(relative.expression > 0)


gee <- ece2 %>%  
  group_by(Isotype)%>% # makes the data split into isotype groups rather than samples
  mutate(iso.mean = mean(relative.expression, na.rm = TRUE),
         iso.sd = sd(relative.expression))%>%
  data.frame()

gee1 <- gee %>%
  select(Isotype, iso.mean, iso.sd, Sample.type) # isolates the isotype mean data with species and isotype
gee2 <- gee1[!duplicated(gee1[,2]),] # removes duplicates as the mean for the isotype is assigned to each sample
### the 3 refers to the column that will be searched for duplicates, in this case the iso.mean ###
### if different column used it could remove to much data ###

write.csv(gee2, "Mean_egg_isotype_expression_nospecies.csv")
p2 <- ggplot(gee2, aes(Isotype, iso.mean, fill=Isotype)) + # plots mean expression isotypes
  geom_bar(mapping = NULL, data = NULL, stat = "identity",
           position = "stack", width = NULL,
           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  scale_fill_manual(values=c("A"="green2","B"= "goldenrod1","C"= "midnightblue","D"= "brown1","E"= "dodgerblue3","F"= "springgreen4","G"= "darkorchid1")) +
  geom_errorbar(aes(ymin=iso.mean-iso.sd, ymax=iso.mean+iso.sd), width=.2,
                position=position_dodge(.9)) +
  labs(title = "Egg isotype expression", x="Isotype", y="Relative expression") +
  theme_classic()
p2
ggsave("Egg_mean_expression_nospecies.jpg")

###### LARVAE ##########

ecl <- ec1 %>%
  arrange(Isotype)%>% # Arranges data 1st into species and then into isotypes 
  filter(Sample.type == "Larvae")%>%
  data.frame()
head(eca)
ecl2 <- ecl %>%
  filter(relative.expression > 0)
#ecl <- ec3 %>%  # seperates al the A. lumbricoides data into 1 data frame
# filter(Species == "A.lumbricoides")
#ecs <- ec3 %>%  # seperates al the A. suum data into 1 data frame
#filter(Species == "A.suum")

gel <- ecl2 %>%  
  group_by(Isotype)%>% # makes the data split into isotype groups rather than samples
  mutate(iso.mean = mean(relative.expression, na.rm = TRUE),
         iso.sd = sd(relative.expression))%>%
  data.frame()

gel1 <- gel %>%
  select(Isotype, iso.mean, iso.sd, Sample.type) # isolates the isotype mean data with species and isotype
gel2 <- gel1[!duplicated(gel1[,2]),] # removes duplicates as the mean for the isotype is assigned to each sample
### the 3 refers to the column that will be searched for duplicates, in this case the iso.mean ###
### if different column used it could remove to much data ###

write.csv(gel2, "Mean_larval_isotype_expression_nospecies.csv")
p3 <- ggplot(gel2, aes(Isotype, iso.mean, fill=Isotype)) + # plots mean expression isotypes
  geom_bar(mapping = NULL, data = NULL, stat = "identity",
           position = "stack", width = NULL,
           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  scale_fill_manual(values=c("A"="green2","B"= "goldenrod1","C"= "midnightblue","D"= "brown1","E"= "dodgerblue3","F"= "springgreen4","G"= "darkorchid1")) +
  geom_errorbar(aes(ymin=iso.mean-iso.sd, ymax=iso.mean+iso.sd), width=.2,
                position=position_dodge(.9)) +
  labs(title = "Larval isotype expression", x="Isotype", y="Relative expression") +
  theme_classic()
p3
ggsave("Larval_mean_expression_nospecies.jpg")


p4 <- ggarrange(p1, p2, p3, ncol = 2, nrow = 2) # combines all plots into one figure
p4
ggsave("comb_expression_plot.tiff", width = 10, height = 10, dpi = 300)

#need to make df with all stages in 
ge_all <- rbind(gea2, gee2, gel2)
ge_all2 <- ge_all %>%
  select(Sample.type, Isotype, iso.mean)


s <- ggplot(ge_all2, aes(fill=Isotype, y=iso.mean, x=Sample.type)) + 
  geom_bar(position="fill", stat="identity") +
  theme_classic() +
  labs(title = "Relative isotype expression at different life stages", x="Sample type", y="Relative expression") +
  scale_fill_manual(values=c("green2","goldenrod1","midnightblue","brown1","dodgerblue3","springgreen4","darkorchid1")) 
s
ggsave("combined_relative_expression.jpeg")


