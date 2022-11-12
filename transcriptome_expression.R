library(ggplot2)
library(tidyverse)
library(forcats)

te <- read.csv("tran_express.csv")
head(te)

te1 <- te %>%  
  gather(label, value, 1:26) %>%
  drop_na(value)
te1
write.csv(te1,"trans_express2.csv")
# made changes in excel to this file eg removed empties, X and added isotype, added the group numbers i wanted
  
te2 <- read.csv("trans_express2.csv") %>% 
  arrange(group)
head(te2)



ts <- ggplot(te2, aes(fill=Isotype, y=relative.expression, x= fct_inorder(tissue))) + ### fct_inorder(tissue) puts x axis in order of the factors position in list
  geom_bar(position="fill", stat="identity") +
  theme_classic() +
  theme(axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Relative isotype expression at different life stages", x="Sample type", y="Relative expression") +
  scale_fill_manual(values=c("green2","goldenrod1","midnightblue","brown1","dodgerblue3","springgreen4")) 
ts

ggsave("trans_express.jpeg")




