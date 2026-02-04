#libraries
library(ggplot2)
library(readxl)

#setwd
setwd("D:/HackBio")

#create plot saving dir####
dir.create("plots")

#### Part 3####
#----------------------------------------------------------

#Task 1 :Task 1. Reproduce panel 2a: Cell-type ratio distributions####
"Goal: Compare distributions across immune cell types.

Requirements:
  
  Read sheet a
Produce a boxplot of new_ratio grouped by cell_type
Match:
  Orientation (rotated labels)
Relative scaling
Outlier visibility"
#load data
data <- read_xlsx("data/hb_stage_2.xlsx", sheet = "a")

png("plots/Figure3a.png",res = 300, width = 300*12, height = 300*6)
(unique(data$cell_type))
ggplot(data = data, aes(x=cell_type, y=new_ratio, fill=cell_type)) + geom_boxplot()+
  labs(
    x= element_blank(),
    y= "Ratio"
  )+
  theme_classic(base_family = "Arial")
dev.off()
# Task 6: Reproduce panel 2f: Stacked proportions#####
#import data

data<- readxl::read_xlsx("data/hb_stage_2.xlsx",sheet ='f' )
data <- data[data$stage=="s00h" | data$stage=="s72h",]

#plot f 
png("plots/Figure3f.png",res = 300, width = 300*5, height = 300*6)
ggplot(data,
       aes(x= stage, y= proportion, fill = cell_type))+
  geom_bar(stat="identity" )+
  scale_fill_manual("legend", values = c("Plasma" = "steelblue", "B" = "hotpink"))+

  labs(
    x="Stage",
    y="Proportion") +
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.05))+
  theme_classic()
dev.off()



#