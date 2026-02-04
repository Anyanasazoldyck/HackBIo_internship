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
#Task 2. Reproduce panel 2b: Half-life vs alpha-life scatter####
"Goal: Identify kinetic regimes.

Requirements:
  
  Read sheet b
Plot log2(half_life) vs log2(alpha)
Add:
  Vertical and horizontal cutoffs
Color-coded subsets based on thresholds
Labeled exemplar genes (Camp, Ccr2)
Conceptual checks:
  
  Why log2?
  What do the four quadrants mean?"
data <- read_xlsx("data/hb_stage_2.xlsx", sheet = "b")
#what are the  Vertical and horizontal cutoffs?
y_cutoff <- median(data$alpha)+ sd(data$alpha)
x_cuttoff <- median(data$half_life)+sd(data$half_life)

data$category[data$alpha > y_cutoff & data$half_life < x_cuttoff] <- "q2"
data$category[data$alpha < y_cutoff & data$half_life > x_cuttoff] <- "q4"
data$category[data$alpha > y_cutoff & data$half_life > x_cuttoff] <- "q3"
data$category[data$alpha < y_cutoff & data$half_life < x_cuttoff] <- "q1"

#highlit the genes
highlight <- subset(data, data$cell %in% c("Ccr2", "Camp"))

png("plots/Figure3b.png",res = 300, width = 300*6, height = 300*6)

ggplot (data, aes(x= log2(half_life),
                  y=log2(alpha), colour  = category))+geom_point()+
  scale_color_manual("category", values = c("q1"="black","q2"="lightgreen","q3"="red","q4"="steelblue"))+
    geom_vline(xintercept =log2( x_cuttoff), linetype="dashed")+
  geom_hline(yintercept = log2(y_cutoff), linetype="dashed")+
  geom_label(data = highlight, aes(label = cell), text.colour = "hotpink",position = "jitter")+
  
  geom_point(data = highlight,
             shape = 21,      
             size = 2,
             stroke = 1,
             fill = NA,
             color = "hotpink")+
  theme_classic(base_family = "Arial")+
  theme(legend.position = "none")
  
dev.off()
?geom_text
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