#libraries
library(ggplot2)
library(readxl)
library(pheatmap)
library(igraph)

#setwd
setwd("D:/HackBio")

#create plot saving dir####
dir.create("plots")
rm(list = ls())
gc()
graphics.off()
#Part 1 – Gene Expression Analysis####
#------------------------------------------------------------
#a. Heatmap####
"Use the normalized gene expression dataset to plot a clustered heatmap
of the top differentially expressed genes between HBR and UHR samples.
Label both genes and samples.
Use a color gradient (e.g., Blues) to indicate expression levels."

data <- read.csv("https://raw.githubusercontent.com/HackBio-Internship/2025_project_collection/refs/heads/main/Python/Dataset/hbr_uhr_top_deg_normalized_counts.csv")
rownames(data)<- data$X
data$X <- NULL
samples <- colnames(data)
hm_m<- as.matrix(data)
annotation_df <- data.frame(
  samples = samples,
  state = stringr::str_extract(samples, "^[A-Za-z]+")
)
rownames(annotation_df)<-samples
png("plots/figure1a.png", res=300,width = 300*10, height = 300*10)
pheatmap(hm_m, annotation_col = annotation_df, fontsize = 18)
dev.off()

#b. Volcano Plot####
"Plot log2FoldChange vs log10(Padj) from the DEG results.
Color points by significance:
  Upregulated: green
Downregulated: orange
Not significant: grey
Add dashed vertical lines at log2FoldChange = ±1."
data <- read.csv("https://raw.githubusercontent.com/HackBio-Internship/2025_project_collection/refs/heads/main/Python/Dataset/hbr_uhr_deg_chr22_with_significance.csv")


png("plots/figure2a.png", res=300,width = 300*6, height = 300*6)

ggplot(data = data, aes(x=-log2FoldChange, y=X.log10PAdj, colour = significance))+geom_point()+
  theme_classic(base_size = 18, base_family = "Arial")

dev.off()
#------------------------------------------------------------




#Part 2 – Breast Cancer Data Exploration####
#------------------------------------------------------------
#data ####
data <- read.csv("https://raw.githubusercontent.com/HackBio-Internship/2025_project_collection/refs/heads/main/Python/Dataset/data-3.csv")
#c. Scatter Plot (radius vs texture)####
"Plot texture_mean vs radius_mean and color points by diagnosis (M = malignant, B = benign)."
png("plots/figure2c.png", res=300, width = 300*6, height = 300*5)
ggplot(data, aes(x=texture_mean,y=radius_mean,colour= diagnosis))+geom_point()+
  theme_classic(base_size = 18, base_family = "Arial")
dev.off()

#d. Correlation Heatmap####
"Compute the correlation matrix of six key features:
  
  radius_mean, texture_mean, perimeter_mean, area_mean, smoothness_mean, compactness_mean.

Plot as a heatmap with correlation values annotated."
hm_m <- data[,c("radius_mean", "texture_mean", "perimeter_mean", "area_mean", "smoothness_mean", "compactness_mean"),]
cor_hm <- round(cor(hm_m),1)
png("plots/figure2d.png", res = 300, width = 300*8, height = 300*8)
pheatmap(cor_hm, cluster_rows = F, cluster_cols = F, border_color = "black",display_numbers = TRUE,
         number_color = "black", fontsize = 18)
dev.off()
?pheatmap
#e. Scatter Plot (smoothness vs compactness)####
"Plot compactness_mean vs smoothness_mean colored by diagnosis.
Include gridlines and clear axis labels."
png("plots/figure2e.png", res = 300, width = 300*6, height = 300*6)

ggplot(data, aes(x=smoothness_mean,y=compactness_mean, colour = diagnosis))+geom_point()+
  theme_classic(base_size = 18,base_family = "Arial")
dev.off()
#f. Density Plot (area distribution)####
"Plot kernel density estimates (KDE) of area_mean for both M and B diagnoses on the same axis.
Add legend and labeled axes."

png("plots/Figure2f.png",res = 300, width = 300*6, height = 300*5)

ggplot(data, aes(x=area_mean,fill  = diagnosis))+geom_density(alpha=0.5)+
  scale_color_manual("diagnosis",values = c("B"="#FFA5007F","M"="#4682B47F"))+
  theme_classic(base_size = 18, base_family = "Arial")
dev.off()
#---------------------------------------------------------------




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
  theme_classic(base_family = "Arial",base_size = 18)
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

png("plots/Figure3b.png",res = 300, width = 300*7, height = 300*6)

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
  theme_classic(base_family = "Arial", base_size = 18)+
  theme(legend.position = "none")
  
dev.off()
?geom_text
#Task 3. Reproduce panel 2c: Heatmap across cell types and time####
"Goal: Visualize temporal structure across immune compartments.

Requirements:
  
  Read sheet c
Convert to matrix correctly
Build column annotations for:
  CellType
Time
Cluster rows only, not columns
Key thinking check:
  
  Why cluster genes but not time?"

data <- read_xlsx("data/hb_stage_2.xlsx", sheet = "c")
hm_mtx <- as.data.frame(data)
rownames(hm_mtx)<- data$genes
hm_mtx$genes<- NULL

#add annotation
samples <- colnames(hm_mtx)
annotation_df <- data.frame(
  Cell = stringr::str_extract(samples, "^[A-Za-z]+(?=n)"),
  Time = stringr::str_extract(samples, "[0-9]+h$")
)
rownames(annotation_df) <- samples

#add 
annotation_df <- annotation_df[order(annotation_df$Cell, annotation_df$Time), ]
hm_mtx <- hm_mtx[, rownames(annotation_df)]
png("plots/Figure3c.png",res = 300, width = 300*10, height = 300*10)

pheatmap(hm_mtx,
         annotation_col  = annotation_df,
         show_colnames = F, show_rownames = F,cluster_cols = F, fontsize = 18
)
dev.off()

#Task 4. Reproduce panel 2d: Pathway enrichment heatmap####
"Goal: Compare pathway-level responses across timepoints.

Requirements:
  
  Read sheet d_1
Use pathway names as rownames
No clustering
Diverging color scale centered at zero
Conceptual check:
  
  Why no clustering here?
  Why a diverging palette?"
data <- read_xlsx("data/hb_stage_2.xlsx", sheet = "d_1")
hm_m <- as.data.frame(data)
rownames(hm_m)=data$pathway
hm_m$pathway<- NULL
png("plots/Figure3d.png",res = 300, width = 300*10, height = 300*10)
pheatmap(hm_m, cluster_rows = F, cluster_cols = F, fontsize = 18)
dev.off()


#Task 5. Reproduce panel 2e: Bubble plot of kinetic regimes####
"Goal: Show count-weighted kinetic behavior.

Requirements:
  
  Read sheet e
Scatter plot:
  x = half_life
y = alpha
color = stage
size = count
Two legends:
  stage (color)
count (size)"

data <- read_xlsx("data/hb_stage_2.xlsx", sheet = "e")

png("plots/figure3e.png",res = 300, width = 300*5, height = 300*6)
ggplot(data,aes(x=alpha,y=half_life,colour  = stage, size = count))+geom_point()+
  scale_color_manual("stage", values = c("6h"="lightgreen","72h"="steelblue"))+
  theme_classic(base_size = 18, base_family = "Arial")
dev.off()



#Task 6: Reproduce panel 2f: Stacked proportions#####
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
  theme_classic(base_size = 18, base_family = "Arial")
dev.off()



#
#Task 7. Reproduce panel 2g: Directed cell–cell interaction network####
"Goal: Visualize directional communication changes.

Requirements:
  
  Read sheet g
Convert to adjacency matrix
Build directed graph
Remove zero-weight edges
Edge arrow size proportional to weight
Force-directed layout
Conceptual check:
  
  Why directed?
  What does edge weight encode biologically?"
data<- as.data.frame(readxl::read_xlsx("data/hb_stage_2.xlsx",sheet ='g' ))
rownames(data)<- data$...1
data$...1<-NULL
data<- as.matrix(data)
g <- graph_from_adjacency_matrix(data,
                                 mode = "directed",
                                 weighted = TRUE)
plot(g,
     edge.arrow.size = 0.5,
     edge.width = E(g)$weight * 5,
     vertex.size = 30)
# plot
png("plots/Figure3g.png",res = 300, width = 300*5, height = 300*5)

plot(g,
     edge.arrow.size = 1,
     edge.width = E(g)$weight * 5,
     vertex.size = 40)
dev.off()

#----------------------------------------------------------