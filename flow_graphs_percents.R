# Script to generate graphs from flow cytometry data tables

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)


# Setting up-----------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./flow_graphs_functions.r")

if (!file.exists("./percent_graphs"))
  dir.create(path = "./percent_graphs")

# locate data
percents.input.file <- list.files( pattern = "*.percents.csv")

if (length(percents.input.file)!=1 ){
  print(error0)
}

percents.input.data <- read.csv(percents.input.file)
# remove mean and SD rows if present
percents.input.data <- percents.input.data[!percents.input.data[,1] %in% c("Mean", "SD"),]
colnames(percents.input.data)[1] <- "Sample"
sample.names <- percents.input.data$Sample
percents.input.data <- percents.input.data[,-1]
rownames(percents.input.data) <- sample.names
colnames(percents.input.data) <- gsub(".", "-", colnames(percents.input.data), fixed = TRUE)

# check for NAs in columns
if (sum(is.na(percents.input.data))!=0) {
  print(error3)
}

# User input required here-----------

# set groups-----------------
# What cell type is this?
cell.type <- "Treg"

group.names <- c("Spleen NOD", "Spleen C57", "Spleen BALB", "Spleen CBA", "Spleen CD1", "Spleen 129",
                 "pLN NOD", "pLN C57", "pLN BALB", "pLN CBA", "pLN CD1", "pLN 129",
                 "Pancreas NOD", "Pancreas C57", "Pancreas BALB", "Pancreas CBA", "Pancreas CD1", "Pancreas 129")
# set group sizes
group.sizes <- c(4,4,2,2,2,2,
                 4,4,2,2,2,2,
                 4,4,2,2,2,2)

groups.to.check <- sum(group.sizes)

if (groups.to.check != nrow(percents.input.data)) {
  print(error4)
}

if (length(group.names) != length(group.sizes)) {
  print(error5)
}


# modify graph parameters (optional)----------

# specify graph type: options are violin = violin with points, box = box&whisker with points, 
# scatter = points only, or bar = barchart with mean +/- SE

plot.type <- "box"

# to set specific colors for each group 
# write them in below using either the name or the hex code (e.g., #0000FF = blue)
# see https://r-graph-gallery.com/ggplot2-color.html

group.condition.color <- c("#0000FF", "#FFFACD", "#9400D3", "#7FFFD4", "#EE82EE", "#6495ED", 
                           "#0000FF", "#FFFACD", "#9400D3", "#7FFFD4", "#EE82EE", "#6495ED",
                           "#0000FF", "#FFFACD", "#9400D3", "#7FFFD4", "#EE82EE", "#6495ED")
names(group.condition.color) <- group.names

if (length(group.condition.color) != length(group.names)) {
  print(error6)
}


# Now execute the rest of the script to generate your graphs. 

# add group column
percents.input.data <- percents.input.data %>%
  mutate( ., Group = rep(group.names, group.sizes) )
row.names(percents.input.data) <- sample.names
percents.input.data <- percents.input.data %>%
  mutate( ., group.color = rep(group.condition.color, group.sizes) )

# plot and save summary graph

summary.plot.percents <- ggplot( melt(percents.input.data, id.vars = c("Group", "group.color")), 
                        aes(x = Group, y = value, fill = Group) )+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = group.condition.color)+
  facet_grid(.~variable)+
  ylab(paste("Percent of ", cell.type, sep = ""))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 4))

summary.plot.percents

ggsave("./percent_graphs/summary_plot_percents.png")


# plot cell frequencies for each cell type on linear scale-------------------

cell.percents.long <- pivot_longer(percents.input.data, !Group & !group.color, 
                                   names_to = "Marker", values_to = "Percent")

plot.cell.percents(plot.type)


# stats------------

library(emmeans)

if (!file.exists("./percent_stats"))
  dir.create(path = "./percent_stats")

for (gene in unique(cell.percents.long$Marker)) {
  temp <- cell.percents.long %>% 
    filter(Marker == gene ) %>%
    group_by(Group, group.color) %>%
    suppressMessages(summarise(Percent = Percent))
  
  res_aov <- aov( Percent ~ Group, data = temp )
  
  fitted.em <- emmeans(res_aov, "Group", data = temp )
  
  p.values <- data.frame( pairs(fitted.em, adjust = "tukey" ) )
  p.values <- p.values[,c(1,6)]
  p.values$significant <- ifelse( p.values$p.value < 0.05, "Yes", "No" )
  
  write.csv(p.values, file = paste0("./percent_stats/", cell.type, "_", gene, "_percents_anova.csv"))
  
}
