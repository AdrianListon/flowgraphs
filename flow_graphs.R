# Script to generate graphs from flow cytometry data tables

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)


# Setting up-----------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./flow_graphs_functions.r")

if (!file.exists("./graphs"))
  dir.create(path = "./graphs")
if (!file.exists("./calculations"))
  dir.create(path = "./calculations")



## locate data----------

# Your input csv file should contain a column labeled "Total" with the total 
# number of leukocytes in each fcs file, a column labeled "cell counts" with 
# the hemocytometer (Countess) counts if you have them, and a column labeled 
# "fraction stained". You need either a hemocytometer count or a fraction 
# stained value for each sample.

input.file <- list.files( pattern = "*.numbers.csv")
if (length(input.file)!=1 ){
  print(error0)
}

input.data <- read.csv(input.file)
# remove mean and SD rows if present
input.data <- input.data[!input.data[,1] %in% c("Mean", "SD"),]
colnames(input.data)[1] <- "Sample"
sample.names <- input.data$Sample
input.data <- input.data[,-1]
rownames(input.data) <- sample.names

# run a couple checks on the input data
if ( sum(c("Total", "cell.count", "fraction.stained") %in% colnames(input.data)) != 3 ) {
  print(error1)
}

data.to.check <- subset(input.data, select = c(cell.count, fraction.stained))
if ( any(rowSums(is.na(data.to.check)) > 1 ) == TRUE ) {
  print(error2)
}
    
data.to.check <- subset(input.data, select = -c(cell.count, fraction.stained))
if (sum(is.na(data.to.check))!=0) {
  print(error3)
}

# User input required here-----------

# How many counting beads did you add?
bead.count <- 10000
# How many cells did you stain for samples you counted?
cells.stained <- 2e6

# set groups
group.names <- c("Spleen NOD", "Spleen C57", "Spleen BALB", "Spleen CBA", "Spleen CD1", "Spleen 129",
                 "pLN NOD", "pLN C57", "pLN BALB", "pLN CBA", "pLN CD1", "pLN 129",
                 "Pancreas NOD", "Pancreas C57", "Pancreas BALB", "Pancreas CBA", "Pancreas CD1", "Pancreas 129")
# set group sizes
group.sizes <- c(4,4,2,2,2,2,
                 4,4,2,2,2,2,
                 4,4,2,2,2,2)

groups.to.check <- sum(group.sizes)

if (groups.to.check != nrow(input.data)) {
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

# calculate total cell counts for organs without cell counts-------------------

input.data$cell.count <- ifelse(is.na(input.data$cell.count)==TRUE, 
                                10000/bead.count*input.data$Total/input.data$fraction.stained, 
                                input.data$cell.count)

# calculate fraction stained for counted samples
input.data$fraction.stained <- ifelse(is.na(input.data$fraction.stained)==TRUE, 
                                cells.stained/input.data$cell.count, 
                                input.data$fraction.stained)

stopifnot(is.na(input.data)==FALSE)


# calculate cell numbers-------------
calc.numbers <- function(column.count, total.count, live.count ) {
  result <- ( column.count/live.count*total.count )
}

cell.numbers <- input.data %>%
  select_if(is.numeric) %>%
  transmute(across(!beads & !cell.count & !fraction.stained, 
                   funs( calc.numbers(., cell.count, Total )), .names = "{.col}" ))

# calculate cell frequencies
calc.percentage <- function(column.count, total.count) {
  result <- ( column.count / total.count )*100
}

cell.frequencies <- cell.numbers %>%
  transmute(across(!Total , funs( percent = calc.percentage(., Total)), .names = "{.col}" ))

# add group column, export a copy of the calculated values in case you want to plot in another program

cell.numbers <- cell.numbers %>%
  mutate( ., Group = rep(group.names, group.sizes) )
cell.frequencies <- cell.frequencies %>%
  mutate( ., Group = rep(group.names, group.sizes) )
row.names(cell.numbers) <- sample.names
row.names(cell.frequencies) <- sample.names
colnames(cell.numbers) <- gsub(".", " ", colnames(cell.numbers), fixed = TRUE)
colnames(cell.frequencies) <- gsub(".", " ", colnames(cell.frequencies), fixed = TRUE)
write.csv(cell.numbers, file = "./calculations/calculated_cell_numbers.csv" )
write.csv(cell.frequencies, file = "./calculations/calculated_cell_frequencies.csv")

cell.numbers <- cell.numbers %>%
  mutate( ., group.color = rep(group.condition.color, group.sizes) )
cell.frequencies <- cell.frequencies %>%
  mutate( ., group.color = rep(group.condition.color, group.sizes) )

# plot and save summary graph----------------

cell.numbers.melted <- melt(cell.numbers, id.vars = c("Group", "group.color"))

summary.plot <- ggplot( cell.numbers.melted, 
                        aes(x = Group, y = value, fill = Group) )+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = group.condition.color)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  facet_grid(.~variable)+
  ylab("cell number")+
  theme_classic(base_line_size = 0.1)+
  theme(strip.background = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom")+
  annotation_logticks(sides = "l", outside = TRUE, 
                      short = unit(0.5, "mm"), mid = unit(0.5, "mm"), 
                      long = unit(1.5, "mm"), size = 0.1 )+
  coord_cartesian(clip = "off")+
  guides(fill = guide_legend(ncol = 4))

summary.plot

ggsave("./graphs/summary_plot.png")


# plot cell numbers for each cell type on log scale----------------

cell.numbers.long <- pivot_longer(cell.numbers, !Group & !group.color, 
                                  names_to = "Type", values_to = "Count")

plot.cell.numbers(plot.type)

# plot cell frequencies for each cell type on linear scale-------------------

cell.frequencies.long <- pivot_longer(cell.frequencies, !Group & !group.color,
                                      names_to = "Type", values_to = "Percent")

plot.cell.frequencies(plot.type)



# stats-------------

library(emmeans)

if (!file.exists("./stats"))
  dir.create(path = "./stats")

for (cell.type in unique(cell.numbers.long$Type)) {
  temp <- cell.numbers.long %>% 
    filter(Type == cell.type) %>%
    group_by(Group, group.color) %>%
    suppressMessages(summarise(Count = Count))
  
  res_aov <- aov( Count ~ Group, data = temp )
  
  fitted.em <- emmeans(res_aov, "Group", data = temp )
  
  p.values <- data.frame( pairs(fitted.em, adjust = "tukey" ) )
  p.values <- p.values[,c(1,6)]
  p.values$significant <- ifelse( p.values$p.value < 0.05, "Yes", "No" )
  
  write.csv(p.values, file = paste0("./stats/", cell.type, "_numbers_anova.csv"))
  
}


for (cell.type in unique(cell.frequencies.long$Type)) {
  temp <- cell.frequencies.long %>% 
    filter(Type == cell.type) %>%
    group_by(Group, group.color) %>%
    suppressMessages(summarise(Percent = Percent))
  
  res_aov <- aov( Percent ~ Group, data = temp )
  
  fitted.em <- emmeans(res_aov, "Group", data = temp )
  
  p.values <- data.frame( pairs(fitted.em, adjust = "tukey" ) )
  p.values <- p.values[,c(1,6)]
  p.values$significant <- ifelse( p.values$p.value < 0.05, "Yes", "No" )
  
  write.csv(p.values, file = paste0("./stats/", cell.type, "_frequencies_anova.csv"))
  
}












