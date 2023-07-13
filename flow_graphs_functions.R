# flow_graphs source functions


# define cell count plotting function--------------

plot.cell.numbers <- function(plot.type = "violin") {
  if (plot.type == "violin") {
    
    create.cell.plot <-function(cell.type) {
      temp <- cell.numbers.long %>% 
        filter(Type == cell.type) %>%
        group_by(Group, group.color) %>%
        summarise(Count = Count)
      
      p <- ggplot(temp, aes(x = Group, y = Count, fill = Group))+
        geom_violin(alpha=0.5, size = 0.1)+
        geom_point(aes(fill = Group), color = "black",
                   position = position_jitter(seed = 1, width = 0.1), 
                   shape = 21, stroke = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x)))+
        labs(x = "Group", y = "cell numbers", title = cell.type )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
        annotation_logticks(sides = "l", outside = TRUE, short = unit(0.5, "mm"),
                            mid = unit(0.5, "mm"), long = unit(1.5, "mm"), size = 0.1 )+
        coord_cartesian(clip = "off")
      
      ggsave(p, file = paste0("./graphs/", cell.type, "_numbers.png"))
    }
    
    for (x in unique(cell.numbers.long$Type)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else if (plot.type == "box") {
    
    create.cell.plot <-function(cell.type) {
      temp <- cell.numbers.long %>% 
        filter(Type == cell.type) %>%
        group_by(Group, group.color) %>%
        summarise(Count = Count)
      
      p <- ggplot(temp, aes(x = Group, y = Count, fill = Group))+
        geom_boxplot(alpha=0.5, size = 0.1)+
        geom_point(aes(fill = Group), color = "black",
                   position = position_jitter(seed = 1, width = 0.1), 
                   shape = 21, stroke = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x)))+
        labs(x = "Group", y = "cell numbers", title = cell.type )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
        annotation_logticks(sides = "l", outside = TRUE, short = unit(0.5, "mm"),
                            mid = unit(0.5, "mm"), long = unit(1.5, "mm"), size = 0.1 )+
        coord_cartesian(clip = "off")
      
      ggsave(p, file = paste0("./graphs/", cell.type, "_numbers.png"))
    }
    
    for (x in unique(cell.numbers.long$Type)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else if (plot.type == "scatter") {
    
    create.cell.plot <-function(cell.type) {
      temp <- cell.numbers.long %>% 
        filter(Type == cell.type) %>%
        group_by(Group, group.color) %>%
        summarise(Count = Count, mean = mean(Count), se = sd(Count)/sqrt(length(Count)))
      
      p <- ggplot(temp, aes(x = Group, y = Count, ymin=mean-se, ymax=mean+se, fill = Group))+
        geom_point(aes(fill = Group), color = "black",
                   position = position_jitter(seed = 1, width = 0.1), 
                   shape = 21, stroke = 0.1 )+
        geom_errorbar( size = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x)))+
        labs(x = "Group", y = "cell numbers", title = cell.type )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
        annotation_logticks(sides = "l", outside = TRUE, short = unit(0.5, "mm"),
                            mid = unit(0.5, "mm"), long = unit(1.5, "mm"), size = 0.1 )+
        coord_cartesian(clip = "off")
      
      ggsave(p, file = paste0("./graphs/", cell.type, "_numbers.png"))
    }
    
    for (x in unique(cell.numbers.long$Type)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else if (plot.type == "bar") {
    
    create.cell.plot <-function(cell.type) {
      temp <- cell.numbers.long %>% 
        filter(Type == cell.type) %>%
        group_by(Group, group.color) %>%
        summarise(Count = Count, mean = mean(Count), se = sd(Count)/sqrt(length(Count)))
      
      p <- ggplot(temp, aes(x = Group, y = mean, ymin=mean-se, ymax=mean+se, fill = Group))+
        geom_bar(position = "dodge", stat = "identity")+
        geom_errorbar( size = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x)))+
        labs(x = "Group", y = "cell numbers", title = cell.type )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
        annotation_logticks(sides = "l", outside = TRUE, short = unit(0.5, "mm"),
                            mid = unit(0.5, "mm"), long = unit(1.5, "mm"), size = 0.1 )+
        coord_cartesian(clip = "off")
      
      ggsave(p, file = paste0("./graphs/", cell.type, "_numbers.png"))
    }
    
    for (x in unique(cell.numbers.long$Type)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else {
    print("Error in defining plot type")
  } 
}



plot.cell.frequencies <- function(plot.type = "violin") {
  if (plot.type == "violin") {
    
    create.cell.plot <-function(cell.type) {
      temp <- cell.frequencies.long %>% 
        filter(Type == cell.type) %>%
        group_by(Group, group.color) %>%
        summarise(Percent = Percent)
      
      p <- ggplot(temp, aes(x = Group, y = Percent, fill = Group))+
        geom_violin(alpha=0.5, size = 0.1)+
        geom_point(aes(fill = Group), color = "black",
                   position = position_jitter(seed = 1, width = 0.1), 
                   shape = 21, stroke = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        labs(x = "Group", y = "Percent of leukocytes", title = cell.type )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
      ggsave(p, file = paste0("./graphs/", cell.type, "_frequencies.png"))
    }
    
    for (x in unique(cell.frequencies.long$Type)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else if (plot.type == "box") {
    
    create.cell.plot <-function(cell.type) {
      temp <- cell.frequencies.long %>% 
        filter(Type == cell.type) %>%
        group_by(Group, group.color) %>%
        summarise(Percent = Percent)
      
      p <- ggplot(temp, aes(x = Group, y = Percent, fill = Group))+
        geom_boxplot(alpha=0.5, size = 0.1)+
        geom_point(aes(fill = Group), color = "black",
                   position = position_jitter(seed = 1, width = 0.1), 
                   shape = 21, stroke = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        labs(x = "Group", y = "Percent of leukocytes", title = cell.type )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
      ggsave(p, file = paste0("./graphs/", cell.type, "_frequencies.png"))
    }
    
    for (x in unique(cell.frequencies.long$Type)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else if (plot.type == "scatter") {
    
    create.cell.plot <-function(cell.type) {
      temp <- cell.frequencies.long %>% 
        filter(Type == cell.type) %>%
        group_by(Group, group.color) %>%
        summarise(Percent = Percent, mean = mean(Percent), se = sd(Percent)/sqrt(length(Percent)))
      
      p <- ggplot(temp, aes(x = Group, y = Percent, ymin=mean-se, ymax=mean+se, fill = Group))+
        geom_point(aes(fill = Group), color = "black",
                   position = position_jitter(seed = 1, width = 0.1), 
                   shape = 21, stroke = 0.1 )+
        geom_errorbar( size = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        labs(x = "Group", y = "Percent of leukocytes", title = cell.type )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
      ggsave(p, file = paste0("./graphs/", cell.type, "_frequencies.png"))
    }
    
    for (x in unique(cell.frequencies.long$Type)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else if (plot.type == "bar") {
    
    create.cell.plot <-function(cell.type) {
      temp <- cell.frequencies.long %>% 
        filter(Type == cell.type) %>%
        group_by(Group, group.color) %>%
        summarise(Percent = Percent, mean = mean(Percent), se = sd(Percent)/sqrt(length(Percent)))
      
      p <- ggplot(temp, aes(x = Group, y = mean, ymin=mean-se, ymax=mean+se, fill = Group))+
        geom_bar(position = "dodge", stat = "identity")+
        geom_errorbar( size = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        labs(x = "Group", y = "Percent of leukocytes", title = cell.type )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
      ggsave(p, file = paste0("./graphs/", cell.type, "_frequencies.png"))
    }
    
    for (x in unique(cell.frequencies.long$Type)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else {
    print("Error in defining plot type")
  } 
}



plot.cell.percents <- function(plot.type = "violin") {
  if (plot.type == "violin") {
    
    create.cell.plot <-function(gene) {
      temp <- cell.percents.long %>% 
        filter(Marker == gene) %>%
        group_by(Group, group.color) %>%
        summarise(Percent = Percent)
      
      p <- ggplot(temp, aes(x = Group, y = Percent, fill = Group))+
        geom_violin(alpha=0.5, size = 0.1)+
        geom_point(aes(fill = Group), color = "black",
                   position = position_jitter(seed = 1, width = 0.1), 
                   shape = 21, stroke = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        labs(x = "Group", y = paste("Percent of ", cell.type, sep = ""), title = gene )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
      ggsave(p, file = paste0("./percent_graphs/", cell.type, "_", gene, "_percent.png"))
    }
    
    for (x in unique(cell.percents.long$Marker)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else if (plot.type == "box") {
    
    create.cell.plot <-function(gene) {
      temp <- cell.percents.long %>% 
        filter(Marker == gene) %>%
        group_by(Group, group.color) %>%
        summarise(Percent = Percent)
      
      p <- ggplot(temp, aes(x = Group, y = Percent, fill = Group))+
        geom_boxplot(alpha=0.5, size = 0.1)+
        geom_point(aes(fill = Group), color = "black",
                   position = position_jitter(seed = 1, width = 0.1), 
                   shape = 21, stroke = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        labs(x = "Group", y = paste("Percent of ", cell.type, sep = ""), title = gene )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
      ggsave(p, file = paste0("./percent_graphs/", cell.type, "_", gene, "_percent.png"))
    }
    
    for (x in unique(cell.percents.long$Marker)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else if (plot.type == "scatter") {
    
    create.cell.plot <-function(gene) {
      temp <- cell.percents.long %>% 
        filter(Marker == gene) %>%
        group_by(Group, group.color) %>%
        summarise(Percent = Percent, mean = mean(Percent), se = sd(Percent)/sqrt(length(Percent)))
      
      p <- ggplot(temp, aes(x = Group, y = Percent, ymin=mean-se, ymax=mean+se, fill = Group))+
        geom_point(aes(fill = Group), color = "black",
                   position = position_jitter(seed = 1, width = 0.1), 
                   shape = 21, stroke = 0.1 )+
        geom_errorbar( size = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        labs(x = "Group", y = paste("Percent of ", cell.type, sep = ""), title = gene )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
      ggsave(p, file = paste0("./percent_graphs/", cell.type, "_", gene, "_percent.png"))
    }
    
    for (x in unique(cell.percents.long$Marker)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else if (plot.type == "bar") {
    
    create.cell.plot <-function(gene) {
      temp <- cell.percents.long %>% 
        filter(Marker == gene) %>%
        group_by(Group, group.color) %>%
        summarise(Percent = Percent, mean = mean(Percent), se = sd(Percent)/sqrt(length(Percent)))
      
      p <- ggplot(temp, aes(x = Group, y = mean, ymin=mean-se, ymax=mean+se, fill = Group))+
        geom_bar(position = "dodge", stat = "identity")+
        geom_errorbar( size = 0.1 )+
        scale_fill_manual(values = group.condition.color)+
        labs(x = "Group", y = paste("Percent of ", cell.type, sep = ""), title = gene )+
        theme_classic(base_line_size = 0.1)+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      
      ggsave(p, file = paste0("./percent_graphs/", cell.type, "_", gene, "_percent.png"))
    }
    
    for (x in unique(cell.percents.long$Marker)) {
      suppressMessages(create.cell.plot(x))
    }
    
  } else {
    print("Error in defining plot type")
  } 
}



# Error codes -----------------------

error0 <- "Your input csv filename needs to end in the word numbers, 
          and there should only be one such file in your working directory"

error1 <- "You need columns called Total, cell count and fraction stained"

error2 <- "You need either a cell count or a fraction stained value for each sample"

error3 <- "Check data for missing values"

error4 <- "Check matching between group.sizes and your data"

error5 <- "Check matching between groups (group.names) and n (group.sizes)"

error6 <- "Check matching between group.sizes and your color choices"

