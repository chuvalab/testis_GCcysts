
####################################################################################################
## Celine Roelse                                                                                  ##
## 2025-03-21                                                                                     ##
## Bridge characterization - panel KIF23 or TEX14 combined with MAGEA3 and DDX4                   ##
####################################################################################################

library(ggplot2)
library(ggthemes)
library(readxl)
library(Rmisc)
library(lattice)
library(plyr)
library(ggpubr)
library(dplyr)
library(ggsignif)
library(xlsx)


## Specify color-blind-compatible palette:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Set working directory
setwd("D:/R-projects/7B.1_bridges")

## Read data files
p3 <- read_excel("20250321_7B.1_TEX_quant_tidy_MAGE_branches.xlsx")                   
p3 <- as.data.frame(p3)


p3$age <- factor(p3$age, levels = c("WG15", "WG16", "WG17", "WG18", 
                                    "WG19", "WG20", "WG21", "Prepub", "Prepub2", "Adult", "Adult2"))



p3$branched_id <- as.character(p3$branched_id)

branched_count <- as.data.frame(table(p3$branched_id))

branched_plot <- ggplot(branched_count, aes(y=Freq, x="", fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Freq/sum(Freq)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set2") +
  ggtitle("Branched cells") + 
  labs(fill="GC type")

branched_plot

ggsave("./Plots_mage/branches/20250321_branchedid.pdf", width=5, height=3.5)
ggsave("./Plots_mage/branches/20250321_branchedid.png", width=5, height=3.5)


################

nonebranched <- subset(p3, branched_id=="none")

#? p3$branchingcell_id <- as.character(p3$branchingcell_id)

branchingtonone_count <- as.data.frame(table(nonebranched$branchingcell_id))

branchingtonone_plot <- ggplot(branchingtonone_count, aes(y=Freq, x="", fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Freq/sum(Freq)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set2") +
  ggtitle("GCs in cysts with unconnected bridges") + 
  labs(fill="GC type")

branchingtonone_plot

ggsave("./Plots_mage/branches/20250321_branchingtonone.pdf", width=5, height=3.5)
ggsave("./Plots_mage/branches/20250321_branchingtonone.png", width=5, height=3.5)


################

branching_count <- as.data.frame(table(p3$branchingcell_id))

branching_plot <- ggplot(branching_count, aes(y=Freq, x="", fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Freq/sum(Freq)*100), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set2") +
  ggtitle("Branching GCs in cysts") + 
  labs(fill="GC type")

branching_plot

ggsave("./Plots_mage/branches/20250321_branchingcells.pdf", width=5, height=3.5)
ggsave("./Plots_mage/branches/20250321_branchingcells.png", width=5, height=3.5)



##############################################

p3 <- p3 %>% mutate(newbranchpos = branch_pos - 1)
p3 <- p3 %>% mutate(newlonglen = longestlen_cyst - 1)

p3$relbranchpos <- (p3$newbranchpos)/(p3$newlonglen)

p3 <- p3 %>% mutate(newrelbranchpos = case_when(relbranchpos >= 0.5 ~ 1-relbranchpos,
                      TRUE ~ relbranchpos))


sum.relbranch <- summarySE(p3, measurevar='newrelbranchpos', groupvars = 'age', na.rm = T)

sum.relbranch$age <- factor(sum.relbranch$age, levels = c("WG15", "WG16", "WG17", "WG18", 
                                                          "WG19", "WG20", "WG21", "Prepub", "Prepub2", "Adult", "Adult2"))



relpos_plot <- ggplot(p3, aes(y=newrelbranchpos, x=age)) +
  geom_jitter(width=0.2) +
  geom_crossbar(data=sum.relbranch, aes(ymin = newrelbranchpos, ymax = newrelbranchpos),
                linewidth=0.5,col="red", width = .5)+
#  ylim(0, 1) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5), 
        legend.text = element_text(size=rel(1))) +
  ggtitle("Relative branch position") 

relpos_plot

ggsave("./Plots_mage/branches/20250331_average_relbranchposition.pdf", width=8, height=3.5)
ggsave("./Plots_mage/branches/20250331_average_relbranchposition.png", width=8, height=3.5)




p3 <- p3 %>% mutate(binrelpos = cut(newrelbranchpos, breaks=c(-.1, 0, 0.1, 0.2, 0.3, 0.4, 5)))


relpos_age <- prop.table(table(p3$age, p3$binrelpos), 1)
relpos_age <- as.data.frame(relpos_age)


relpos_plot_bin <- ggplot(relpos_age, aes(y=Freq, x=Var1, fill=Var2)) + 
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  geom_text(aes(label = paste0(round(Freq*100), "%")), position = position_stack(vjust=0.5, reverse=TRUE)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size=rel(1.4)), 
        axis.text.x = element_text(size=rel(1.5), vjust = 0.5), 
        axis.text.y = element_text(size=rel(1.5), hjust = 0.5)) +
  scale_fill_brewer(palette="Accent")

relpos_plot_bin

ggsave("./Plots_mage/branches/20250401_relbransposbinned.pdf", width=9, height=3.5)
ggsave("./Plots_mage/branches/20250401_relbransposbinned.png", width=9, height=3.5)


