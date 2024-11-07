########### Script for Abiotic Appendix H2 Figures ###########

library(reshape2)
library(ggplot2)
library(dplyr)

df <- read.csv("table3.csv")
df$Time <- as.factor(df$Time)
df$Sample <- factor(df$Sample,levels = c("H2O", "Granite_ctrl", "Sandstone_ctrl", "Basalt_ctrl", "Granite", "Sandstone", "Basalt"))

exp1 <- df[1:42,]
exp2 <- df[43:84,]
exp3 <- df[85:126,]
exp4 <- df[127:156,]

# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#CC79A7")


# <45um
p1 <- ggplot(exp1, aes(x=Time, y=H2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=H2-Std_Dev, ymax=H2+Std_Dev),
                position=position_dodge(.9)) + 
  theme_bw() + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("H2 Production - <45um") +
  xlab("Time (hours)") +
  ylab("H2 (nmol/g rock)") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_manual(values=cbPalette) + ylim(-60,405)
p1


# <45um edited
exp0 <- read.csv("table5.csv")

cbbbPalette <- c("#009E73", "springgreen", "#E69F00", "goldenrod1", "#CC79A7","plum1", "#56B4E9")



exp0$Time <- as.factor(exp0$Time)
exp0$Sample <- factor(exp0$Sample,levels = c("Granite + Water", "Granite only", "Sandstone + Water", "Sandstone only",
                                             "Basalt + Water", "Basalt only", "Water"))

p1 <- ggplot(exp0, aes(x=Time, y=H2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(), size=0.7) +
  geom_errorbar(aes(ymin=H2-Std_Dev, ymax=H2+Std_Dev),
                position=position_dodge(.9), 
                width = 0.25,  # Adjusts the width of the error bars
                size = 0.8) +  # Adjusts the thickness of the error bars
  theme_bw() + 
  #scale_fill_brewer(palette="Dark2") +
  ggtitle("H2 Production - <45um") +
  xlab("Time (hours)") +
  ylab(expression(H[2] ~ "(nmol/g rock)")) +
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),  
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),  
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position="top") +
  scale_fill_manual(values=cbbbPalette)+
  guides(fill = guide_legend(nrow = 1))
p1


# 45-106um
p2 <- ggplot(exp2, aes(x=Time, y=H2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=H2-Std_Dev, ymax=H2+Std_Dev),
                position=position_dodge(.9)) + 
  theme_bw() + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("H2 Production - 45-106um") +
  xlab("Time (hours)") +
  ylab("H2 (nmol/g rock)") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_manual(values=cbPalette) + ylim(-60,405)
p2

# 106-150um
p3 <- ggplot(exp3, aes(x=Time, y=H2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=H2-Std_Dev, ymax=H2+Std_Dev),
                position=position_dodge(.9)) + 
  theme_bw() + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("106-150um") +
  xlab("Time (hours)") +
  ylab("H2 (nmol/g rock)") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_manual(values=cbPalette) + ylim(-60,405)
p3

# <45um seawater
p4 <- ggplot(exp4, aes(x=Time, y=H2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=H2-Std_Dev, ymax=H2+Std_Dev),
                position=position_dodge(.9)) + 
  theme_bw() + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("<45um, seawater") +
  xlab("Time (hours)") +
  ylab("H2 (nmol/g rock)") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_manual(values=cbbPalette) + ylim(-60,405)
p4

########### FIGURE S.1.6B ########### 

granite <- read.csv("table4.csv")
granite$Sample <- gsub("um", "μm", granite$Sample)
granite$Time <- as.factor(granite$Time)

granite$Sample <- factor(granite$Sample,levels = c("Granite + Water (<45 μm)","Granite + Water (45-106 μm)","Granite + Water (106-150 μm)"))

cbbbPalette <- c("springgreen4","darkslateblue",  "darkturquoise")

p1 <- ggplot(granite, aes(x=Time, y=H2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(), size=0.7) +
  geom_errorbar(aes(ymin=H2-Std_Dev, ymax=H2+Std_Dev),
                position=position_dodge(.9), 
                width = 0.25,  # Adjusts the width of the error bars
                size = 0.8) +  # Adjusts the thickness of the error bars
  theme_bw() + 
  xlab("Time (hours)") +
  ylab(expression(H[2] ~ "(nmol/g rock)")) +
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),  
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),  
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "top") +
  scale_fill_manual(values=cbbbPalette)