########### Script for Abiotic Appendix H2O2 Figures ###########

library(reshape2)
library(ggplot2)

df <- read.csv("../data/table_45.csv")
df$Time <- as.factor(df$Time)
df$Sample <- factor(df$Sample,levels = c("Granite", "Sandstone", "Basalt"))

df2 <- read.csv("../data/table_45-106.csv")
df2$Time <- as.factor(df2$Time)
df2$Sample <- factor(df2$Sample,levels = c("Granite", "Sandstone", "Basalt"))

df3 <- read.csv("../data/table_106-150.csv")
df3$Time <- as.factor(df3$Time)
df3$Sample <- factor(df3$Sample,levels = c("Granite", "Sandstone", "Basalt"))

df4 <- read.csv("../data/table_H2O.csv")
df4$Time <- as.factor(df4$Time)
df4$Sample <- factor(df4$Sample,levels = c("H2O_Granite", "H2O_Sandstone", "H2O_Basalt", "H2O_Seawater"))

df5 <- read.csv("../data/table_GranBasSea.csv")
df5$Time <- as.factor(df5$Time)
df5$Sample <- factor(df5$Sample,levels = c("Granite", "Basalt"))

# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
cbbPalette <- c("#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#CC79A7")


# <45um
ggplot(df, aes(x=Time, y=H2O2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=H2O2-Std_Dev, ymax=H2O2+Std_Dev),
                position=position_dodge(.9)) + 
  theme_bw() + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Hydrogen Peroxide Production - <45 μm") +
  xlab("Time (hours)") +
  ylab(expression(H[2]*O[2]~"(μM)")) +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_color_manual(values=cbPalette) + ylim(0,15)

# <45um and water

df0 <- read.csv("../data/table_45_water.csv")
df0$Time <- as.factor(df0$Time)
df0$Sample <- factor(df0$Sample,levels = c("Granite + Water", "Sandstone + Water", "Basalt + Water", "Water"))
cbbbPalette <- c("#009E73","#E69F00",  "#CC79A7", "#56B4E9")

ggplot(df0, aes(x=Time, y=H2O2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(), size =0.7) +
  geom_errorbar(aes(ymin=H2O2-Std_Dev, ymax=H2O2+Std_Dev),
                position=position_dodge(.9), 
                width = 0.25,  # Adjusts the width of the error bars
                size = 0.8) +  # Adjusts the thickness of the error bars
  theme_bw() + 
  #scale_fill_brewer(palette="Dark2") +
  ggtitle("Hydrogen Peroxide Production - <45 μm") +
  xlab("Time (hours)") +
  ylab(expression(H[2]*O[2] ~ (mu*M))) +
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),  
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),  
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_fill_manual(values=cbbbPalette) + ylim(0,15)


# 45-106um
ggplot(df2, aes(x=Time, y=H2O2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=H2O2-Std_Dev, ymax=H2O2+Std_Dev),
                position=position_dodge(.9)) + 
  theme_bw() + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Hydrogen Peroxide Production - 45-106 μm") +
  xlab("Time (hours)") +
  ylab(expression(H[2]*O[2]~"(μM)")) +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_color_manual(values=cbPalette) + ylim(0,15)


# 106-150um
ggplot(df3, aes(x=Time, y=H2O2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=H2O2-Std_Dev, ymax=H2O2+Std_Dev),
                position=position_dodge(.9)) + 
  theme_bw() + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Hydrogen Peroxide Production - 106-150 μm") +
  xlab("Time (hours)") +
  ylab(expression(H[2]*O[2]~"(μM)")) +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_color_manual(values=cbPalette) + ylim(0,15)


# H2O
ggplot(df4, aes(x=Time, y=H2O2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=H2O2-Std_Dev, ymax=H2O2+Std_Dev),
                position=position_dodge(.9)) + 
  theme_bw() + 
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Hydrogen Peroxide Production - Water Controls") +
  xlab("Time (hours)") +
  ylab(expression(H[2]*O[2]~"(μM)")) +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_color_manual(values=cbPalette) + ylim(0,15)


# <45um Seawater
cbbbPalette <- c("#1b9e77", "#7570b3")

ggplot(df5, aes(x=Time, y=H2O2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=H2O2-Std_Dev, ymax=H2O2+Std_Dev),
                position=position_dodge(.9)) + 
  theme_bw() + 
  #scale_fill_brewer(palette="Dark2") +
  ggtitle("Hydrogen Peroxide Production - <45 μm, seawater") +
  xlab("Time (hours)") +
  ylab(expression(H[2]*O[2]~"(μM)")) +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_manual(values = cbbbPalette, labels = c("Granite", "Basalt")) + ylim(0,15)

########### FIGURE S.1.6A ########### 

df6 <- read.csv("../data/table_granite.csv")
df6$Sample <- gsub("um", "μm", df6$Sample)
df6$Time <- as.factor(df6$Time)
df6$Sample <- factor(df6$Sample,levels = c("Granite + Water (<45 μm)","Granite + Water (45-106 μm)","Granite + Water (106-150 μm)"))
cbbbPalette <- c("springgreen4","darkslateblue",  "darkturquoise")

ggplot(df6, aes(x=Time, y=H2O2, fill=Sample)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(), size =0.7) +
  geom_errorbar(aes(ymin=H2O2-Std_Dev, ymax=H2O2+Std_Dev),
                position=position_dodge(.9), 
                width = 0.25,  # Adjusts the width of the error bars
                size = 0.8) +  # Adjusts the thickness of the error bars
  theme_bw() + 
  xlab("Time (hours)") +
  ylab(expression(H[2]*O[2] ~ (mu*M))) +
  theme(plot.title=element_text(hjust=0.5),
        axis.title.x = element_text(size = 16, margin = margin(t = 20)),  
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),  
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  scale_fill_manual(values=cbbbPalette) + ylim(0,15)

