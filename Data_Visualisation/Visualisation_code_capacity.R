require(ggplot2)
require(plyr)
require(tidyverse)

# User input
firstNode  = "111"
secondNode = "122"
# secondNode = "13333"
histYear = 13

shutdown = TRUE

# CaseStudy = "1"
# CaseStudy = "2"
# CaseStudy = "3"
# CaseStudy = "4a"
# CaseStudy = "4b"
# CaseStudy = "4c"
# CaseStudy = "4d"
# CaseStudy = "5"
# CaseStudy = "6a"
# CaseStudy = "6b"
# CaseStudy = "8a"
# CaseStudy = "8b"
# CaseStudy = "8a_shut"
CaseStudy = "8b_shut"


Huntly = 3

dir <- paste("C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\Case_Studies3\\CS ", CaseStudy, sep = "")
setwd(dir)

# # # # # DATA       
# Read in JuDGE output for a given node
tree.struct <- read.csv(paste(dir,"\\7node-tree.csv", sep = ""), header = TRUE)
JuDGE <- read.csv(paste(dir,"\\JuDGE_expansions.csv", sep = ""), header = TRUE)
costs <- read.csv(paste(dir,"\\Costs_and_parameters.csv", sep = ""), header = TRUE, fileEncoding="UTF-8-BOM")

# Tech type
tech.types <- colnames(costs)[3:ncol(costs)]
if (shutdown) {tech.types <- c(tech.types, tech.types[c(1, 2)])} 
for (i in c("CCS", "Geothermal", "HydroR", "HydroS", "Battery", "Wind")) {tech.types[startsWith(tech.types, i)] <- i}


# Calculate solar GWh generation
myfiles = list.files(path="C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\JuDGE_model_development\\TP_Portfolio_CSVs", pattern="*_00000_*", full.names=TRUE)
solar_GWh_scaling <- sum(as_tibble(ldply(myfiles, read_csv))$Solar) * 0.5 / 1000

# # # GGPLOT 1: Capacity Investments

JuDGE.subs <- JuDGE[JuDGE$variable %in% c("x", "x_shut"), ]
JuDGE.subs$value <- round(JuDGE.subs$value * (JuDGE.subs$value > 0.999))
JuDGE.subs <- cbind(tech.types, JuDGE.subs)

temp <- as.data.frame(unname(costs[4, 3:ncol(costs)]))
if (shutdown) {JuDGE.subs$Existing <- rep(t(cbind(as.data.frame(temp), 785, 350)), length(unique(JuDGE.subs$node)))} else {JuDGE.subs$Existing <- rep(t(as.data.frame(temp)), length(unique(JuDGE.subs$node)))}
temp <- as.data.frame(unname(costs[5, 3:ncol(costs)]))
if (shutdown) {JuDGE.subs$Investment <- rep(t(cbind(as.data.frame(temp), 0, 0)), length(unique(JuDGE.subs$node)))} else {JuDGE.subs$Investment <- rep(t(as.data.frame(temp)), length(unique(JuDGE.subs$node)))}


JuDGE.subs$Removed <- ifelse(JuDGE.subs$variable == "x_shut", JuDGE.subs$value * JuDGE.subs$Existing, 0)
JuDGE.subs$Invested <- ifelse(JuDGE.subs$variable == "x", JuDGE.subs$value * JuDGE.subs$Investment, 0)

for (i in unique(JuDGE.subs$node)) {
  # cat(i)
  if (nchar(i) >= Huntly) {
    # cat(i)
    # JuDGE.subs[JuDGE.subs$tech.types == "Coal", "Existing"] = 0
    JuDGE.subs[JuDGE.subs$node == i & JuDGE.subs$tech.types == "Coal", "Removed"] = 500}}

# Inc solar
solar <- tree.struct[, c(1, 4)]
zz <- as.data.frame(cbind("Solar", as.numeric(solar$n), 0, 0, 0, as.numeric(solar$solar_MW), 0, 0, 0))
colnames(zz) <- colnames(JuDGE.subs)
JuDGE.subs <- rbind(JuDGE.subs, zz)

JuDGE.subs$Total <- as.numeric(JuDGE.subs$Existing) + as.numeric(JuDGE.subs$Invested) - as.numeric(JuDGE.subs$Removed)

# df <- JuDGE.subs %>% group_by(node, tech.types) %>% filter(tech.types != "Battery") %>%
#   summarise(Existing = sum(as.numeric(Existing) - as.numeric(Removed)), Investments = sum(as.numeric(Invested)), Removed = sum(as.numeric(Removed)), Total = sum(as.numeric(Total)))
df <- JuDGE.subs %>% group_by(node, tech.types) %>% 
  summarise(Existing = sum(as.numeric(Existing) - as.numeric(Removed)), Investments = sum(as.numeric(Invested)), Removed = sum(as.numeric(Removed)), Total = sum(as.numeric(Total)))


# Stack the dataframe
dat_long <- df[c(1:5)] %>% gather("Stat", "Value", -c(node, tech.types))
dat_long$Total <- c(df$Total, rep(NA, 2* length(df$Total)))
dat_long$Total <- ifelse(dat_long$Total == 0, NA,dat_long$Total)


dat_long$Stat <- relevel(as.factor(dat_long$Stat), 2)

mynode = 1
for (mynode in c(1, firstNode, secondNode)) {
# for (mynode in unique(JuDGE.subs$node)) {
  # Graphs of investments at a certain node
  dat_long %>% filter(node == mynode) %>% 
    # relevel(as.factor(dat_long$Stat), c("Invested", "Existing", "Removed")) %>%
  ggplot() +
    geom_bar(aes(x = tech.types, y = as.numeric(Value), fill = Stat, alpha = Stat), position="stack", stat = "identity",color="white", size = 1) + 
    xlab("Generation Technology Type") + ylab("Total Capacity (MW)")+ guides(alpha=FALSE) + ylim(0, 4800) +
    geom_text(aes(x = tech.types, y = Total, label=paste(round(Total), "MW")), position=position_dodge(width=0.9), vjust=-0.5, size = 3.5) +
    ggtitle(paste("Electric Power Generation Stack Capacity at node", mynode, "for the years",2020 + 5*(nchar(mynode)-1), "-", 2020 + 5*nchar(mynode))) +
    # ggtitle(paste("Electric Power Generation Stack Capacity at node", mynode, " Toy example 2 (without lag)")) +
    scale_alpha_manual(values = c(0.8, 1, 0.8)) + 
    scale_fill_manual("Legend", values = c("Existing" = "black", "Investments" = "orange", "Removed" = "red")) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste(dir, "\\ggplot_CS_", CaseStudy, "_node_", mynode, ".png", sep = ""), plot = last_plot())
}






