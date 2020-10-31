require(igraph)
require(ggplot2)
require(plyr)
require(tidyverse)

# User input
# firstNode  = "11111"
# secondNode = "12422"
firstNode  = "112"
secondNode = "121"

histYear = 5

shutdown = TRUE                                   # <- Don't keep forgetting this, dumbass

showPaths = TRUE     
sharedPaths = TRUE

# CaseStudy = "1"
# CaseStudy = "1b"
# CaseStudy = "1b2"
# CaseStudy = "2"
# CaseStudy = "3"
# CaseStudy = "4a"
# CaseStudy = "4b"
# CaseStudy = "4c"
# CaseStudy = "4d"
# CaseStudy = "5"
# CaseStudy = "6a"
# CaseStudy = "6b"
# CaseStudy = "7a"
# CaseStudy = "7b"
# CaseStudy = "7c"
# CaseStudy = "7d"
# CaseStudy = "8a"
# CaseStudy = "8b"
# CaseStudy = "8a_shut"
CaseStudy = "8b_shut"


Huntly = 3

dir <- paste("C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\Case_Studies3\\CS ", CaseStudy, sep = "")
setwd(dir)

# Node trajectory to highlight
firstPath = c()
secondPath = c()
if (showPaths) {for (i in 1:nchar(firstNode)) {firstPath[i] = substr(firstNode, 1, i)}}else{firstPath[1] = firstNode}
if (showPaths & sharedPaths) {for (i in 1:nchar(secondNode)) {secondPath[i] = substr(secondNode, 1, i)}}else{secondPath[1] = secondNode}

if (sharedPaths) {
  while (sum(firstPath == secondPath) > 1) {
    firstPath <- firstPath[-1]
    secondPath <- secondPath[-1]
  }
}

# # # # # DATA       
# Read in JuDGE output for a given node
tree.struct <- read.csv(paste(dir,"\\7node-tree.csv", sep = ""), header = TRUE)                                  # May have to change this
JuDGE <- read.csv(paste(dir,"\\JuDGE_expansions.csv", sep = ""), header = TRUE)
costs <- read.csv(paste(dir,"\\Costs_and_parameters.csv", sep = ""), header = TRUE, fileEncoding="UTF-8-BOM")

# tech.types <- colnames(costs)[3:ncol(costs)]
# if (shutdown) {tech.types <- c(tech.types, tech.types[c(1, 2)])} 
# for (i in c("CCS", "Geothermal", "HydroR", "HydroS", "Battery", "Wind")) {tech.types[startsWith(tech.types, i)] <- i}


# Calculate solar GWh generation
myfiles = list.files(path="C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\JuDGE_model_development\\TP_Portfolio_CSVs", pattern="*_00000_*", full.names=TRUE)
solar_GWh_scaling <- sum(as_tibble(ldply(myfiles, read_csv))$Solar) * 0.5 / 1000

# GGPLOT 2: 

for (myNode in c(firstNode, secondNode)) {
  
  myPath <- c()
  for (i in 1:nchar(myNode)) {myPath[i] = substr(myNode, 1, i)}

  JuDGE.subs6 <- JuDGE[JuDGE$node %in% myPath & JuDGE$variable == "y", ]
  tech.types <- colnames(costs)[3:ncol(costs)]
  for (i in c("CCS", "Geothermal", "HydroR", "HydroS", "Battery", "Wind")) {tech.types[startsWith(tech.types, i)] <- i}
  JuDGE.subs6 <- cbind(tech.types, JuDGE.subs6)
  temp <- unlist(strsplit(as.character(JuDGE.subs6$index), ","))
  JuDGE.subs6$tech <- as.numeric(gsub("\\[", "", temp[seq(1, length(temp), 3)]))
  JuDGE.subs6$histYears <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 3)+2]))
  
  if (shutdown) {
    shut <- JuDGE[JuDGE$node %in% myPath & JuDGE$variable %in% "y_shut", ]
    shut$value <- round(shut$value * (shut$value > 0.999))
    tech.types <- tech.types[c(1, 2)]
    shut <- cbind(tech.types, shut)
    temp <- unlist(strsplit(as.character(shut$index), ","))
    shut$tech <- as.numeric(gsub("\\[", "", temp[seq(1, length(temp), 3)]))
    shut$histYears <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 3)+2]))
    JuDGE.subs6 <- rbind(JuDGE.subs6, shut)
  }
    
  JuDGE.subs6 <- subset(JuDGE.subs6, histYears == histYear)
  
  JuDGE.subs6_new <- JuDGE.subs6 %>% group_by(node, tech.types) %>% summarise(totalGWh = sum(value)/1000)
  JuDGE.subs6_new$tech_ind <- rep(seq(1, 10), length(unique(JuDGE.subs6_new$node)))

  # Add in solar
  tree.struct6 <- tree.struct[tree.struct$n %in% myPath, c(1, 4)]
  zz <- as.data.frame(cbind(as.integer(tree.struct6$n), "Solar", 0, 0))
  colnames(zz) <- colnames(JuDGE.subs6_new)
  zz$totalGWh <- as.numeric(tree.struct6$solar_MW) * solar_GWh_scaling
  zz$tech_ind <- as.integer(zz$tech_ind)
  JuDGE.subs6_new <- rbind(as.data.frame(JuDGE.subs6_new), zz)
  
  # Wind
  capacity <- JuDGE[JuDGE$node %in% myPath & JuDGE$variable == "x", ]
  capacity$tech <- as.numeric(gsub("\\[|\\]", "", capacity$index))
  capacity <- subset(capacity, tech >= max(capacity$tech) - 4)
  wind.data <- read.csv("C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\Data_Visualisation\\windfactordata.csv", header = TRUE)
  wind.data <- wind.data[, c(4, 6, 8, 9, 10)]
  wind.scale <- wind.data%>%gather() %>% group_by(key) %>% summarise(windFact = sum(value))
  capacity <- cbind(capacity, wind.scale)
  temp <- as.data.frame(unname(costs[4, (max(capacity$tech) - 2):ncol(costs)]))
  capacity$Existing <- rep(t(as.data.frame(temp)), length(unique(capacity$node)))
  temp <- as.data.frame(unname(costs[5, (max(capacity$tech) - 2):ncol(costs)]))
  capacity$Investment <- rep(t(as.data.frame(temp)), length(unique(capacity$node)))
  capacity$Investment <- ifelse(capacity$variable == "x", capacity$value * capacity$Investment, 0)
  capacity$Total <- capacity$Investment + capacity$Existing
  capacity <- capacity %>% group_by(node) %>% summarise(totalGWh = sum(Total * windFact)/1000)
  capacity <- cbind("Wind", capacity)
  capacity$tech_ind <- 10
  colnames(capacity)[1] <- "tech.types"
  JuDGE.subs6_new <- rbind(JuDGE.subs6_new, capacity[, c(2, 1, 3, 4)])
  
  JuDGE.subs6_new <- JuDGE.subs6_new %>% filter(tech.types != "Battery")
  
  MWlabels <- JuDGE.subs6_new %>% group_by(node) %>% summarise(tot = sum(totalGWh))
  # JuDGE.subs6_new$totalGWh <- ifelse(JuDGE.subs6_new$totalGWh == 0, NA, JuDGE.subs6_new$totalGWh)
  colnames(JuDGE.subs6_new)[2] <- "Generation_Tech"
  JuDGE.subs6_new$totalGWh <- ifelse(JuDGE.subs6_new$totalGWh == 0, 0.000001, JuDGE.subs6_new$totalGWh)
  
  mytitle = c()
  if (CaseStudy == "1") {
    mytitle = "No emissions constraints imposed"
  } else if (CaseStudy == "2") {
    mytitle = "100% renewable by 2030, Sixth Labour Government policy"
  } else if (CaseStudy == "3"){
    mytitle = "100% renewable in a normal hydrological year by 2035, prior Sixth Labour Government policy"
  }
  
  ggplot() +
    geom_col(data = JuDGE.subs6_new, aes(x = as.factor(node), y = totalGWh, fill = Generation_Tech), width = 0.4,color="white", size = 0.5) +
    geom_text(data = MWlabels, aes(x = as.factor(node), y = tot, label=paste(round(tot), "GWh")), position=position_dodge(width=0.9), vjust=-0.7, size = 3.5) +
    xlab("Node in Stage") + ylab("Total Generation (GWh)") + ylim(0, 100000) +
    ggtitle(paste("Electric Power Annual GWh Generation for the years ",2020 + 5*(nchar(myPath[1])-1), " - ", 2020 + 5*nchar(myPath[length(myPath)]), "\n", mytitle, sep = "")) +
    theme(text = element_text(size = 20)) 
  ggsave(paste(dir, "\\ggplot_CS_", CaseStudy, "_to_node_", myPath[length(myPath)], ".png", sep = ""), plot = last_plot())
  
}



# # # # #GGPLOT 3 

for (myNode in c(firstNode, secondNode)) {

  myPath <- c()
  for (i in 1:nchar(myNode)) {myPath[i] = substr(myNode, 1, i)}
  
  JuDGE.subs6 <- JuDGE[JuDGE$node %in% myPath & JuDGE$variable == "y", ]
  
  tech.types <- colnames(costs)[3:ncol(costs)]
  for (i in c("CCS", "Geothermal", "HydroR", "HydroS", "Battery", "Wind")) {tech.types[startsWith(tech.types, i)] <- i}
  JuDGE.subs6 <- cbind(tech.types, JuDGE.subs6)
  
  temp <- unlist(strsplit(as.character(JuDGE.subs6$index), ","))
  JuDGE.subs6$tech <- as.numeric(gsub("\\[", "", temp[seq(1, length(temp), 3)]))
  JuDGE.subs6$histYears <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 3)+2]))
  
  if (shutdown) {
    shut <- JuDGE[JuDGE$node %in% myPath & JuDGE$variable %in% "y_shut", ]
    shut$value <- round(shut$value * (shut$value > 0.999))
    tech.types <- tech.types[c(1, 2)]
    shut <- cbind(tech.types, shut)
    temp <- unlist(strsplit(as.character(shut$index), ","))
    shut$tech <- as.numeric(gsub("\\[", "", temp[seq(1, length(temp), 3)]))
    shut$histYears <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 3)+2]))
    
    JuDGE.subs6 <- rbind(JuDGE.subs6, shut)
  }
  
  JuDGE.subs6 <- subset(JuDGE.subs6, nchar(node) == 3)
  JuDGE.subs6_new <- JuDGE.subs6 %>% group_by(histYears, tech.types) %>% summarise(totalGWh = sum(value)/1000)
  JuDGE.subs6_new$tech_ind <- rep(seq(1, 10), 13)
  
  # Add in solar
  tree.struct6 <- tree.struct[tree.struct$n %in% myNode, c(1, 4)]
  zz <- as.data.frame(cbind(as.integer(tree.struct6$n), "Solar", 0, 0))
  colnames(zz) <- colnames(JuDGE.subs6_new)
  zz$totalGWh <- as.numeric(tree.struct6$solar_MW) * solar_GWh_scaling
  zz$tech_ind <- as.integer(zz$tech_ind)
  zz <- cbind(zz, c(1:13))
  zz <- zz[ , c(2, 3, 4, 5)]
  colnames(zz)[4] <- "histYears"
  
  JuDGE.subs6_new <- rbind(as.data.frame(JuDGE.subs6_new), zz[, c(4, 1, 2, 3)])
  
  # Wind
  capacity <- JuDGE[JuDGE$node %in% myPath & JuDGE$variable == "x", ]
  capacity$tech <- as.numeric(gsub("\\[|\\]", "", capacity$index))
  capacity <- subset(capacity, tech >= max(capacity$tech) - 4)
  wind.data <- read.csv("C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\Data_Visualisation\\windfactordata.csv", header = TRUE)
  wind.data <- wind.data[, c(4, 6, 8, 9, 10)]
  wind.scale <- wind.data%>%gather() %>% group_by(key) %>% summarise(windFact = sum(value))
  capacity <- cbind(capacity, wind.scale)
  
  temp <- as.data.frame(unname(costs[4, (max(capacity$tech) - 2):ncol(costs)]))
  capacity$Existing <- rep(t(as.data.frame(temp)), length(unique(capacity$node)))
  temp <- as.data.frame(unname(costs[5, (max(capacity$tech) - 2):ncol(costs)]))
  capacity$Investment <- rep(t(as.data.frame(temp)), length(unique(capacity$node)))
  
  capacity$Investment <- ifelse(capacity$variable == "x", capacity$value * capacity$Investment, 0)
  capacity$Total <- capacity$Investment + capacity$Existing
  
  capacity <- capacity %>% group_by(node) %>% summarise(totalGWh = sum(Total * windFact)/1000)
  capacity <- subset(capacity, nchar(node) == 3)
  
  capacity <- cbind(c(1:13), "Wind", capacity)
  colnames(capacity)[1] <- "histYears"
  colnames(capacity)[2] <- "tech.types"
  capacity$tech_ind <- 10
  JuDGE.subs6_new <- rbind(JuDGE.subs6_new, capacity[, c(1, 2, 4, 5)])
  
  JuDGE.subs6_new <- JuDGE.subs6_new %>% filter(tech.types != "Battery")
  
  MWlabels <- JuDGE.subs6_new %>% group_by(histYears) %>% summarise(tot = sum(totalGWh))
  # JuDGE.subs6_new$totalGWh <- ifelse(JuDGE.subs6_new$totalGWh == 0, NA, JuDGE.subs6_new$totalGWh)
  colnames(JuDGE.subs6_new)[2] <- "Generation_Tech"
  JuDGE.subs6_new$totalGWh <- ifelse(JuDGE.subs6_new$totalGWh == 0, 0.00001, JuDGE.subs6_new$totalGWh)
  
  mytitle = c()
  if (CaseStudy == "1") {
    mytitle = "No emissions constraints imposed"
  } else if (CaseStudy == "2") {
    mytitle = "100% renewable by 2030, Sixth Labour Government policy"
  } else if (CaseStudy == "3"){
    mytitle = "100% renewable in a normal hydrological year by 2035, prior Sixth Labour Government policy"
  }
  
  JuDGE.subs6_new$histYears <- JuDGE.subs6_new$histYears + 2004
  
  ggplot() +
    geom_col(data = JuDGE.subs6_new, aes(x = as.factor(histYears), y = totalGWh, fill = Generation_Tech), width = 0.4,color="white", size = 0.5) +
    # geom_text(data = MWlabels, aes(x = as.factor(histYears), y = tot, label=paste(round(tot), "GWh")), position=position_dodge(width=0.9), vjust=-0.7, size = 3.5) +
    xlab("Historical inflows from year") + ylab("Total Generation (GWh)") + ylim(0, 100000) +
    ggtitle(paste("Electric Power Annual GWh Generation for the years 2040 - 2045\n", mytitle, "\nTotal generation: ", round(MWlabels[1, 2]), " GWh", sep = "")) +
    theme(text = element_text(size = 20)) 
  ggsave(paste(dir, "\\ggplot_CS_", CaseStudy, "_histyears_node_", myNode, ".png", sep = ""), plot = last_plot())
  
}





# CO2 <- unname(costs[6, 3:ncol(costs)])
# JuDGE.subs4 <- JuDGE[JuDGE$node %in% firstNode & JuDGE$variable == "y", ]
# JuDGE.subs4 <- cbind(tech.types, t(CO2), JuDGE.subs4)






















# # # # # GGPLOT 4

# myNode = firstNode
myNode = secondNode
# myNode = 111
JuDGE.subs <- JuDGE[JuDGE$node %in% myNode & JuDGE$variable %in% c("Charge", "Discharge") , ]
temp <- unlist(strsplit(as.character(JuDGE.subs$index), ","))
JuDGE.subs$blocks <- as.numeric(gsub("\\[", "", temp[seq(1, length(temp), 3)]))
JuDGE.subs$histyear <- as.numeric(gsub("\\[", "", temp[seq(1, length(temp), 3)+1]))
JuDGE.subs$battery <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 3)+2]))
# JuDGE.subs$block_sum <- JuDGE.subs$blocks - 10 * floor((JuDGE.subs$blocks-1)/10)
# JuDGE.subs <- JuDGE.subs %>% filter(histyear == histYear) %>% group_by(variable, blocks) %>% summarise(total = sum(value))
JuDGE.subs <- JuDGE.subs %>% filter(blocks >= 31) %>% filter(histyear == 11) %>% group_by(variable, blocks) %>% summarise(total = sum(value))



LDC <- JuDGE[JuDGE$node %in% myNode & JuDGE$variable %in% c("SelectedLDC") , ]
temp <- unlist(strsplit(as.character(LDC$index), ","))
LDC$blocks <- as.numeric(gsub("\\[", "", temp[seq(1, length(temp), 2)]))
LDC$histyear <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 2)+1]))
# LDC$block_sum <- LDC$blocks - 10 * floor((LDC$blocks-1)/10)

LDC <- LDC %>% filter(histyear == histYear) %>% group_by(variable, blocks) %>% summarise(MWh = sum(value))

Matrix = read.csv("C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\JuDGE_model_development\\Generated_LDCs\\1\\total_hours.csv", header = FALSE)

lambda <- JuDGE[JuDGE$node %in% myNode & JuDGE$variable %in% c("lambda") , ]
temp <- unlist(strsplit(as.character(lambda$index), ","))
lambda$ind <- as.numeric(gsub("\\[|\\]", "", temp[seq(1, length(temp), 1)]))
lambda <- lambda %>% filter(value == 1)
hours <- Matrix[lambda[1, "ind"], ]

LDC$hours <- t(unname(as.matrix(hours)))[, 1]

LDC <- LDC %>% filter(blocks >= 31)

LDC$charge <- unname(as.data.frame(JuDGE.subs) %>% filter(variable == "Charge"))[, 3]
LDC$discharge <- unname(as.data.frame(JuDGE.subs) %>% filter(variable == "Discharge"))[, 3]

# LDC$MW <- LDC$MWh / LDC$hours
# LDC$charge <- LDC$charge / LDC$hours
# LDC$discharge <- LDC$discharge / LDC$hours
# LDC$discharge <- LDC$discharge * 100

LDC$blocks <- LDC$blocks - 30 
# LDC$MW_original <- LDC$MW - LDC$charge + LDC$discharge
LDC$season <- 1 + floor((LDC$blocks-1)/10)



LDC %>% 
  # filter(blocks %in% c(11:20)) %>%
  ggplot() +
    geom_col(aes(x = as.factor(blocks), y = charge, fill = "Charge"), alpha = 0.9) +
    geom_col(aes(x = as.factor(blocks), y = discharge, fill = "Discharge"), alpha = 0.9) + # facet_wrap(~season, nrow = 2) +
    xlab("Block number") + ylab("Total battery utilisation (MWh)") +
    ggtitle("Battery charge and discharge quantities in each load block\nSpring, node 12422")














# # # # # # # # GGPLOT 5: Emissions
for (myNode in c(firstNode, secondNode)) {
  
  myPath <- c()
  for (i in 1:nchar(myNode)) {myPath[i] = substr(myNode, 1, i)}
  
  JuDGE.subs6 <- JuDGE[JuDGE$node %in% myPath & JuDGE$variable == "y", ]
  tech.types <- colnames(costs)[3:ncol(costs)]
  for (i in c("CCS", "Geothermal", "HydroR", "HydroS", "Battery", "Wind")) {tech.types[startsWith(tech.types, i)] <- i}
  JuDGE.subs6 <- cbind(tech.types, JuDGE.subs6)
  JuDGE.subs6 <- cbind(JuDGE.subs6, t(unname(costs[6, 3:ncol(costs)])))
  temp <- unlist(strsplit(as.character(JuDGE.subs6$index), ","))
  JuDGE.subs6$tech <- as.numeric(gsub("\\[", "", temp[seq(1, length(temp), 3)]))
  JuDGE.subs6$histYears <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 3)+2]))
  
  if (shutdown) {
    shut <- JuDGE[JuDGE$node %in% myPath & JuDGE$variable %in% "y_shut", ]
    shut$value <- round(shut$value * (shut$value > 0.999))
    tech.types <- tech.types[c(1, 2)]
    shut <- cbind(tech.types, shut)
    shut <- cbind(shut, t(unname(costs[6, 3:4])))
    
    
    temp <- unlist(strsplit(as.character(shut$index), ","))
    shut$tech <- as.numeric(gsub("\\[", "", temp[seq(1, length(temp), 3)]))
    shut$histYears <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 3)+2]))
    JuDGE.subs6 <- rbind(JuDGE.subs6, shut)
  }
  colnames(JuDGE.subs6)[6] <- "emissions"
  JuDGE.subs6 <- subset(JuDGE.subs6, histYears == histYear)
  
  JuDGE.subs6_new <- JuDGE.subs6 %>% group_by(node, tech.types) %>% summarise(kTonneCO2 = sum(value * emissions)/1000)
  JuDGE.subs6_new$tech_ind <- rep(seq(1, 10), length(unique(JuDGE.subs6_new$node)))
  
  # Add in solar
  tree.struct6 <- tree.struct[tree.struct$n %in% myPath, c(1, 4)]
  zz <- as.data.frame(cbind(as.integer(tree.struct6$n), "Solar", 0, 0))
  colnames(zz) <- colnames(JuDGE.subs6_new)
  zz$kTonneCO2 <- as.numeric(tree.struct6$solar_MW) * solar_GWh_scaling * 0
  zz$tech_ind <- as.integer(zz$tech_ind)
  JuDGE.subs6_new <- rbind(as.data.frame(JuDGE.subs6_new), zz)
  
  # Wind
  capacity <- JuDGE[JuDGE$node %in% myPath & JuDGE$variable == "x", ]
  capacity$tech <- as.numeric(gsub("\\[|\\]", "", capacity$index))
  capacity <- subset(capacity, tech >= max(capacity$tech) - 4)
  wind.data <- read.csv("C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\Data_Visualisation\\windfactordata.csv", header = TRUE)
  wind.data <- wind.data[, c(4, 6, 8, 9, 10)]
  wind.scale <- wind.data%>%gather() %>% group_by(key) %>% summarise(windFact = sum(value))
  capacity <- cbind(capacity, wind.scale)
  temp <- as.data.frame(unname(costs[4, (max(capacity$tech) - 2):ncol(costs)]))
  capacity$Existing <- rep(t(as.data.frame(temp)), length(unique(capacity$node)))
  temp <- as.data.frame(unname(costs[5, (max(capacity$tech) - 2):ncol(costs)]))
  capacity$Investment <- rep(t(as.data.frame(temp)), length(unique(capacity$node)))
  capacity$Investment <- ifelse(capacity$variable == "x", capacity$value * capacity$Investment, 0)
  capacity$Total <- capacity$Investment + capacity$Existing
  capacity <- capacity %>% group_by(node) %>% summarise(kTonneCO2 = sum(0 * Total * windFact)/1000)
  capacity <- cbind("Wind", capacity)
  capacity$tech_ind <- 10
  colnames(capacity)[1] <- "tech.types"
  JuDGE.subs6_new <- rbind(JuDGE.subs6_new, capacity[, c(2, 1, 3, 4)])
  
  JuDGE.subs6_new <- JuDGE.subs6_new %>% filter(tech.types != "Battery")
  
  MWlabels <- JuDGE.subs6_new %>% group_by(node) %>% summarise(tot = sum(kTonneCO2))
  # JuDGE.subs6_new$totalGWh <- ifelse(JuDGE.subs6_new$totalGWh == 0, NA, JuDGE.subs6_new$totalGWh)
  colnames(JuDGE.subs6_new)[2] <- "Generation_Tech"
  JuDGE.subs6_new$kTonneCO2 <- ifelse(JuDGE.subs6_new$kTonneCO2 == 0, 0.000001, JuDGE.subs6_new$kTonneCO2)
  
  mytitle = c()
  if (CaseStudy == "1") {
    mytitle = "No emissions constraints imposed"
  } else if (CaseStudy == "2") {
    mytitle = "100% renewable by 2030, Sixth Labour Government policy"
  } else if (CaseStudy == "3"){
    mytitle = "100% renewable in a normal hydrological year by 2035, prior Sixth Labour Government policy"
  }
  
  ggplot() +
    geom_col(data = JuDGE.subs6_new, aes(x = as.factor(node), y = kTonneCO2, fill = Generation_Tech), width = 0.4,color="white", size = 0.5) +
    geom_text(data = MWlabels, aes(x = as.factor(node), y = tot, label=paste(round(tot), "kT")), position=position_dodge(width=0.9), vjust=-0.7, size = 3.5) +
    xlab("Node in Stage") + ylab("Total Emissions (kT)") + ylim(0, 5000) +
    ggtitle(paste("Generation stack annual emissions for the years ",2020 + 5*(nchar(myPath[1])-1), " - ", 2020 + 5*nchar(myPath[length(myPath)]), "\n", mytitle, sep = "")) +
    theme(text = element_text(size = 20)) 
  ggsave(paste(dir, "\\ggplot_CS_", CaseStudy, "_emissions_to_node_", myPath[length(myPath)], ".png", sep = ""), plot = last_plot())
  
}



