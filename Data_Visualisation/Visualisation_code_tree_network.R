require(igraph)
require(ggplot2)
require(plyr)
require(tidyverse)

# User input
# Assumes firstNode is a valid node
# Assumes both nodes are in the same stage, i.e. have the same length 
firstNode  = "111"        # String
secondNode = "122"        # String
histYear = 5

showPaths = TRUE
sharedPaths = TRUE

vis.dir <- "C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\Data_Visualisation"
data.dir <- "C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\JuDGE_model_development"

# Process the dataframe   
setwd(vis.dir)
df <- read.csv(paste(vis.dir,"\\node_adj_matrix.csv", sep = ""), header = TRUE)
rownames(df) <- df[, 1]
colnames(df) <- sub("X", "", colnames(df))
df <- df[, -1]

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


# Graph adjacency for igraph formatting
g <- graph.adjacency(data.matrix(df), weighted=T, mode = "directed")

# Tree layout
la <- layout_as_tree(g, root = 1, flip.y = FALSE)

# Labels and colouring code
V(g)$label <- colnames(df)
V(g)$name <- colnames(df)

if (sharedPaths) {
  V(g)$color <- ifelse(V(g)$label %in% firstPath, "orange", ifelse(V(g)$label %in% secondPath, "green", "black"))
  V(g)[V(g)$label == firstPath[firstPath == secondPath]]$color <- "blue"
} else{
  V(g)$color <- ifelse(V(g)$label %in% firstPath, "orange", "black")
}
V(g)$label <- ""

# Plotting functionality
plot(g, main = "45 Node Demand Growth Tree, 2019 Forecast"
     , vertex.size = 6, edge.arrow.size = 0.5
     , edge.width = 1, edge.arrow.width = 1, layout = la)

# tk_rotate(zz, degree = 90)


# # # # # DATA
# Read in JuDGE output for a given node
# tree.struct <- read.csv(paste(vis.dir,"\\341node-tree.csv", sep = ""), header = TRUE)
tree.struct <- read.csv(paste(data.dir,"\\45node-tree4.csv", sep = ""), header = TRUE)
# JuDGE <- read.csv(paste(data.dir,"\\JuDGE_expansions.csv", sep = ""), header = TRUE)
JuDGE <- read.csv(paste(data.dir,"\\JuDGE_expansions_45_node_tree_3.csv", sep = ""), header = TRUE)
costs <- read.csv(paste(data.dir,"\\Costs_and_parameters.csv", sep = ""), header = TRUE, fileEncoding="UTF-8-BOM")
tech.types <- colnames(costs)[3:ncol(costs)]


# Calculate solar GWh generation
mydir="C:\\Users\\calro\\Documents\\GitHub\\Update_JuDGE\\JuDGE_model_development\\TP_Portfolio_CSVs"
myfiles = list.files(path=mydir
                     , pattern="*_00000_*"
                     , full.names=TRUE)

# Read data into a single file
sol_csv <- as_tibble(ldply(myfiles, read_csv))
solar_GWh_scaling <- sum(sol_csv$Solar) * 0.5 / 1000

# # # # # GGPLOT 1   

# Subset accordingly
JuDGE.subs <- JuDGE[JuDGE$node %in% firstNode & JuDGE$variable == "x", ]
JuDGE.subs <- cbind(tech.types, JuDGE.subs)
for (i in c("U", "u")) {
  costs.sub <- unname(costs[costs$ParamName == i, 3:ncol(costs)])
  JuDGE.subs <- cbind(JuDGE.subs, t(costs.sub))
}

# Add cols to the dataframe
colnames(JuDGE.subs)[6:7] <- c("Existing_Capacity", "Invested_Capacity")
JuDGE.subs$Invested_Capacity <- JuDGE.subs$Invested_Capacity * JuDGE.subs$value

# Collate wind
JuDGE.subs[JuDGE.subs$tech.types == "Wind", "Existing_Capacity"] = sum(startsWith(tech.types, "Wind") * JuDGE.subs$Existing_Capacity)
JuDGE.subs[JuDGE.subs$tech.types == "Wind", "Invested_Capacity"] = sum(startsWith(tech.types, "Wind") * JuDGE.subs$Invested_Capacity)
JuDGE.subs$index <- as.numeric(gsub("\\[|\\]", "", unlist(strsplit(as.character(JuDGE.subs$index), ","))))
JuDGE.subs <- JuDGE.subs[JuDGE.subs$index <= 10, ]

# Add in solar
tree.struct1 <- tree.struct[tree.struct$n %in% firstNode, c(1, 4)]
zz <- as.data.frame(cbind("Solar", tree.struct1$n, NA, NA, NA, 0, as.numeric(tree.struct1$solar_MW)))
colnames(zz) <- colnames(JuDGE.subs)
JuDGE.subs <- rbind(JuDGE.subs, zz)


# Stack the dataframe
dat_long <- JuDGE.subs[, c(1, 6, 7)] %>%
  gather("Stat", "Value", -tech.types)
dat_long$Stat <- relevel(as.factor(dat_long$Stat), 2)

# For adding the text label
JuDGE.subs$total <- as.numeric(JuDGE.subs$Existing_Capacity) + as.numeric(JuDGE.subs$Invested_Capacity)
# cat(JuDGE.subs$total, rep(NA, length(JuDGE.subs$total)))
dat_long <- cbind(dat_long, c(JuDGE.subs$total, rep(NA, length(JuDGE.subs$total))))
colnames(dat_long)[4] <- "Total"
dat_long$Total <- ifelse(dat_long$Total == 0, NA,dat_long$Total)

# Graphs of investments at a certain node
ggplot(dat_long) +
  geom_bar(aes(x = tech.types, y = as.numeric(Value), fill = Stat, alpha = Stat), position="stack", stat = "identity",color="white", size = 1) + 
  xlab("Generation Technology Type") + ylab("Total Capacity (MW)")+ guides(alpha=FALSE) +
  geom_text(aes(x = tech.types, y = Total, label=paste(round(Total), "MW")), position=position_dodge(width=0.9), vjust=-0.5, size = 3.5) + 
  ggtitle(paste("Electric Power Generation Stack Capacity at node", firstNode, "for the years",2020 + 5*(nchar(firstNode)-1), "-", 2020 + 5*nchar(firstNode))) +
  scale_alpha_manual(values = c(0.8, 1)) + 
  scale_fill_manual("Legend", values = c("Invested_Capacity" = "orange", "Existing_Capacity" = "black")) + 
  theme(plot.title = element_text(hjust = 0.5))

# # # # GGPLOT 2   

JuDGE.subs2 <- JuDGE[JuDGE$node %in% firstPath & JuDGE$variable == "x", ]
# JuDGE.subs2$index <- as.numeric(gsub("\\[|\\]", "", JuDGE.subs2$index))
# JuDGE.subs2 <- JuDGE.subs2 %>% arrange(node, index)
JuDGE.subs2 <- cbind(tech.types, JuDGE.subs2)
                                
for (i in c("U", "u")) {
  costs.sub <- unname(costs[costs$ParamName == i, 3:ncol(costs)])
  JuDGE.subs2 <- cbind(JuDGE.subs2, t(costs.sub))
}
colnames(JuDGE.subs2)[6:7] <- c("Existing_Capacity", "Invested_Capacity")
JuDGE.subs2$Invested_Capacity <- JuDGE.subs2$Invested_Capacity * JuDGE.subs2$value
JuDGE.subs2$total <- JuDGE.subs2$Existing_Capacity + JuDGE.subs2$Invested_Capacity
JuDGE.subs2$node <- as.factor(JuDGE.subs2$node)

for (n in unique(JuDGE.subs2$node)) {
  
  # Collate wind
  JuDGE.subs2[JuDGE.subs2$tech.types == "Wind" & JuDGE.subs2$node == n, "Existing_Capacity"] = sum(startsWith(tech.types, "Wind") * JuDGE.subs2[JuDGE.subs2$node == n, "Existing_Capacity"])
  JuDGE.subs2[JuDGE.subs2$tech.types == "Wind" & JuDGE.subs2$node == n, "Invested_Capacity"] = sum(startsWith(tech.types, "Wind") * JuDGE.subs2[JuDGE.subs2$node == n, "Invested_Capacity"])
}

temp <- unlist(strsplit(as.character(JuDGE.subs2$index), ","))
JuDGE.subs2$index <- as.numeric(gsub("\\[|\\]", "", temp[seq(1, length(temp), 1)]))
JuDGE.subs2 <- JuDGE.subs2[JuDGE.subs2$index <= 10, ]

# Add in solar
tree.struct2 <- tree.struct[tree.struct$n %in% firstPath, c(1, 4)]
zz <- as.data.frame(cbind("Solar", tree.struct2$n, NA, NA, NA, NA, NA, as.numeric(tree.struct2$solar_MW)))
colnames(zz) <- colnames(JuDGE.subs2)
JuDGE.subs2 <- rbind(JuDGE.subs2, zz)

MWlabels <- JuDGE.subs2 %>% group_by(node) %>% summarise(tot = sum(as.numeric(total)))

ggplot() +
  geom_col(data = JuDGE.subs2, aes(x = node, y = as.numeric(total), fill = tech.types), width = 0.4,color="white", size = 0.5) +
  geom_text(data = MWlabels, aes(x = node, y = tot, label=paste(tot, "MW")), position=position_dodge(width=0.9), vjust=-0.7, size = 3.5) +
  xlab("Node in Trajectory") + ylab("Total Generation Stack Capacity (MW)") +
  ggtitle(paste("Electric Power Generation Stack Capacity along the trajectory from node", firstPath[1], "to node", firstPath[length(firstPath)], "\nRanging over the years",2020 + 5*(nchar(firstPath[1])-1), "-", 2020 + 5*nchar(firstPath[length(firstPath)])))


# # # # GGPLOT 3   
  
# Subset accordingly
JuDGE.subs3 <- JuDGE[JuDGE$node %in% firstNode & JuDGE$variable == "y", ]
JuDGE.subs3 <- cbind(tech.types, JuDGE.subs3)
temp <- unlist(strsplit(as.character(JuDGE.subs3$index), ","))
JuDGE.subs3$histYears <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 3)+2]))
JuDGE.subs3 <- subset(JuDGE.subs3, histYears == histYear)
JuDGE.subs3 <- JuDGE.subs3 %>% group_by(tech.types) %>% summarise(totalGWh = sum(value)/1000)
JuDGE.subs3[JuDGE.subs3$tech.types == "Wind", "totalGWh"] = sum(JuDGE.subs3[startsWith(as.character(JuDGE.subs3$tech.types), "Wind"), "totalGWh"])
JuDGE.subs3$totalGWh <- ifelse(JuDGE.subs3$totalGWh == 0, NA, JuDGE.subs3$totalGWh)
JuDGE.subs3 <- JuDGE.subs3[1:10, ]



tree.struct3 <- tree.struct[tree.struct$n %in% firstNode, c(1, 4)]
zz <- as.data.frame(cbind("Solar", as.numeric(tree.struct3$solar_MW) * solar_GWh_scaling))
colnames(zz) <- colnames(JuDGE.subs3)


JuDGE.subs3 <- rbind(JuDGE.subs3, zz)





ggplot() + 
  geom_col(data = JuDGE.subs3, aes(x = tech.types, y = as.numeric(totalGWh), fill = tech.types), width = 1, color="white", size = 1, show.legend = FALSE) +
  geom_text(data = JuDGE.subs3, aes(x = tech.types, y = as.numeric(totalGWh), label=paste(round(as.numeric(totalGWh)), "GWh")), position=position_dodge(width=0.9), vjust=-0.7, size = 3.5) +
  xlab("Generation Technology Type") + ylab("Total Generation (GWh)") + 
  ggtitle(paste("Electric Power annual GWh Generation at node", firstNode, "for the years",2020 + 5*(nchar(firstNode)-1), "-", 2020 + 5*nchar(firstNode), "\nWith historical year inflows from", 2005+(histYear-1)))


# # # # GGPLOT 4   

CO2 <- unname(costs[costs$ParamName == "em", 3:ncol(costs)])
JuDGE.subs4 <- JuDGE[JuDGE$node %in% firstNode & JuDGE$variable == "y", ]
JuDGE.subs4 <- cbind(tech.types, t(CO2), JuDGE.subs4)
colnames(JuDGE.subs4)[2] <- "emissions"

temp <- unlist(strsplit(as.character(JuDGE.subs4$index), ","))
JuDGE.subs4$histYears <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 3)+2]))
JuDGE.subs4 <- subset(JuDGE.subs4, histYears == histYear)
JuDGE.subs4 <- JuDGE.subs4 %>% group_by(tech.types, emissions) %>% summarise(totalMWh = sum(value))
JuDGE.subs4$totalEmissions <- (JuDGE.subs4$totalMWh * JuDGE.subs4$emissions)/1e3

JuDGE.subs4[JuDGE.subs4$tech.types == "Wind", "totalEmissions"] = sum(JuDGE.subs4[startsWith(as.character(JuDGE.subs4$tech.types), "Wind"), "totalEmissions"])
JuDGE.subs4$tech.types <- as.character(JuDGE.subs4$tech.types)

JuDGE.subs4 <- JuDGE.subs4[1:10, ]

# Add solar as a column
JuDGE.subs4[11, "tech.types"] = "Solar"
JuDGE.subs4[JuDGE.subs4$tech.types == "Solar", "totalEmissions"] <- 0

JuDGE.subs4$totalMWh <- ifelse(JuDGE.subs4$totalMWh == 0, NA, JuDGE.subs4$totalMWh)
JuDGE.subs4$totalEmissions <- ifelse(JuDGE.subs4$totalEmissions == 0, NA, JuDGE.subs4$totalEmissions)


ggplot() + 
  geom_col(data = JuDGE.subs4, aes(x = tech.types, y = totalEmissions, fill = tech.types), width = 1, color="white", size = 1, show.legend = FALSE) +
  geom_text(data = JuDGE.subs4, aes(x = tech.types, y = totalEmissions, label=paste(round(totalEmissions), "kt CO2")), position=position_dodge(width=0.9), vjust=-0.7, size = 3.5) +
  xlab("Generation Technology Type") + ylab("Total Emissions (kt CO2)") + 
  ggtitle(paste("Electric Power annual CO2 emissions at node", firstNode, "for the years",2020 + 5*(nchar(firstNode)-1), "-", 2020 + 5*nchar(firstNode), "\nWith historical year inflows from", 2005+(histYear-1)))


# # # # GGPLOT 5 (looks bad for late stage nodes)    

JuDGE.subs5 <- JuDGE[nchar(JuDGE$node) == nchar(firstNode) & JuDGE$variable == "x", ]
JuDGE.subs5 <- cbind(tech.types, JuDGE.subs5)

for (i in c("U", "u")) {
  costs.sub <- unname(costs[costs$ParamName == i, 3:ncol(costs)])
  JuDGE.subs5 <- cbind(JuDGE.subs5, t(costs.sub))
}
colnames(JuDGE.subs5)[6:7] <- c("Existing_Capacity", "Invested_Capacity")
JuDGE.subs5$Invested_Capacity <- JuDGE.subs5$Invested_Capacity * JuDGE.subs5$value
JuDGE.subs5$node <- as.factor(JuDGE.subs5$node)

for (n in unique(JuDGE.subs5$node)) {
  cat(n, "\n")
  # Collate wind
  JuDGE.subs5[JuDGE.subs5$tech.types == "Wind" & JuDGE.subs5$node == n, "Existing_Capacity"] = sum(startsWith(tech.types, "Wind") * JuDGE.subs5[JuDGE.subs5$node == n, "Existing_Capacity"])
  JuDGE.subs5[JuDGE.subs5$tech.types == "Wind" & JuDGE.subs5$node == n, "Invested_Capacity"] = sum(startsWith(tech.types, "Wind") * JuDGE.subs5[JuDGE.subs5$node == n, "Invested_Capacity"])
}

temp <- unlist(strsplit(as.character(JuDGE.subs5$index), ","))
JuDGE.subs5$index <- as.numeric(gsub("\\[|\\]", "", temp[seq(1, length(temp), 1)]))
JuDGE.subs5 <- JuDGE.subs5[JuDGE.subs5$index <= 10, ]
JuDGE.subs5$total <- JuDGE.subs5$Existing_Capacity + JuDGE.subs5$Invested_Capacity

# Add in solar
tree.struct5 <- tree.struct[nchar(tree.struct$n) == nchar(firstNode), c(1, 4)]
zz <- as.data.frame(cbind("Solar", tree.struct5$n, NA, NA, NA, NA, NA, as.numeric(tree.struct5$solar_MW)))
colnames(zz) <- colnames(JuDGE.subs5)
JuDGE.subs5 <- rbind(JuDGE.subs5, zz)


MWlabels <- JuDGE.subs5 %>% group_by(node) %>% summarise(tot = sum(as.numeric(total)))

ggplot() +
  geom_col(data = JuDGE.subs5, aes(x = node, y = as.numeric(total), fill = tech.types), width = 0.4,color="white", size = 0.5) +
  geom_text(data = MWlabels, aes(x = node, y = tot, label=paste(round(tot), "MW")), position=position_dodge(width=0.9), vjust=-0.7, size = 3.5) +
  xlab("Node in Stage") + ylab("Total Generation Stack Capacity (MW)") +
  ggtitle(paste("Electric Power Generation Stack Capacity across all nodes for the years",2020 + 5*(nchar(firstNode)-1), "-", 2020 + 5*nchar(firstNode)))

# # # # GGPLOT 6 (looks bad for late stage nodes)

JuDGE.subs6 <- JuDGE[nchar(JuDGE$node) == nchar(firstNode) & JuDGE$variable == "y", ]
JuDGE.subs6 <- cbind(tech.types, JuDGE.subs6)
temp <- unlist(strsplit(as.character(JuDGE.subs6$index), ","))
JuDGE.subs6$histYears <- as.numeric(gsub("\\]", "", temp[seq(1, length(temp), 3)+2]))
# JuDGE.subs6$tech_ind <- as.numeric(gsub("\\[", "", temp[seq(1, length(temp), 3)]))
JuDGE.subs6 <- subset(JuDGE.subs6, histYears == histYear)
JuDGE.subs6_new <- JuDGE.subs6 %>% group_by(node, tech.types) %>% summarise(totalGWh = sum(value)/1000)
JuDGE.subs6_new$tech_ind <- rep(seq(1, 14), length(unique(JuDGE.subs6_new$node)))

for (n in unique(JuDGE.subs6_new$node)) {
  cat(n, "\n")
  # Collate wind
  JuDGE.subs6_new[JuDGE.subs6_new$tech.types == "Wind" & JuDGE.subs6_new$node == n, "totalGWh"] = sum(startsWith(tech.types, "Wind") * JuDGE.subs6_new[JuDGE.subs6_new$node == n, "totalGWh"])
}


JuDGE.subs6_new <- JuDGE.subs6_new[JuDGE.subs6_new$tech_ind <= 10, ]

# Add in solar
tree.struct6 <- tree.struct[nchar(tree.struct$n) == nchar(firstNode), c(1, 4)]
zz <- as.data.frame(cbind(as.integer(tree.struct6$n), "Solar", 0, 0))
colnames(zz) <- colnames(JuDGE.subs6_new)
zz$totalGWh <- as.numeric(tree.struct6$solar_MW) * solar_GWh_scaling
zz$tech_ind <- as.integer(zz$tech_ind)
JuDGE.subs6_new <- rbind(as.data.frame(JuDGE.subs6_new), zz)

MWlabels <- JuDGE.subs6_new %>% group_by(node) %>% summarise(tot = sum(totalGWh))
JuDGE.subs6_new$totalGWh <- ifelse(JuDGE.subs6_new$totalGWh == 0, NA, JuDGE.subs6_new$totalGWh)



ggplot() +
  geom_col(data = JuDGE.subs6_new, aes(x = as.factor(node), y = totalGWh, fill = tech.types), width = 0.4,color="white", size = 0.5) +
  geom_text(data = MWlabels, aes(x = as.factor(node), y = tot, label=paste(round(tot), "GWh")), position=position_dodge(width=0.9), vjust=-0.7, size = 3.5) +
  xlab("Node in Stage") + ylab("Total Generation (GWh)") +
  ggtitle(paste("Electric Power Generation across all nodes for the years",2020 + 5*(nchar(firstNode)-1), "-", 2020 + 5*nchar(firstNode)))



