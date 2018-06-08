# Project Title: Simulations of Reinforcement Learning in an Ecological Task
# Author: Michael Badri, Brooklyn College Department of Computer and Information Science
# Supervisor: Dr. Stefano Ghirlanda, Brooklyn College Department of Psychology

# The goal of this project is to simulate the ecological task of behavior chaining using reinforcement learning
# algorithms.

# This R script takes the output files from the corresponding Python program 
# (https://github.com/badrimichael/CISC4900) and generates a frequency polygon for each file.
# Upon script completion, output will be 2 (Two) frequency polygons.
# Note: THE FILES MUST BE RENAMED ACCORDINGLY.

# This script requires ggplot2 package.
library(ggplot2)

# Read the two output files into the environment and specify the reward value.
output = read.csv(file = 'output.csv', head = TRUE, sep = ',')
reward_value = 200

# Divide the output files into multiple dataframes, one per agent type.
for (agent_type in levels(output$Agent.Type)) {
  write.csv(subset(output, Agent.Type == agent_type),
            paste(agent_type),
            row.names = FALSE)
}


q_agents = read.csv(file = 'Q-learning', head = TRUE, sep = ',')
sarsa_agents = read.csv(file = 'SARSA', head = TRUE, sep = ',')
expected_sarsa_agents = read.csv(file = 'Expected SARSA', head = TRUE, sep =',')
qv_agents = read.csv(file = 'QV-learning', head = TRUE, sep = ',')

# Automatically determine the total number of agents and number of agents per dataframe.
agents_per_list = max(q_agents$Agent)
total_num_of_agents = max(qv_agents$Agent)

# Create a list of agents and the timestep they recieved reward_value.
i = 1
q_learning <- c()
for (i in 1:agents_per_list) {
  sub = q_agents[q_agents$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  q_learning = c(q_learning, besttime)
}

sarsa <- c()
for (i in (agents_per_list + 1):(2 * agents_per_list)) {
  sub = sarsa_agents[sarsa_agents$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  sarsa = c(sarsa, besttime)
}

expected_sarsa <- c()
for (i in (2*agents_per_list+1):(total_num_of_agents-agents_per_list)) {
  sub = expected_sarsa_agents[expected_sarsa_agents$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  expected_sarsa = c(expected_sarsa, besttime)
}

qv_learning <- c()
for (i in (total_num_of_agents-agents_per_list+1):total_num_of_agents) {
  sub = qv_agents[qv_agents$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  qv_learning = c(qv_learning, besttime)
}

# Combine the lists.
qplot <- c()
qplot$group[1:agents_per_list] <- "q-learning"
qplot$val <- q_learning
q<- cbind(qplot$group,qplot$val)

qvplot <- c()
qvplot$group[1:agents_per_list] <- "qv-learning"
qvplot$val <- qv_learning
qv<- cbind(qvplot$group,qvplot$val)

esarsaplot <- c()
esarsaplot$group[1:agents_per_list] <- "expected_sarsa"
esarsaplot$val <- expected_sarsa
sar1<-cbind(esarsaplot$group,esarsaplot$val)

sarsaplot <- c()
sarsaplot$group[1:agents_per_list] <- "sarsa"
sarsaplot$val <- sarsa
sar2<- cbind(sarsaplot$group,sarsaplot$val)

ggdf<-rbind(sar1,sar2,q,qv)
ggdf <- data.frame(ggdf)
colnames(ggdf) <- c("type","time")
ggdf$time<- as.numeric(as.character(ggdf$time))

# Plot lists on a frequency polygon.
ggplot(ggdf,aes(x=time,fill=as.factor(type), color=type)) +geom_freqpoly(bins=20) + theme_classic() + ggtitle(paste(reward_value,"total reward:", agents_per_list, "of Each Agent", sep = " "))


       