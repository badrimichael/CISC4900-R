# Project Title: Simulations of Reinforcement Learning in an Ecological Task
# Author: Michael Badri, Brooklyn College Department of Computer and Information Science
# Supervisor: Dr. Stefano Ghirlanda, Brooklyn College Department of Psychology

# The goal of this project is to simulate the ecological task of behavior chaining using reinforcement learning
# algorithms.

# This R script takes the output files from the corresponding Python program 
# (https://github.com/badrimichael/CISC4900) and generates a histogram for each learning algorithm
# for each output file. Upon completion, output will be 8 (Eight) histograms.
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


# Plot histograms.
qplot(q_learning,
      main = paste(reward_value,"total reward:", agents_per_list, "Q-Learning Agents", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)

qplot(sarsa,
      main = paste(reward_value,"total reward:", agents_per_list, "SARSA Agents", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)

qplot(expected_sarsa,
      main = paste(reward_value,"total reward:", agents_per_list, "Expected-SARSA Agents", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)

qplot(qv_learning,
      main = paste(reward_value,"total reward:", agents_per_list, "QV-Learning Agents", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)
