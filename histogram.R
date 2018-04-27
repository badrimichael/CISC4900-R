library(ggplot2)

output_0 = read.csv(file = 'sampleoutput/output500-0percent.csv', head = TRUE, sep = ',')
output_10 = read.csv(file = 'sampleoutput/output500-10percent.csv', head = TRUE, sep = ',')
reward_value = 25000

for (agent_type in levels(output_0$Agent.Type)) {
  write.csv(subset(output_0, Agent.Type == agent_type),
            paste(agent_type,'0', sep=" "),
            row.names = FALSE)
}

for (agent_type in levels(output_10$Agent.Type)) {
  write.csv(subset(output_10, Agent.Type == agent_type),
            paste(agent_type,'10', sep=" "),
            row.names = FALSE)
}


q_agents_0 = read.csv(file = 'Q-learning 0', head = TRUE, sep = ',')
sarsa_agents_0 = read.csv(file = 'SARSA 0', head = TRUE, sep = ',')
expected_sarsa_agents_0 = read.csv(file = 'Expected SARSA 0', head = TRUE, sep =',')
qv_agents_0 = read.csv(file = 'QV-learning 0', head = TRUE, sep = ',')

q_agents_10 = read.csv(file = 'Q-learning 10', head = TRUE, sep = ',')
sarsa_agents_10 = read.csv(file = 'SARSA 10', head = TRUE, sep = ',')
expected_sarsa_agents_10 = read.csv(file = 'Expected SARSA 10', head = TRUE, sep =',')
qv_agents_10 = read.csv(file = 'QV-learning 10', head = TRUE, sep = ',')


agents_per_list = max(q_agents_0$Agent)
total_num_of_agents = max(qv_agents_0$Agent)


i = 1
q_learning_0 <- c()
for (i in 1:agents_per_list) {
  sub = q_agents_0[q_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  q_learning_0 = c(q_learning_0, besttime)
}
qplot(q_learning_0,
      main = paste(reward_value,"total reward:", agents_per_list, "Q-Learning Agents (0 percent)", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)


sarsa_0 <- c()
for (i in (agents_per_list + 1):(2 * agents_per_list)) {
  sub = sarsa_agents_0[sarsa_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  sarsa_0 = c(sarsa_0, besttime)
}
qplot(sarsa_0,
      main = paste(reward_value,"total reward:", agents_per_list, "SARSA Agents (0 percent)", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)




expected_sarsa_0 <- c()
for (i in (2*agents_per_list+1):(total_num_of_agents-agents_per_list)) {
  sub = expected_sarsa_agents_0[expected_sarsa_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  expected_sarsa_0 = c(expected_sarsa_0, besttime)
}
qplot(expected_sarsa_0,
      main = paste(reward_value,"total reward:", agents_per_list, "Expected-SARSA Agents (0 percent)", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)



qv_learning_0 <- c()
for (i in (total_num_of_agents-agents_per_list+1):total_num_of_agents) {
  sub = qv_agents_0[qv_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  qv_learning_0 = c(qv_learning_0, besttime)
}


qplot(qv_learning_0,
      main = paste(reward_value,"total reward:", agents_per_list, "QV-Learning Agents (0 percent)", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)

i = 1
q_learning_10 <- c()
for (i in 1:agents_per_list) {
  sub = q_agents_10[q_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  q_learning_10 = c(q_learning_10, besttime)
}
qplot(q_learning_10,
      main = paste(reward_value,"total reward:", agents_per_list, "Q-Learning Agents (10 percent)", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)



sarsa_10 <- c()
for (i in (agents_per_list + 1):(2 * agents_per_list))  {
  sub = sarsa_agents_10[sarsa_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  sarsa_10 = c(sarsa_10, besttime)
}
qplot(sarsa_10,
      main = paste(reward_value,"total reward:", agents_per_list, "SARSA Agents (10 percent)", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)




expected_sarsa_10 <- c()
for (i in (2*agents_per_list+1):(total_num_of_agents-agents_per_list)) {
  sub = expected_sarsa_agents_10[expected_sarsa_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  expected_sarsa_10 = c(expected_sarsa_10, besttime)
}
qplot(expected_sarsa_10,
      main = paste(reward_value,"total reward:", agents_per_list, "Expected-SARSA Agents (10 percent)", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)



qv_learning_10 <- c()
for (i in (total_num_of_agents-agents_per_list+1):total_num_of_agents) {
  sub = qv_agents_10[qv_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  qv_learning_10 = c(qv_learning_10, besttime)
}
qplot(qv_learning_10,
      main = paste(reward_value,"total reward:", agents_per_list, "QV-Learning Agents (10 percent)", sep = " "),
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"),
      bins = 20)