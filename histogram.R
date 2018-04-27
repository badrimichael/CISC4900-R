library(ggplot2)
output_0 = read.csv(file = 'sampleoutput/output-0percent.csv', head = TRUE, sep = ',')
output_10 = read.csv(file = 'sampleoutput/output-10percent.csv', head = TRUE, sep = ',')

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


reward_value = 25000



i = 1
q_learning_0 <- c()
for (i in 1:200) {
  sub = q_agents_0[q_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  q_learning_0 = c(q_learning_0, besttime)
}
qplot(q_learning_0,
      main = "25000 total reward: 200 Q-Learning Agents (0 percent)",
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"))



sarsa_0 <- c()
for (i in 201:400) {
  sub = sarsa_agents_0[sarsa_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  sarsa_0 = c(sarsa_0, besttime)
}
qplot(sarsa_0,
      main = "25000 total reward: 200 SARSA Agents (0 percent)",
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"))




expected_sarsa_0 <- c()
for (i in 401:600) {
  sub = expected_sarsa_agents_0[expected_sarsa_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  expected_sarsa_0 = c(expected_sarsa_0, besttime)
}
qplot(expected_sarsa_0,
      main = "25000 total reward: 200 Expected-SARSA Agents (0 percent)",
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"))



qv_learning_0 <- c()
for (i in 601:800) {
  sub = qv_agents_0[qv_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  qv_learning_0 = c(qv_learning_0, besttime)
}


qplot(qv_learning_0,
      main = "25000 total reward: 200 QV-Learning Agents (0 percent)",
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"))

i = 1
q_learning_10 <- c()
for (i in 1:200) {
  sub = q_agents_10[q_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  q_learning_10 = c(q_learning_10, besttime)
}
qplot(q_learning_10,
      main = "25000 total reward: 200 Q-Learning Agents (10 percent)",
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"))



sarsa_10 <- c()
for (i in 201:400) {
  sub = sarsa_agents_10[sarsa_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  sarsa_10 = c(sarsa_10, besttime)
}
qplot(sarsa_10,
      main = "25000 total reward: 200 SARSA Agents (10 percent)",
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"))




expected_sarsa_10 <- c()
for (i in 401:600) {
  sub = expected_sarsa_agents_10[expected_sarsa_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  expected_sarsa_10 = c(expected_sarsa_10, besttime)
}
qplot(expected_sarsa_10,
      main = "25000 total reward: 200 Expected-SARSA Agents (10 percent)",
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"))



qv_learning_10 <- c()
for (i in 601:800) {
  sub = qv_agents_0[qv_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  qv_learning_10 = c(qv_learning_10, besttime)
}
qplot(qv_learning_10,
      main = "25000 total reward: 200 QV-Learning Agents (10 percent)",
      xlab = 'Time-step',
      ylab = 'Frequency',
      fill=I("white"),
      col=I("black"))


