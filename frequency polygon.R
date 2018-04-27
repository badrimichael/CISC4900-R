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

sarsa_0 <- c()
for (i in (agents_per_list + 1):(2 * agents_per_list)) {
  sub = sarsa_agents_0[sarsa_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  sarsa_0 = c(sarsa_0, besttime)
}

expected_sarsa_0 <- c()
for (i in (2*agents_per_list+1):(total_num_of_agents-agents_per_list)) {
  sub = expected_sarsa_agents_0[expected_sarsa_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  expected_sarsa_0 = c(expected_sarsa_0, besttime)
}


qv_learning_0 <- c()
for (i in (total_num_of_agents-agents_per_list+1):total_num_of_agents) {
  sub = qv_agents_0[qv_agents_0$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  qv_learning_0 = c(qv_learning_0, besttime)
}

i = 1
q_learning_10 <- c()
for (i in 1:agents_per_list) {
  sub = q_agents_10[q_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  q_learning_10 = c(q_learning_10, besttime)
}

sarsa_10 <- c()
for (i in (agents_per_list + 1):(2 * agents_per_list)) {
  sub = sarsa_agents_10[sarsa_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  sarsa_10 = c(sarsa_10, besttime)
}

expected_sarsa_10 <- c()
for (i in (2*agents_per_list+1):(total_num_of_agents-agents_per_list)) {
  sub = expected_sarsa_agents_10[expected_sarsa_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  expected_sarsa_10 = c(expected_sarsa_10, besttime)
}

qv_learning_10 <- c()

for (i in (total_num_of_agents-agents_per_list+1):total_num_of_agents)  {
  sub = qv_agents_10[qv_agents_10$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  qv_learning_10 = c(qv_learning_10, besttime)
}

qplot_0 <- c()
qplot_0$group[1:agents_per_list] <- "q-learning"
qplot_0$val <- q_learning_0
q_0<- cbind(qplot_0$group,qplot_0$val)

qvplot_0 <- c()
qvplot_0$group[1:agents_per_list] <- "qv-learning"
qvplot_0$val <- qv_learning_0
qv_0<- cbind(qvplot_0$group,qvplot_0$val)

esarsaplot_0 <- c()
esarsaplot_0$group[1:agents_per_list] <- "expected_sarsa"
esarsaplot_0$val <- expected_sarsa_0
sar1_0<-cbind(esarsaplot_0$group,esarsaplot_0$val)

sarsaplot_0 <- c()
sarsaplot_0$group[1:agents_per_list] <- "sarsa"
sarsaplot_0$val <- sarsa_0
sar2_0<- cbind(sarsaplot_0$group,sarsaplot_0$val)


ggdf_0<-rbind(sar1_0,sar2_0,q_0,qv_0)
ggdf_0 <- data.frame(ggdf_0)
colnames(ggdf_0) <- c("type","time")
ggdf_0$time<- as.numeric(as.character(ggdf_0$time))


qplot_10 <- c()
qplot_10$group[1:agents_per_list] <- "q-learning"
qplot_10$val <- q_learning_10
q_10<- cbind(qplot_10$group,qplot_10$val)

qvplot_10 <- c()
qvplot_10$group[1:agents_per_list] <- "qv-learning"
qvplot_10$val <- qv_learning_10
qv_10<- cbind(qvplot_10$group,qvplot_10$val)

esarsaplot_10 <- c()
esarsaplot_10$group[1:agents_per_list] <- "expected_sarsa"
esarsaplot_10$val <- expected_sarsa_10
sar1_10<-cbind(esarsaplot_10$group,esarsaplot_10$val)

sarsaplot_10 <- c()
sarsaplot_10$group[1:agents_per_list] <- "sarsa"
sarsaplot_10$val <- sarsa_10
sar2_10<- cbind(sarsaplot_10$group,sarsaplot_10$val)

ggdf_10<-rbind(sar1_10,sar2_10,q_10,qv_10)
ggdf_10 <- data.frame(ggdf_10)
colnames(ggdf_10) <- c("type","time")
ggdf_10$time<- as.numeric(as.character(ggdf_10$time))

ggplot(ggdf_0,aes(x=time,fill=as.factor(type), color=type)) +geom_freqpoly(bins=20) + theme_classic() + ggtitle(paste(reward_value,"total reward:", agents_per_list, "of Each Agent (0 percent)", sep = " "))

ggplot(ggdf_10,aes(x=time,fill=as.factor(type), color=type)) + geom_freqpoly(bins=20) + theme_classic() + ggtitle(paste(reward_value,"total reward:", agents_per_list, "of Each Agent (10 percent)", sep = " "))

       