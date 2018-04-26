output = read.csv(file='output.csv', head=TRUE, sep=',')

for(agent_type in levels(output$Agent.Type)){
  write.csv(subset(output,Agent.Type==agent_type),agent_type,row.names=FALSE)
}

q_agents = read.csv(file='Q-learning', head=TRUE, sep=',')
sarsa_agents = read.csv(file='SARSA', head=TRUE, sep=',')
expected_sarsa_agents = read.csv(file='Expected SARSA', head=TRUE, sep=',')
qv_agents = read.csv(file='QV-learning', head=TRUE, sep=',')

reward_value = 25000
total_agents = max(output$Agent)


i = 1
q_learning<-c()
for(i in 1:200){
  sub = q_agents[q_agents$Agent==i,]
  sub2 = sub[sub$Reward==reward_value,]
  Times=sub2$Time[!is.na(sub2$Time)]
  besttime<-min(Times)
  q_learning=c(q_learning,besttime)
}
hist(q_learning)



sarsa<-c()
for(i in 201:400){
  sub = sarsa_agents[sarsa_agents$Agent==i,]
  sub2 = sub[sub$Reward==reward_value,]
  Times=sub2$Time[!is.na(sub2$Time)]
  besttime<-min(Times)
  sarsa=c(sarsa,besttime)
}
hist(sarsa)


expected_sarsa<-c()
for(i in 401:600){
  sub = expected_sarsa_agents[expected_sarsa_agents$Agent==i,]
  sub2 = sub[sub$Reward==reward_value,]
  Times=sub2$Time[!is.na(sub2$Time)]
  besttime<-min(Times)
  expected_sarsa=c(expected_sarsa,besttime)
}
hist(expected_sarsa)


qv_learning<-c()
for(i in 601:800){
  sub = qv_agents[qv_agents$Agent==i,]
  sub2 = sub[sub$Reward==reward_value,]
  Times=sub2$Time[!is.na(sub2$Time)]
  besttime<-min(Times)
  qv_learning=c(qv_learning,besttime)
}
hist(qv_learning)