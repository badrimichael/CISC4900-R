library(ggplot2)

output = read.csv(file = 'sampleoutput/output-0percent.csv', head = TRUE, sep = ',')

for (agent_type in levels(output$Agent.Type)) {
  write.csv(subset(output, Agent.Type == agent_type),
            agent_type,
            row.names = FALSE)
}

q_agents = read.csv(file = 'Q-learning', head = TRUE, sep = ',')
sarsa_agents = read.csv(file = 'SARSA', head = TRUE, sep = ',')
expected_sarsa_agents = read.csv(file = 'Expected SARSA', head = TRUE, sep =
                                   ',')
qv_agents = read.csv(file = 'QV-learning', head = TRUE, sep = ',')

reward_value = 25000
total_agents = max(output$Agent)


i = 1
q_learning <- c()
for (i in 1:200) {
  sub = q_agents[q_agents$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  q_learning = c(q_learning, besttime)
}




sarsa <- c()
for (i in 201:400) {
  sub = sarsa_agents[sarsa_agents$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  sarsa = c(sarsa, besttime)
}



expected_sarsa <- c()
for (i in 401:600) {
  sub = expected_sarsa_agents[expected_sarsa_agents$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  expected_sarsa = c(expected_sarsa, besttime)
}



qv_learning <- c()
for (i in 601:800) {
  sub = qv_agents[qv_agents$Agent == i, ]
  sub2 = sub[sub$Reward == reward_value, ]
  Times = sub2$Time[!is.na(sub2$Time)]
  besttime <- min(Times)
  qv_learning = c(qv_learning, besttime)
}

qplot(q_learning,geom='freqpoly')

qplot <- c()
qplot$group[1:200] <- "q-learning"
qplot$val <- q_learning
q<- cbind(qplot$group,qplot$val)

qvplot <- c()
qvplot$group[1:200] <- "qv-learning"
qvplot$val <- qv_learning
qv<- cbind(qvplot$group,qvplot$val)

esarsaplot <- c()
esarsaplot$group[1:200] <- "expected_sarsa"
esarsaplot$val <- expected_sarsa
sar1<-cbind(esarsaplot$group,esarsaplot$val)

sarsaplot <- c()
sarsaplot$group[1:200] <- "sarsa"
sarsaplot$val <- sarsa
sar2<- cbind(sarsaplot$group,sarsaplot$val)


ggdf<-rbind(sar1,sar2,q,qv)
ggdf <- data.frame(ggdf)
colnames(ggdf) <- c("type","time")
ggdf$time<- as.numeric(as.character(ggdf$time))

ggplot(ggdf,aes(x=time,fill=as.factor(type), color=type)) +geom_freqpoly(bins=25) + theme_classic()



       