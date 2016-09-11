myy <- read.table("New.txt", header = TRUE, sep = ";")

m1 <- data.frame(myy)

m1$ID<-seq.int(nrow(m1))

m2 <- m1[c(23,1:22)]

summary(m2)

for(i in m2[,1])
{
    if(m2$default[i] == "no")
    {
      if(m2$housing[i] == "no")
      {
        if(m2$loan[i] == "no")
         {
          m2$new.user[i] = "yes"
        }
      }
      else
        m2$new.user[i] = "no"
    }
  else
    m2$new.user[i] = "no"
}
table(m2$new.user)

m2$age.group <- findInterval(m1$age, c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

new.m<- table(m2$age.group, m2$response)

new.m1 = as.data.frame.matrix(new.m)

colnames(new.m1)[1] <- "Age Group"

new.m1$ID<-seq.int(nrow(new.m1))

new.m1 <- new.m1[c(3,1:2)]

mm <- melt(new.m1[,c('ID','no','yes')],id.vars = 1)

ggplot(mm,aes(x = ID,y = value, fill = variable)) + 
           geom_bar(stat="identity", position = "dodge") +
  scale_x_continuous(breaks=seq(10, 90, 10))



pie(new.m1$yes, new.m1$ID, labels = names(new.m1$ID), edges = 200, radius = 0.8)


piepercent<- round(100*new.m1$yes/sum(new.m1$yes), 1)

piepercent <- paste(piepercent, "%")

pie(new.m1$yes, labels = piepercent, main = "Distribution of age wrt positive response", col = rainbow(length(new.m1$yes)))
legend("bottomleft", c("10-19","20-29","30-39","40-49", "50-59", "60-69", "70-79", "80-89", "90-99"), cex = 1.0,
      fill = rainbow(length(new.m1$yes)))

library(data.table)

n2 <- table(m1$month, m1$response)

n2 = as.data.frame.matrix(n2)

setDT(n2, keep.rownames = TRUE)[]

colnames(n2)[1] <- "month"

nn2 <- melt(n2[,c('month','no','yes')],id.vars = 1)

piepercent1<- round(100*n2$yes/sum(n2$yes), 1)

piepercent1 <- paste(piepercent1, "%")

pie(n2$yes, labels = piepercent1, main = "Distribution of months wrt positive response", col = rainbow(length(n2$yes)))
legend("topright", c(n2$month), cex = 1.0,
       fill = rainbow(length(n2$yes)))

  j2 <- table(m1$job, m1$response)
    j2 = as.data.frame.matrix(j2)
    setDT(j2, keep.rownames = TRUE)[]
    colnames(j2)[1] <- "job"

barplot(j2$yes, names.arg=j2$job, horiz=TRUE, las=1, cex.names=0.5, border=TRUE, xlab = "Count", main = "Profession wrt positive response", axes = "TRUE")


table(m2$campaign, m2$response)

for(i in m2[,1])
{
  if(m2$default[i] == "yes")
  {
    if(m2$housing[i] == "yes")
    {
      if(m2$loan[i] == "yes")
      {
        m2$old.user[i] = "yes"
      }
    }
    else
      m2$old.user[i] = "no"
  }
  else
    m2$old.user[i] = "no"
}

cc <- count(m2$new.user)
cc = as.data.frame.matrix(cc)
