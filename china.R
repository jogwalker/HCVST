# China

# China main results
setwd("~/git/HCVST/")
source("model_structure.R")

trustoraltest <- param_in %>% dplyr::select(Label,Value=China) 
TOT.highselftest <- trustoraltest %>% mutate(Value=replace(Value,Label=="n",0.5))
TOT.highreplace <- trustoraltest %>% mutate(Value=replace(Value,Label=="o",0.2))
TOT.reporthigh <- trustoraltest %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95))

noselftest <- trustoraltest %>% mutate(Value=replace(Value,Label %in% (c("n","o")),0))

retestalloral <- trustoraltest %>% mutate(Value=replace(Value,Label=="v",0))
RTO.highselftest <- retestalloral %>% mutate(Value=replace(Value,Label=="n",0.5))
RTO.highreplace <- retestalloral %>% mutate(Value=replace(Value,Label=="o",0.2))
RTO.reporthigh <- retestalloral %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95))

trustbloodtest <- trustoraltest %>% mutate(Value=replace(Value,Label=="e",0.8))
TBT.highselftest <- trustbloodtest %>% mutate(Value=replace(Value,Label=="n",0.5))
TBT.highreplace <- trustbloodtest %>% mutate(Value=replace(Value,Label=="o",0.2))
TBT.reporthigh <- trustbloodtest %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95))

retestallblood <- trustbloodtest %>% mutate(Value=replace(Value,Label=="v",0))
RTB.highselftest <- retestallblood %>% mutate(Value=replace(Value,Label=="n",0.5))
RTB.highreplace <- retestallblood %>% mutate(Value=replace(Value,Label=="o",0.2))
RTB.reporthigh <- retestallblood %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95))

China_list <- list(trustoraltest,TOT.highselftest,TOT.highreplace,TOT.reporthigh,noselftest,retestalloral,RTO.highselftest,RTO.highreplace,RTO.reporthigh,trustbloodtest,TBT.highselftest,TBT.highreplace,TBT.reporthigh,retestallblood,RTB.highselftest,RTB.highreplace,RTB.reporthigh)

scenario_names <- c("trustoraltest","TOT.highselftest","TOT.highreplace","TOT.reporthigh","noselftest","retestalloral","RTO.highselftest","RTO.highreplace","RTO.reporthigh","trustbloodtest","TBT.highselftest","TBT.highreplace","TBT.reporthigh","retestallblood","RTB.highselftest","RTB.highreplace","RTB.reporthigh")
names(China_list) <- scenario_names  

China_out <- China_list
names(China_out) <- names(China_list)
China_cost <- data.frame(scenario=scenario_names,cost=NA) 

for(i in 1:length(China_list)) {
  param <- China_list[[i]]
  struc <-  assignSelfReport(param,struc1.long,rnd=5)
  mat <- makeMatrix(struc)
  cost <- totalcosts(mC=mat$vals,mP=mat$prob)
  cascade <- numbers(mat,struc1,param) # note this needs to be struc1 not struc1.long
  China_out[[i]] <- list(param=param,struc=struc,mat=mat,cost=cost,cascade=cascade)
  China_cost$cost[i] <- cost[1] 
}

China_cost$totalcost <- China_cost$cost*param_in$China[param_in$Label=="a"]*param_in$China[param_in$Label=="c"]
China_cost$diff <- China_cost$totalcost - China_cost$totalcost[China_cost$scenario=="noselftest"]
China_cost$perc <- China_cost$diff/China_cost$totalcost[China_cost$scenario=="noselftest"]


