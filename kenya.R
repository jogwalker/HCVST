# Kenya main results
setwd("~/git/HCVST/")
source("model_structure.R")

trustoraltest <- param_in %>% dplyr::select(Label,Value=Kenya) 
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

kenya_list <- list(trustoraltest,TOT.highselftest,TOT.highreplace,TOT.reporthigh,noselftest,retestalloral,RTO.highselftest,RTO.highreplace,RTO.reporthigh,trustbloodtest,TBT.highselftest,TBT.highreplace,TBT.reporthigh,retestallblood,RTB.highselftest,RTB.highreplace,RTB.reporthigh)

scenario_names <- c("trustoraltest","TOT.highselftest","TOT.highreplace","TOT.reporthigh","noselftest","retestalloral","RTO.highselftest","RTO.highreplace","RTO.reporthigh","trustbloodtest","TBT.highselftest","TBT.highreplace","TBT.reporthigh","retestallblood","RTB.highselftest","RTB.highreplace","RTB.reporthigh")
names(kenya_list) <- scenario_names  

kenya_out <- kenya_list
names(kenya_out) <- names(kenya_list)
kenya_cost <- data.frame(scenario=scenario_names,cost=NA) 

for(i in 1:length(kenya_list)) {
  
  param <- kenya_list[[i]]
  struc <-  assignSelfReport(param,struc1.long,rnd=8)
  mat <- makeMatrix(struc)
  cost <- totalcosts(mC=mat$vals,mP=mat$prob)
  cascade <- numbers(mat,struc1,param) # note this needs to be struc1 not struc1.long
  kenya_out[[i]] <- list(param=param,struc=struc,mat=mat,cost=cost,cascade=cascade)
  kenya_cost$cost[i] <- cost[1] 
}

kenya_cost$totalcost <- kenya_cost$cost*param_in$Kenya[param_in$Label=="a"]*param_in$Kenya[param_in$Label=="c"]
kenya_cost$diff <- kenya_cost$totalcost - kenya_cost$totalcost[kenya_cost$scenario=="noselftest"]
kenya_cost$perc <- kenya_cost$diff/kenya_cost$totalcost[kenya_cost$scenario=="noselftest"]


