# Vietnam main results
setwd("~/git/HCVST/")
source("model_structure.R")

trustoraltest <- param_in %>% dplyr::select(Label,Value=Vietnam) 
TOT.highselftest <- trustoraltest %>% mutate(Value=replace(Value,Label=="n",0.5))
TOT.highreplace <- trustoraltest %>% mutate(Value=replace(Value,Label=="o",0.2))
TOT.reporthigh <- trustoraltest %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95))

noselftest <- trustoraltest %>% mutate(Value=replace(Value,Label %in% (c("n","o")),0))

retestalloral <- trustoraltest %>% mutate(Value=replace(Value,Label=="v",0))  
RTO.highselftest <- retestalloral %>% mutate(Value=replace(Value,Label=="n",0.5))
RTO.highreplace <- retestalloral %>% mutate(Value=replace(Value,Label=="o",0.2))
RTO.reporthigh <- retestalloral %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95))

trustbloodtest <- trustoraltest %>% mutate(Value=replace(Value,Label=="e",1.5))
TBT.highselftest <- trustbloodtest %>% mutate(Value=replace(Value,Label=="n",0.5))
TBT.highreplace <- trustbloodtest %>% mutate(Value=replace(Value,Label=="o",0.2))
TBT.reporthigh <- trustbloodtest %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95))

retestallblood <- trustbloodtest %>% mutate(Value=replace(Value,Label=="v",0))
RTB.highselftest <- retestallblood %>% mutate(Value=replace(Value,Label=="n",0.5))
RTB.highreplace <- retestallblood %>% mutate(Value=replace(Value,Label=="o",0.2))
RTB.reporthigh <- retestallblood %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95))

### remove all treatment costs
treatcosts <- c("l","h","g.svr")

trustoraltestD <- param_in %>% dplyr::select(Label,Value=Vietnam) %>% mutate(Value=replace(Value,Label %in% treatcosts,0))
TOT.highselftestD <- trustoraltestD %>% mutate(Value=replace(Value,Label=="n",0.5)) 
TOT.highreplaceD <- trustoraltestD %>% mutate(Value=replace(Value,Label=="o",0.2)) 
TOT.reporthighD <- trustoraltestD %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95)) 

noselftestD <- trustoraltestD %>% mutate(Value=replace(Value,Label %in% (c("n","o")),0)) 

retestalloralD <- trustoraltestD %>% mutate(Value=replace(Value,Label=="v",0))   
RTO.highselftestD <- retestalloralD %>% mutate(Value=replace(Value,Label=="n",0.5)) 
RTO.highreplaceD <- retestalloralD %>% mutate(Value=replace(Value,Label=="o",0.2)) 
RTO.reporthighD <- retestalloralD %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95)) 

trustbloodtestD <- trustoraltestD %>% mutate(Value=replace(Value,Label=="e",1.5)) 
TBT.highselftestD <- trustbloodtestD %>% mutate(Value=replace(Value,Label=="n",0.5)) 
TBT.highreplaceD <- trustbloodtestD %>% mutate(Value=replace(Value,Label=="o",0.2)) 
TBT.reporthighD <- trustbloodtestD %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95)) 

retestallbloodD <- trustbloodtestD %>% mutate(Value=replace(Value,Label=="v",0)) 
RTB.highselftestD <- retestallbloodD %>% mutate(Value=replace(Value,Label=="n",0.5)) 
RTB.highreplaceD <- retestallbloodD %>% mutate(Value=replace(Value,Label=="o",0.2)) 
RTB.reporthighD <- retestallbloodD %>% mutate(Value=replace(Value,Label %in% (c("z1","z3")),0.95)) 




Vietnam_list <- list(trustoraltest,TOT.highselftest,TOT.highreplace,TOT.reporthigh,noselftest,retestalloral,RTO.highselftest,RTO.highreplace,RTO.reporthigh,trustbloodtest,TBT.highselftest,TBT.highreplace,TBT.reporthigh,retestallblood,RTB.highselftest,RTB.highreplace,RTB.reporthigh)

Vietnam_listD <- list(trustoraltestD,TOT.highselftestD,TOT.highreplaceD,TOT.reporthighD,noselftestD,retestalloralD,RTO.highselftestD,RTO.highreplaceD,RTO.reporthighD,trustbloodtestD,TBT.highselftestD,TBT.highreplaceD,TBT.reporthighD,retestallbloodD,RTB.highselftestD,RTB.highreplaceD,RTB.reporthighD)

scenario_names <- c("trustoraltest","TOT.highselftest","TOT.highreplace","TOT.reporthigh","noselftest","retestalloral","RTO.highselftest","RTO.highreplace","RTO.reporthigh","trustbloodtest","TBT.highselftest","TBT.highreplace","TBT.reporthigh","retestallblood","RTB.highselftest","RTB.highreplace","RTB.reporthigh")
scenario_namesD <- c("trustoraltestD","TOT.highselftestD","TOT.highreplaceD","TOT.reporthighD","noselftestD","retestalloralD","RTO.highselftestD","RTO.highreplaceD","RTO.reporthighD","trustbloodtestD","TBT.highselftestD","TBT.highreplaceD","TBT.reporthighD","retestallbloodD","RTB.highselftestD","RTB.highreplaceD","RTB.reporthighD")
names(Vietnam_list) <- scenario_names  
names(Vietnam_listD) <- scenario_namesD

Vietnam_out <- Vietnam_list
names(Vietnam_out) <- names(Vietnam_list)
Vietnam_cost <- data.frame(scenario=scenario_names,cost=NA,diagnosed=NA,treated=NA,cured=NA) 

Vietnam_outD <- Vietnam_listD
names(Vietnam_outD) <- names(Vietnam_listD)
Vietnam_costD <- data.frame(scenario=scenario_namesD,cost=NA,diagnosed=NA,treated=NA,cured=NA) 

for(i in 1:length(Vietnam_list)) {
  
  param <- Vietnam_list[[i]]
  struc <-  assignSelfReport(param,struc1.long,rnd=8)
  mat <- makeMatrix(struc)
  cost <- totalcosts(mC=mat$vals,mP=mat$prob)
  cascade <- numbers(mat,struc1,param) # note this needs to be struc1 not struc1.long
  Vietnam_out[[i]] <- list(param=param,struc=struc,mat=mat,cost=cost,cascade=cascade)
  Vietnam_cost$cost[i] <- cost[1] 
  Vietnam_cost$diagnosed[i] <- cascade$number[cascade$groups=="Chronic"]
  Vietnam_cost$treated[i] <- cascade$number[cascade$groups=="Treated"]
  Vietnam_cost$cured[i] <- cascade$number[cascade$groups=="Cured"]
  
  paramD <- Vietnam_listD[[i]]
  strucD <-  assignSelfReport(paramD,struc1.long,rnd=8)
  matD <- makeMatrix(strucD)
  costD <- totalcosts(mC=matD$vals,mP=matD$prob)
  cascadeD <- numbers(matD,struc1,paramD) # note this needs to be struc1 not struc1.long
  Vietnam_outD[[i]] <- list(param=paramD,struc=strucD,mat=matD,cost=costD,cascade=cascadeD)
  Vietnam_costD$cost[i] <- costD[1] 
  Vietnam_costD$diagnosed[i] <- cascadeD$number[cascadeD$groups=="Chronic"]
  Vietnam_costD$treated[i] <- cascadeD$number[cascadeD$groups=="Treated"]
  Vietnam_costD$cured[i] <- cascadeD$number[cascadeD$groups=="Cured"]
}

Vietnam_cost$totalcost <- Vietnam_cost$cost*param_in$Vietnam[param_in$Label=="a"]*param_in$Vietnam[param_in$Label=="c"]
Vietnam_cost$diff <- Vietnam_cost$totalcost - Vietnam_cost$totalcost[Vietnam_cost$scenario=="noselftest"]
Vietnam_cost$perc <- Vietnam_cost$diff/Vietnam_cost$totalcost[Vietnam_cost$scenario=="noselftest"]

Vietnam_cost$diffd <- Vietnam_cost$diagnosed - Vietnam_cost$diagnosed[Vietnam_cost$scenario=="noselftest"]
Vietnam_cost$difft <- Vietnam_cost$treated - Vietnam_cost$treated[Vietnam_cost$scenario=="noselftest"]
Vietnam_cost$diffc <- Vietnam_cost$cured - Vietnam_cost$cured[Vietnam_cost$scenario=="noselftest"]

Vietnam_cost$cpd <- Vietnam_cost$diff / Vietnam_cost$diffd
Vietnam_cost$cpt <- Vietnam_cost$diff / Vietnam_cost$difft
Vietnam_cost$cpc <- Vietnam_cost$diff / Vietnam_cost$diffc


Vietnam_costD$totalcost <- Vietnam_costD$cost*param_in$Vietnam[param_in$Label=="a"]*param_in$Vietnam[param_in$Label=="c"]
Vietnam_costD$diff <- Vietnam_costD$totalcost - Vietnam_costD$totalcost[Vietnam_costD$scenario=="noselftestD"]
Vietnam_costD$perc <- Vietnam_costD$diff/Vietnam_costD$totalcost[Vietnam_costD$scenario=="noselftestD"]

Vietnam_costD$diffd <- Vietnam_costD$diagnosed - Vietnam_costD$diagnosed[Vietnam_costD$scenario=="noselftestD"]
Vietnam_costD$difft <- Vietnam_costD$treated - Vietnam_costD$treated[Vietnam_costD$scenario=="noselftestD"]
Vietnam_costD$diffc <- Vietnam_costD$cured - Vietnam_costD$cured[Vietnam_costD$scenario=="noselftestD"]

Vietnam_costD$cpd <- Vietnam_costD$diff / Vietnam_costD$diffd
Vietnam_costD$cpt <- Vietnam_costD$diff / Vietnam_costD$difft
Vietnam_costD$cpc <- Vietnam_costD$diff / Vietnam_costD$diffc

Vietnam_cost_out <- bind_rows(Vietnam_cost,Vietnam_costD)

write.xlsx(Vietnam_cost_out,"Results.xlsx",sheetName = "Vietnam",append=TRUE)
