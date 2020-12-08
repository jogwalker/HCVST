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

trustoraltestD <- param_in %>% dplyr::select(Label,Value=China) %>% mutate(Value=replace(Value,Label %in% treatcosts,0))
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




China_list <- list(trustoraltest,TOT.highselftest,TOT.highreplace,TOT.reporthigh,noselftest,retestalloral,RTO.highselftest,RTO.highreplace,RTO.reporthigh,trustbloodtest,TBT.highselftest,TBT.highreplace,TBT.reporthigh,retestallblood,RTB.highselftest,RTB.highreplace,RTB.reporthigh)

China_listD <- list(trustoraltestD,TOT.highselftestD,TOT.highreplaceD,TOT.reporthighD,noselftestD,retestalloralD,RTO.highselftestD,RTO.highreplaceD,RTO.reporthighD,trustbloodtestD,TBT.highselftestD,TBT.highreplaceD,TBT.reporthighD,retestallbloodD,RTB.highselftestD,RTB.highreplaceD,RTB.reporthighD)

scenario_names <- c("trustoraltest","TOT.highselftest","TOT.highreplace","TOT.reporthigh","noselftest","retestalloral","RTO.highselftest","RTO.highreplace","RTO.reporthigh","trustbloodtest","TBT.highselftest","TBT.highreplace","TBT.reporthigh","retestallblood","RTB.highselftest","RTB.highreplace","RTB.reporthigh")
scenario_namesD <- c("trustoraltestD","TOT.highselftestD","TOT.highreplaceD","TOT.reporthighD","noselftestD","retestalloralD","RTO.highselftestD","RTO.highreplaceD","RTO.reporthighD","trustbloodtestD","TBT.highselftestD","TBT.highreplaceD","TBT.reporthighD","retestallbloodD","RTB.highselftestD","RTB.highreplaceD","RTB.reporthighD")
names(China_list) <- scenario_names  
names(China_listD) <- scenario_namesD

China_out <- China_list
names(China_out) <- names(China_list)
China_cost <- data.frame(scenario=scenario_names,cost=NA,diagnosed=NA,treated=NA,cured=NA) 

China_outD <- China_listD
names(China_outD) <- names(China_listD)
China_costD <- data.frame(scenario=scenario_namesD,cost=NA,diagnosed=NA,treated=NA,cured=NA) 

for(i in 1:length(China_list)) {
  
  param <- China_list[[i]]
  struc <-  assignSelfReport(param,struc1.long,rnd=8)
  mat <- makeMatrix(struc)
  cost <- totalcosts(mC=mat$vals,mP=mat$prob)
  cascade <- numbers(mat,struc1,param) # note this needs to be struc1 not struc1.long
  China_out[[i]] <- list(param=param,struc=struc,mat=mat,cost=cost,cascade=cascade)
  China_cost$cost[i] <- cost[1] 
  China_cost$diagnosed[i] <- cascade$number[cascade$groups=="Chronic"]
  China_cost$treated[i] <- cascade$number[cascade$groups=="Treated"]
  China_cost$cured[i] <- cascade$number[cascade$groups=="Cured"]
  
  paramD <- China_listD[[i]]
  strucD <-  assignSelfReport(paramD,struc1.long,rnd=8)
  matD <- makeMatrix(strucD)
  costD <- totalcosts(mC=matD$vals,mP=matD$prob)
  cascadeD <- numbers(matD,struc1,paramD) # note this needs to be struc1 not struc1.long
  China_outD[[i]] <- list(param=paramD,struc=strucD,mat=matD,cost=costD,cascade=cascadeD)
  China_costD$cost[i] <- costD[1] 
  China_costD$diagnosed[i] <- cascadeD$number[cascadeD$groups=="Chronic"]
  China_costD$treated[i] <- cascadeD$number[cascadeD$groups=="Treated"]
  China_costD$cured[i] <- cascadeD$number[cascadeD$groups=="Cured"]
}

China_cost$totalcost <- China_cost$cost*param_in$China[param_in$Label=="a"]*param_in$China[param_in$Label=="c"]
China_cost$diff <- China_cost$totalcost - China_cost$totalcost[China_cost$scenario=="noselftest"]
China_cost$perc <- China_cost$diff/China_cost$totalcost[China_cost$scenario=="noselftest"]

China_cost$diffd <- China_cost$diagnosed - China_cost$diagnosed[China_cost$scenario=="noselftest"]
China_cost$difft <- China_cost$treated - China_cost$treated[China_cost$scenario=="noselftest"]
China_cost$diffc <- China_cost$cured - China_cost$cured[China_cost$scenario=="noselftest"]

China_cost$cpd <- China_cost$diff / China_cost$diffd
China_cost$cpt <- China_cost$diff / China_cost$difft
China_cost$cpc <- China_cost$diff / China_cost$diffc


China_costD$totalcost <- China_costD$cost*param_in$China[param_in$Label=="a"]*param_in$China[param_in$Label=="c"]
China_costD$diff <- China_costD$totalcost - China_costD$totalcost[China_costD$scenario=="noselftestD"]
China_costD$perc <- China_costD$diff/China_costD$totalcost[China_costD$scenario=="noselftestD"]

China_costD$diffd <- China_costD$diagnosed - China_costD$diagnosed[China_costD$scenario=="noselftestD"]
China_costD$difft <- China_costD$treated - China_costD$treated[China_costD$scenario=="noselftestD"]
China_costD$diffc <- China_costD$cured - China_costD$cured[China_costD$scenario=="noselftestD"]

China_costD$cpd <- China_costD$diff / China_costD$diffd
China_costD$cpt <- China_costD$diff / China_costD$difft
China_costD$cpc <- China_costD$diff / China_costD$diffc

China_cost_out <- bind_rows(China_cost,China_costD)

write.xlsx(China_cost_out,"Results.xlsx",sheetName = "China",append=TRUE)
