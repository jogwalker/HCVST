# Georgia main results
setwd("~/git/HCVST/")
source("model_structure.R")

trustoraltest <- param_in %>% dplyr::select(Label,Value=Georgia) 
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

trustoraltestD <- param_in %>% dplyr::select(Label,Value=Georgia) %>% mutate(Value=replace(Value,Label %in% treatcosts,0))
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




Georgia_list <- list(trustoraltest,TOT.highselftest,TOT.highreplace,TOT.reporthigh,noselftest,retestalloral,RTO.highselftest,RTO.highreplace,RTO.reporthigh,trustbloodtest,TBT.highselftest,TBT.highreplace,TBT.reporthigh,retestallblood,RTB.highselftest,RTB.highreplace,RTB.reporthigh)

Georgia_listD <- list(trustoraltestD,TOT.highselftestD,TOT.highreplaceD,TOT.reporthighD,noselftestD,retestalloralD,RTO.highselftestD,RTO.highreplaceD,RTO.reporthighD,trustbloodtestD,TBT.highselftestD,TBT.highreplaceD,TBT.reporthighD,retestallbloodD,RTB.highselftestD,RTB.highreplaceD,RTB.reporthighD)

scenario_names <- c("trustoraltest","TOT.highselftest","TOT.highreplace","TOT.reporthigh","noselftest","retestalloral","RTO.highselftest","RTO.highreplace","RTO.reporthigh","trustbloodtest","TBT.highselftest","TBT.highreplace","TBT.reporthigh","retestallblood","RTB.highselftest","RTB.highreplace","RTB.reporthigh")
scenario_namesD <- c("trustoraltestD","TOT.highselftestD","TOT.highreplaceD","TOT.reporthighD","noselftestD","retestalloralD","RTO.highselftestD","RTO.highreplaceD","RTO.reporthighD","trustbloodtestD","TBT.highselftestD","TBT.highreplaceD","TBT.reporthighD","retestallbloodD","RTB.highselftestD","RTB.highreplaceD","RTB.reporthighD")
 names(Georgia_list) <- scenario_names  
 names(Georgia_listD) <- scenario_namesD

Georgia_out <- Georgia_list
names(Georgia_out) <- names(Georgia_list)
Georgia_cost <- data.frame(scenario=scenario_names,cost=NA,diagnosed=NA,treated=NA,cured=NA) 

Georgia_outD <- Georgia_listD
names(Georgia_outD) <- names(Georgia_listD)
Georgia_costD <- data.frame(scenario=scenario_namesD,cost=NA,diagnosed=NA,treated=NA,cured=NA) 

for(i in 1:length(Georgia_list)) {
  
  param <- Georgia_list[[i]]
  struc <-  assignSelfReport(param,struc1.long,rnd=8)
  mat <- makeMatrix(struc)
  cost <- totalcosts(mC=mat$vals,mP=mat$prob)
  cascade <- numbers(mat,struc1,param) # note this needs to be struc1 not struc1.long
  Georgia_out[[i]] <- list(param=param,struc=struc,mat=mat,cost=cost,cascade=cascade)
  Georgia_cost$cost[i] <- cost[1] 
  Georgia_cost$diagnosed[i] <- cascade$number[cascade$groups=="Chronic"]
  Georgia_cost$treated[i] <- cascade$number[cascade$groups=="Treated"]
  Georgia_cost$cured[i] <- cascade$number[cascade$groups=="Cured"]
  
  paramD <- Georgia_listD[[i]]
  strucD <-  assignSelfReport(paramD,struc1.long,rnd=8)
  matD <- makeMatrix(strucD)
  costD <- totalcosts(mC=matD$vals,mP=matD$prob)
  cascadeD <- numbers(matD,struc1,paramD) # note this needs to be struc1 not struc1.long
  Georgia_outD[[i]] <- list(param=paramD,struc=strucD,mat=matD,cost=costD,cascade=cascadeD)
  Georgia_costD$cost[i] <- costD[1] 
  Georgia_costD$diagnosed[i] <- cascadeD$number[cascadeD$groups=="Chronic"]
  Georgia_costD$treated[i] <- cascadeD$number[cascadeD$groups=="Treated"]
  Georgia_costD$cured[i] <- cascadeD$number[cascadeD$groups=="Cured"]
}

Georgia_cost$totalcost <- Georgia_cost$cost*param_in$Georgia[param_in$Label=="a"]*param_in$Georgia[param_in$Label=="c"]
Georgia_cost$diff <- Georgia_cost$totalcost - Georgia_cost$totalcost[Georgia_cost$scenario=="noselftest"]
Georgia_cost$perc <- Georgia_cost$diff/Georgia_cost$totalcost[Georgia_cost$scenario=="noselftest"]

Georgia_cost$diffd <- Georgia_cost$diagnosed - Georgia_cost$diagnosed[Georgia_cost$scenario=="noselftest"]
Georgia_cost$difft <- Georgia_cost$treated - Georgia_cost$treated[Georgia_cost$scenario=="noselftest"]
Georgia_cost$diffc <- Georgia_cost$cured - Georgia_cost$cured[Georgia_cost$scenario=="noselftest"]

Georgia_cost$cpd <- Georgia_cost$diff / Georgia_cost$diffd
Georgia_cost$cpt <- Georgia_cost$diff / Georgia_cost$difft
Georgia_cost$cpc <- Georgia_cost$diff / Georgia_cost$diffc


Georgia_costD$totalcost <- Georgia_costD$cost*param_in$Georgia[param_in$Label=="a"]*param_in$Georgia[param_in$Label=="c"]
Georgia_costD$diff <- Georgia_costD$totalcost - Georgia_costD$totalcost[Georgia_costD$scenario=="noselftestD"]
Georgia_costD$perc <- Georgia_costD$diff/Georgia_costD$totalcost[Georgia_costD$scenario=="noselftestD"]

Georgia_costD$diffd <- Georgia_costD$diagnosed - Georgia_costD$diagnosed[Georgia_costD$scenario=="noselftestD"]
Georgia_costD$difft <- Georgia_costD$treated - Georgia_costD$treated[Georgia_costD$scenario=="noselftestD"]
Georgia_costD$diffc <- Georgia_costD$cured - Georgia_costD$cured[Georgia_costD$scenario=="noselftestD"]

Georgia_costD$cpd <- Georgia_costD$diff / Georgia_costD$diffd
Georgia_costD$cpt <- Georgia_costD$diff / Georgia_costD$difft
Georgia_costD$cpc <- Georgia_costD$diff / Georgia_costD$diffc

Georgia_cost_out <- bind_rows(Georgia_cost,Georgia_costD)

write.xlsx(Georgia_cost_out,"Results.xlsx",sheetName = "Georgia")
