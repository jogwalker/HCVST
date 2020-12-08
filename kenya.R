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

trustoraltestD <- param_in %>% dplyr::select(Label,Value=Kenya) %>% mutate(Value=replace(Value,Label %in% treatcosts,0))
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




Kenya_list <- list(trustoraltest,TOT.highselftest,TOT.highreplace,TOT.reporthigh,noselftest,retestalloral,RTO.highselftest,RTO.highreplace,RTO.reporthigh,trustbloodtest,TBT.highselftest,TBT.highreplace,TBT.reporthigh,retestallblood,RTB.highselftest,RTB.highreplace,RTB.reporthigh)

Kenya_listD <- list(trustoraltestD,TOT.highselftestD,TOT.highreplaceD,TOT.reporthighD,noselftestD,retestalloralD,RTO.highselftestD,RTO.highreplaceD,RTO.reporthighD,trustbloodtestD,TBT.highselftestD,TBT.highreplaceD,TBT.reporthighD,retestallbloodD,RTB.highselftestD,RTB.highreplaceD,RTB.reporthighD)

scenario_names <- c("trustoraltest","TOT.highselftest","TOT.highreplace","TOT.reporthigh","noselftest","retestalloral","RTO.highselftest","RTO.highreplace","RTO.reporthigh","trustbloodtest","TBT.highselftest","TBT.highreplace","TBT.reporthigh","retestallblood","RTB.highselftest","RTB.highreplace","RTB.reporthigh")
scenario_namesD <- c("trustoraltestD","TOT.highselftestD","TOT.highreplaceD","TOT.reporthighD","noselftestD","retestalloralD","RTO.highselftestD","RTO.highreplaceD","RTO.reporthighD","trustbloodtestD","TBT.highselftestD","TBT.highreplaceD","TBT.reporthighD","retestallbloodD","RTB.highselftestD","RTB.highreplaceD","RTB.reporthighD")
names(Kenya_list) <- scenario_names  
names(Kenya_listD) <- scenario_namesD

Kenya_out <- Kenya_list
names(Kenya_out) <- names(Kenya_list)
Kenya_cost <- data.frame(scenario=scenario_names,cost=NA,diagnosed=NA,treated=NA,cured=NA) 

Kenya_outD <- Kenya_listD
names(Kenya_outD) <- names(Kenya_listD)
Kenya_costD <- data.frame(scenario=scenario_namesD,cost=NA,diagnosed=NA,treated=NA,cured=NA) 

for(i in 1:length(Kenya_list)) {
  
  param <- Kenya_list[[i]]
  struc <-  assignSelfReport(param,struc1.long,rnd=8)
  mat <- makeMatrix(struc)
  cost <- totalcosts(mC=mat$vals,mP=mat$prob)
  cascade <- numbers(mat,struc1,param) # note this needs to be struc1 not struc1.long
  Kenya_out[[i]] <- list(param=param,struc=struc,mat=mat,cost=cost,cascade=cascade)
  Kenya_cost$cost[i] <- cost[1] 
  Kenya_cost$diagnosed[i] <- cascade$number[cascade$groups=="Chronic"]
  Kenya_cost$treated[i] <- cascade$number[cascade$groups=="Treated"]
  Kenya_cost$cured[i] <- cascade$number[cascade$groups=="Cured"]
  
  paramD <- Kenya_listD[[i]]
  strucD <-  assignSelfReport(paramD,struc1.long,rnd=8)
  matD <- makeMatrix(strucD)
  costD <- totalcosts(mC=matD$vals,mP=matD$prob)
  cascadeD <- numbers(matD,struc1,paramD) # note this needs to be struc1 not struc1.long
  Kenya_outD[[i]] <- list(param=paramD,struc=strucD,mat=matD,cost=costD,cascade=cascadeD)
  Kenya_costD$cost[i] <- costD[1] 
  Kenya_costD$diagnosed[i] <- cascadeD$number[cascadeD$groups=="Chronic"]
  Kenya_costD$treated[i] <- cascadeD$number[cascadeD$groups=="Treated"]
  Kenya_costD$cured[i] <- cascadeD$number[cascadeD$groups=="Cured"]
}

Kenya_cost$totalcost <- Kenya_cost$cost*param_in$Kenya[param_in$Label=="a"]*param_in$Kenya[param_in$Label=="c"]
Kenya_cost$diff <- Kenya_cost$totalcost - Kenya_cost$totalcost[Kenya_cost$scenario=="noselftest"]
Kenya_cost$perc <- Kenya_cost$diff/Kenya_cost$totalcost[Kenya_cost$scenario=="noselftest"]

Kenya_cost$diffd <- Kenya_cost$diagnosed - Kenya_cost$diagnosed[Kenya_cost$scenario=="noselftest"]
Kenya_cost$difft <- Kenya_cost$treated - Kenya_cost$treated[Kenya_cost$scenario=="noselftest"]
Kenya_cost$diffc <- Kenya_cost$cured - Kenya_cost$cured[Kenya_cost$scenario=="noselftest"]

Kenya_cost$cpd <- Kenya_cost$diff / Kenya_cost$diffd
Kenya_cost$cpt <- Kenya_cost$diff / Kenya_cost$difft
Kenya_cost$cpc <- Kenya_cost$diff / Kenya_cost$diffc


Kenya_costD$totalcost <- Kenya_costD$cost*param_in$Kenya[param_in$Label=="a"]*param_in$Kenya[param_in$Label=="c"]
Kenya_costD$diff <- Kenya_costD$totalcost - Kenya_costD$totalcost[Kenya_costD$scenario=="noselftestD"]
Kenya_costD$perc <- Kenya_costD$diff/Kenya_costD$totalcost[Kenya_costD$scenario=="noselftestD"]

Kenya_costD$diffd <- Kenya_costD$diagnosed - Kenya_costD$diagnosed[Kenya_costD$scenario=="noselftestD"]
Kenya_costD$difft <- Kenya_costD$treated - Kenya_costD$treated[Kenya_costD$scenario=="noselftestD"]
Kenya_costD$diffc <- Kenya_costD$cured - Kenya_costD$cured[Kenya_costD$scenario=="noselftestD"]

Kenya_costD$cpd <- Kenya_costD$diff / Kenya_costD$diffd
Kenya_costD$cpt <- Kenya_costD$diff / Kenya_costD$difft
Kenya_costD$cpc <- Kenya_costD$diff / Kenya_costD$diffc

Kenya_cost_out <- bind_rows(Kenya_cost,Kenya_costD)

write.xlsx(Kenya_cost_out,"Results.xlsx",sheetName = "Kenya",append=TRUE)
