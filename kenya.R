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

Kenya_list <- list(trustoraltest,TOT.highselftest,TOT.highreplace,TOT.reporthigh,noselftest,retestalloral,RTO.highselftest,RTO.highreplace,RTO.reporthigh,trustbloodtest,TBT.highselftest,TBT.highreplace,TBT.reporthigh,retestallblood,RTB.highselftest,RTB.highreplace,RTB.reporthigh)

scenario_names <- c("trustoraltest","TOT.highselftest","TOT.highreplace","TOT.reporthigh","noselftest","retestalloral","RTO.highselftest","RTO.highreplace","RTO.reporthigh","trustbloodtest","TBT.highselftest","TBT.highreplace","TBT.reporthigh","retestallblood","RTB.highselftest","RTB.highreplace","RTB.reporthigh")
names(Kenya_list) <- scenario_names  

Kenya_out <- Kenya_list
names(Kenya_out) <- names(Kenya_list)
Kenya_cost <- data.frame(scenario=scenario_names,cost=NA) 

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

write.xlsx(Kenya_cost,"Results.xlsx",sheetName = "Kenya", append=TRUE)