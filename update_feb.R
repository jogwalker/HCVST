# Feb update

setwd("~/git/HCVST/")
source("model_structure.R")

settings <- c("Kenya","Georgia","Vietnam","China")

param_list <- list()
param_list_xT <- list()

for(i in 1:length(settings)) {
  paramS <- param_sub %>% dplyr::select(Label, ReadIn=settings[i]) %>% mutate(NoST=ReadIn,BaseCase=ReadIn,dNAT=ReadIn,RDT=ReadIn,dNAT.RDT=ReadIn,PMC=ReadIn,LowAcc=ReadIn,HighUptake=ReadIn,LowSuccess=ReadIn,HighRepl=ReadIn,Link65=ReadIn)
  # set up alternative scenarios
  
# Baseline no ST (NoST)
  paramS$NoST[paramS$Label=="n"] <- 0
  paramS$NoST[paramS$Label=="o"] <- 0
  
# Repeat serologic testing with ST by Oraquick ST (BaseCase)
  paramS$BaseCase[paramS$Label=="v"] <- 0
  
#   * Direct to NAT testing following a reactive self-test; (dNAT)
  paramS$dNAT[paramS$Label=="v"] <- 1

#   *  Standard of care antibody testing (and repeat serologic testing) is by RDT (RDT)
  paramS$RDT[paramS$Label=="v"] <- 0
  paramS$RDT[paramS$Label=="f"] <- paramS$RDT[paramS$Label=="f1"]
  paramS$RDT[paramS$Label=="sens2"] <- 0.98
  paramS$RDT[paramS$Label=="spec2"] <- 1
  
#   *  Direct to NAT testing in combination with RDT as standard of care antibody test; (dNAT.RDT)
  paramS$dNAT.RDT[paramS$Label=="v"] <- 1
  paramS$dNAT.RDT[paramS$Label=="f"] <- paramS$dNAT.RDT[paramS$Label=="f1"]
  paramS$dNAT.RDT[paramS$Label=="sens2"] <- 0.98
  paramS$dNAT.RDT[paramS$Label=="spec2"] <- 1

#   *  Using a finger-prick blood-based test instead, based on PMC (PMC)
  paramS$PMC[paramS$Label=="v"] <- 0
  paramS$PMC[paramS$Label=="sens"] <- 0.96
  paramS$PMC[paramS$Label=="spec"] <- 0.99
  paramS$PMC[paramS$Label=="e"] <- 1.5
  
# *  Reduced HCVST performance to 80% sensitivity and 80% specificity; (LowAcc)
  paramS$LowAcc[paramS$Label=="v"] <- 0
  paramS$LowAcc[paramS$Label=="sens"] <- 0.8
  paramS$LowAcc[paramS$Label=="spec"] <- 0.8

# *  Increase the uptake of self-testing to 50% compared to 10% in the baseline; (HighUptake)
  paramS$HighUptake[paramS$Label=="v"] <- 0
  paramS$HighUptake[paramS$Label=="n"] <- 0.5

# *  Increase the proportion of self-test failures (invalid results) to 20% from 5% in the baseline; (LowSuccess)
  paramS$LowSuccess[paramS$Label=="v"] <- 0
  paramS$LowSuccess[paramS$Label=="w"] <- 0.8
  
# *  Increase the replacement of facility-based testing by self-testing to 20% compared to 5% in the baseline; (HighRepl)
  paramS$HighRepl[paramS$Label=="v"] <- 0
  paramS$HighRepl[paramS$Label=="o"] <- 0.2

# *  Assume that 65% (95% CI: 52%–78%) of reactive self-tests receive facility-based testing regardless of self-test distribution model (only affects Vietnam and Kenya). (Link65)
  paramS$HighRepl[paramS$Label=="v"] <- 0
  paramS$HighRepl[paramS$Label=="z1"] <- 0.65
  paramS$HighRepl[paramS$Label=="z3"] <- 0.65
  
  param_list[[settings[i]]] <- paramS
  
  # remove treatment costs for cost of diagnosis only
  paramSxT  <- paramS
  paramSxT[paramSxT$Label %in% c("l","h","g.svr"),3:ncol(paramSxT)] <- 0
  param_list_xT[[settings[i]]] <- paramSxT
}


### run models
scen_names <- names(paramS[3:ncol(paramS)])
results <- list()

for(i in 1:length(param_list)) {
  out_j <- list()
  for(j in 3:ncol(param_list[[i]])) {
    p_ij <- param_list[[i]][c(1,j)] 
    names(p_ij) <- c("Label","Value")
    struc <- assignSelfReport(p_ij,struc1.long,rnd=8)
    mat <- makeMatrix(struc)
    cost <- totalcosts(mC=mat$vals,mP=mat$prob)
    cascade <- numbers(mat,struc1,p_ij)
    
    # get costs with diagnosis only
    p_ij_xT <- param_list_xT[[i]][c(1,j)] 
    names(p_ij_xT) <- c("Label","Value")
    struc_xT <- assignSelfReport(p_ij_xT,struc1.long,rnd=8)
    mat_xT <- makeMatrix(struc_xT)
    cost_xT <- totalcosts(mC=mat_xT$vals,mP=mat_xT$prob)
    
    out_j[[scen_names[j-2]]] <- list(scenario=scen_names[j-2],param=p_ij,struc=struc,mat=mat,cost=cost,cost_xT=cost_xT,cascade=cascade)
  
  }
  results[[settings[i]]] <- out_j   
}

# summarise results needed
results_sum <- list()
for(i in 1:length(results))  {
  for (j in 1:length(results[[1]])) {
    setting <- names(results)[i]
    scenario <-  results[[i]][[j]]$scenario
    cost <- results[[i]][[j]]$cost[1]
    start_pop <- results[[i]][[j]]$cascade$number[results[[i]][[j]]$cascade$groups=="Start"]
    standard.tested <- results[[i]][[j]]$cascade$number[results[[i]][[j]]$cascade$groups=="StandardTested"]
    self.tested <- results[[i]][[j]]$cascade$number[results[[i]][[j]]$cascade$groups=="SelfTested"]
    Ab.pos <- results[[i]][[j]]$cascade$number[results[[i]][[j]]$cascade$groups=="ConfirmedAbPos"]
    diagnosed <- results[[i]][[j]]$cascade$number[results[[i]][[j]]$cascade$groups=="Chronic"]
    treated <- results[[i]][[j]]$cascade$number[results[[i]][[j]]$cascade$groups=="Treated"]
    cured <- results[[i]][[j]]$cascade$number[results[[i]][[j]]$cascade$groups=="Cured"]
    totalcost <- cost*start_pop   
    cost_xT <- results[[i]][[j]]$cost_xT[1]
    totalcost_xT <- cost_xT*start_pop  
    all.tested <- self.tested + standard.tested
    
    temp_j <- bind_cols(setting=setting,scenario=scenario,cost=cost,start_pop=start_pop,standard.tested=standard.tested,self.tested=self.tested,all.tested=all.tested,Ab.pos=Ab.pos,diagnosed=diagnosed,treated=treated,cured=cured,totalcost=totalcost,cost_xT=cost_xT,totalcost_xT=totalcost_xT)
    
    results_sum[[length(results_sum) + 1]] <- temp_j 
  }
}
results.df <- bind_rows(results_sum)

### 

numbers <- results.df %>% select(setting,scenario,start_pop:cured) %>% pivot_longer(names_to="outcome",values_to="number",start_pop:cured)
costs <- results.df %>% select(setting,scenario,cost,totalcost,cost_xT,totalcost_xT) %>% pivot_longer(names_to="outcome",values_to="cost",cost:totalcost_xT)
results.cf <- results.df %>% filter(scenario=="NoST") %>% select(setting,scenario,all.tested,diagnosed,cured,totalcost,totalcost_xT)
# cost per diagnosis & cure without HCVST
results.cf$CPD <- results.cf$totalcost_xT / results.cf$diagnosed
results.cf$CPC <- results.cf$totalcost / results.cf$cured

results.CE <- results.df %>% select(setting,scenario,totalcost,totalcost_xT,all.tested,diagnosed,cured)
results.CE <- left_join(results.CE,results.cf,by="setting")
results.CEd <- results.CE %>% select(setting,scenario.x)
results.CEd$totalcost.d <- with(results.CE,totalcost.x - totalcost.y)
results.CEd$totalcost_xT.d <- with(results.CE,totalcost_xT.x - totalcost_xT.y)
results.CEd$all.tested.d <- with(results.CE,all.tested.x - all.tested.y)
results.CEd$diagnosed.d <- with(results.CE,diagnosed.x - diagnosed.y)
results.CEd$cured.d <- with(results.CE,cured.x - cured.y)
results.CEd$costperdiagnosis <- with(results.CEd,totalcost_xT.d/diagnosed.d)
results.CEd$costpercure <- with(results.CEd,totalcost.d/cured.d)

results.CEp <- results.CEd %>% select(setting,scenario.x)
results.CEp$all.tested.p <- results.CEd$all.tested.d / results.CE$all.tested.y *100
results.CEp$diagnosed.p <- results.CEd$diagnosed.d / results.CE$diagnosed.y *100
results.CEp$cured.p <- results.CEd$cured.d / results.CE$cured.y *100

#### cascade of care
numbers$outcomef <- as.factor(numbers$outcome)
numbers$outcomef <- with(numbers, reorder(outcome, number, mean))
numbers$outcomef <- with(numbers,factor(outcomef, levels = rev(levels(outcomef))))
numbers$scenariof <- as.factor(numbers$scenario)
numbers$scenariof <- with(numbers, reorder(scenario, number, mean))
f3 <- numbers %>% filter(outcomef %in% c("all.tested","Ab.pos","diagnosed","treated","cured") & scenariof %in% c("NoST","BaseCase")) %>% ggplot(aes(x=outcomef,y=number,fill=scenariof)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~setting,scales="free") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Cascade of care") + ylab("Number of people")

pdf("fig3.pdf",width=7,height=7)
f3
dev.off()

#### sensitivity analysis on numbers diagnosed
numbers$group <- "Sensitivity analysis"
numbers$group[numbers$scenario=="BaseCase"] <- "Base Case"
numbers$group[numbers$scenario=="NoST"] <- "No HCVST"
numbers %>% filter(outcomef %in% c("diagnosed","treated")) %>% ggplot(aes(x=scenariof,y=number,fill=group)) + geom_bar(position="dodge",stat="identity") + facet_grid(setting~outcomef,scales="free") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Modelled Scenario") + ylab("Number of people diagnosed") 
f4 <- numbers %>% filter(outcomef %in% c("diagnosed")) %>% ggplot(aes(x=scenariof,y=number,fill=group)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~setting,scales="free") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Modelled Scenario") + ylab("Number of people diagnosed") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

pdf("fig4.pdf",width=7,height=7)
f4
dev.off()

#### sensitivity analysis on total cost
costs$group <- "Sensitivity analysis"
costs$group[costs$scenario=="BaseCase"] <- "Base Case"
costs$group[costs$scenario=="NoST"] <- "No HCVST"
costs$scenariof <- as.factor(costs$scenario)
costs$scenariof <- with(costs, reorder(scenario, cost, mean))
f5a <- costs %>% filter(outcome %in% c("totalcost_xT")) %>% ggplot(aes(x=scenariof,y=cost/1000,fill=group)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~setting,scales="free") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Modelled Scenario") + ylab("Total cost (Thousands of dollars)") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# including treatment costs
f5b <- costs %>% filter(outcome %in% c("totalcost")) %>% ggplot(aes(x=scenariof,y=cost/1000,fill=group)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~setting,scales="free") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Modelled Scenario") + ylab("Total cost (Thousands of dollars)") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

pdf("fig5.pdf",width=7,height=7)
f5a
dev.off()

#### Tornado plots

icer <- results.CEd %>% select(setting,scenario.x,costperdiagnosis,costpercure)
bc <- icer %>% filter(scenario.x=="BaseCase") 
icer <- left_join(icer,bc,by="setting")
icer$diffDiagnosis <- icer$costperdiagnosis.x - icer$costperdiagnosis.y
icer$diffCure <- icer$costpercure.x - icer$costpercure.y
icerD <- icer %>% arrange(setting, desc(abs(diffDiagnosis)))
icerC <- icer %>% arrange(setting, desc(abs(diffCure))) 


f6a <- icerD %>% filter(!scenario.x.x %in% c("BaseCase","NoST")) %>% ggplot() + geom_segment(aes(x=scenario.x.x,xend=scenario.x.x,yend=costperdiagnosis.y,y=costperdiagnosis.x,size=2))  + coord_flip() + facet_wrap(~setting,scales="free") + theme_classic()  + geom_hline(data=bc,aes(yintercept=costperdiagnosis)) + theme(legend.position="none")   + xlab("Scenario") +ylab("Incremental cost per diagnosis (USD)") # + geom_hline(yintercept=0,linetype="dotted")

f6b <- icerC %>% filter(!scenario.x.x %in% c("BaseCase","NoST")) %>% ggplot() + geom_segment(aes(x=scenario.x.x,xend=scenario.x.x,y=costpercure.y,yend=costpercure.x,size=2))  + coord_flip() + facet_wrap(~setting,scales="free") + theme_classic()  + geom_hline(data=bc,aes(yintercept=costpercure)) + theme(legend.position="none")   + xlab("Scenario") +ylab("Incremental cost per cure (USD)") # + geom_hline(yintercept=0,linetype="dotted")

pdf("fig6.pdf",width=7,height=7)
f6a
dev.off()

pdf("fig7.pdf",width=7,height=7)
f6b
dev.off()


### TO DO
# assuming RDT as standard of care test needs a different base case
# check results for Link65 as this should only affect Vietnam and Kenya
