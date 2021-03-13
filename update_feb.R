# Feb update

setwd("~/git/HCVST/")
source("model_structure.R")

settings <- c("Kenya","Georgia","Vietnam","China")

param_list <- list()
param_list_xT <- list()

scen_names <- c(NoST="No ST",NoST.EIA="No ST EIA standard of care",BaseCase="Base Case ST",dNAT="Direct to NAT",EIA="EIA standard of care",PMC="Blood-based HCVST",PMChigh="High cost blood-based HCVST",OralHigh="High cost oral-fluid HCVST",EqualST="Equal cost HCVST",LowAcc="Low HCVST performance",HighIRA="High inter-reader agreement",HighLink="High linkage",LowLink="Low linkage",HighUp="High HCVST uptake",LowUp="Low HCVST uptake",HighSub="High substitution",LowSub="Low substitution",HighFail="Low self-test success",LowFail="High self-test success")

for(i in 1:length(settings)) {
  paramS <- param_sub %>% dplyr::select(Label, ReadIn=settings[i]) %>% mutate(NoST=ReadIn,NoST.EIA=ReadIn,BaseCase=ReadIn,dNAT=ReadIn,EIA=ReadIn, PMC=ReadIn,PMChigh=ReadIn,OralHigh=ReadIn,EqualST=ReadIn,LowAcc=ReadIn,HighIRA=ReadIn,HighLink=ReadIn,LowLink=ReadIn,HighUp=ReadIn,LowUp=ReadIn,HighSub=ReadIn,LowSub=ReadIn,HighFail=ReadIn,LowFail=ReadIn)
                                                                              
  # set up alternative scenarios
  
  #No ST: Baseline no ST 
  paramS$NoST[paramS$Label=="n"] <- 0
  paramS$NoST[paramS$Label=="o"] <- 0
  paramS$NoST[paramS$Label=="f"] <- paramS$NoST[paramS$Label=="f1"] #RDT standard
  
  #No ST EIA
  paramS$NoST.EIA[paramS$Label=="n"] <- 0
  paramS$NoST.EIA[paramS$Label=="o"] <- 0
  paramS$NoST.EIA[paramS$Label=="sens2"] <- 1
  paramS$NoST.EIA[paramS$Label=="spec2"] <- 1
  
  # Base Case: Repeat serologic testing by RDT with oral ST  
  paramS$BaseCase[paramS$Label=="v"] <- 0
  paramS$BaseCase[paramS$Label=="f"] <- paramS$BaseCase[paramS$Label=="f1"] #RDT standard  
  
  # Direct to NAT: Patients are tested by NAT testing following a reactive self-test; 
  paramS$dNAT[paramS$Label=="v"] <- 1
  paramS$dNAT[paramS$Label=="f"] <- paramS$dNAT[paramS$Label=="f1"] #RDT standard  
  
  # EIA standard of care: 
  paramS$EIA[paramS$Label=="v"] <- 0
  paramS$EIA[paramS$Label=="sens2"] <- 1
  paramS$EIA[paramS$Label=="spec2"] <- 1
  
#   Blood-based HCVST: 
  paramS$PMC[paramS$Label=="v"] <- 0
  paramS$PMC[paramS$Label=="f"] <- paramS$PMC[paramS$Label=="f1"] #RDT standard  
  paramS$PMC[paramS$Label=="sens"] <- 0.96
  paramS$PMC[paramS$Label=="spec"] <- 0.99
  paramS$PMC[paramS$Label=="e"] <- 2.25
  
# High cost blood-based HCVST cost: 
  
  paramS$PMChigh[paramS$Label=="v"] <- 0
  paramS$PMChigh[paramS$Label=="f"] <- paramS$PMChigh[paramS$Label=="f1"] #RDT standard  
  paramS$PMChigh[paramS$Label=="sens"] <- 0.96
  paramS$PMChigh[paramS$Label=="spec"] <- 0.99
  paramS$PMChigh[paramS$Label=="e"] <- 4.50
  
# High cost oral fluid HCVST: Double the cost of oral fluid-based HCVST to $11.25 compared to $5.63 in the base case;
  paramS$OralHigh[paramS$Label=="v"] <- 0
  paramS$OralHigh[paramS$Label=="f"] <- paramS$OralHigh[paramS$Label=="f1"] #RDT standard  
  paramS$OralHigh[paramS$Label=="e"] <- 11.25
  
  # Equal cost HCVST: Set the cost of HCVST including distribution costs to be equal to the standard of care RDT test cost in each setting;
  
  paramS$EqualST[paramS$Label=="v"] <- 0
  paramS$EqualST[paramS$Label=="f"] <- paramS$EqualST[paramS$Label=="f1"] #RDT standard  
  paramS$EqualST[paramS$Label=="e"] <- paramS$EqualST[paramS$Label=="f1"]
  paramS$EqualST[paramS$Label=="z"] <- 0
  
  
# Low HCVST performance: 
  paramS$LowAcc[paramS$Label=="v"] <- 0
  paramS$LowAcc[paramS$Label=="f"] <- paramS$LowAcc[paramS$Label=="f1"] #RDT standard 
  paramS$LowAcc[paramS$Label=="sens"] <- 0.9
  paramS$LowAcc[paramS$Label=="spec"] <- 0.97
  
# High inter-reader agreement
  paramS$HighIRA[paramS$Label=="v"] <- 0
  paramS$HighIRA[paramS$Label=="f"] <- paramS$HighIRA[paramS$Label=="f1"] #RDT standard 
  paramS$HighIRA[paramS$Label=="x"] <- 1
  
# High linkage: Assume 80% of positive self-tests link to facility-based testing compared to 65% in the base case.
  paramS$HighLink[paramS$Label=="v"] <- 0
  paramS$HighLink[paramS$Label=="f"] <- paramS$HighLink[paramS$Label=="f1"] #RDT standard 
  paramS$HighLink[paramS$Label=="z1"] <- 0.80
  paramS$HighLink[paramS$Label=="z3"] <- 0.80
  
# Low linkage: 50%
  paramS$LowLink[paramS$Label=="v"] <- 0
  paramS$LowLink[paramS$Label=="f"] <- paramS$LowLink[paramS$Label=="f1"] #RDT standard 
  paramS$LowLink[paramS$Label=="z1"] <- 0.50
  paramS$LowLink[paramS$Label=="z3"] <- 0.50
  
# High HCVST uptake
  paramS$HighUp[paramS$Label=="v"] <- 0
  paramS$HighUp[paramS$Label=="f"] <- paramS$HighUp[paramS$Label=="f1"] #RDT standard 
  paramS$HighUp[paramS$Label=="n"] <-  paramS$HighUp[paramS$Label=="highup"]
  
  # Low HCVST uptake 
  paramS$LowUp[paramS$Label=="v"] <- 0
  paramS$LowUp[paramS$Label=="f"] <- paramS$LowUp[paramS$Label=="f1"] #RDT standard 
  paramS$LowUp[paramS$Label=="n"] <-  paramS$LowUp[paramS$Label=="lowup"]
  
# High substitution: Vary the proportion of those using self-testing instead of facility-based testing to be 20% or 5% instead of 10% in the base case, while keeping the overall increase in overall testing at 62%.
  paramS$HighSub[paramS$Label=="v"] <- 0
  paramS$HighSub[paramS$Label=="f"] <- paramS$HighSub[paramS$Label=="f1"] #RDT standard 
  # paramS$HighSub[paramS$Label=="n"] <-  paramS$HighSub[paramS$Label=="highsub"]
  paramS$HighSub[paramS$Label=="o"] <- 0.2

    # Low substitution  
  paramS$LowSub[paramS$Label=="v"] <- 0
  paramS$LowSub[paramS$Label=="f"] <- paramS$LowSub[paramS$Label=="f1"] #RDT standard 
  # paramS$LowSub[paramS$Label=="n"] <-  paramS$LowSub[paramS$Label=="lowsub"]
  paramS$LowSub[paramS$Label=="o"] <- 0.05
    
# High self-test failure: Vary the proportion of invalid self-test results to be 5% or 1% compared to 3% in the base case.
  paramS$HighFail[paramS$Label=="v"] <- 0
  paramS$HighFail[paramS$Label=="f"] <- paramS$HighFail[paramS$Label=="f1"] #RDT standard 
  paramS$HighFail[paramS$Label=="w"] <- 0.95 
  
  # Low failure
  paramS$LowFail[paramS$Label=="v"] <- 0
  paramS$LowFail[paramS$Label=="f"] <- paramS$LowFail[paramS$Label=="f1"] #RDT standard 
  paramS$LowFail[paramS$Label=="w"] <- 0.99

  
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
results.CEd$costpertest <- with(results.CEd,totalcost_xT.d/all.tested.d)

# percent change from base case in numbers tested, diagnosed, and cured
results.CEp <- results.CEd %>% select(setting,scenario.x)
results.CEp$all.tested.p <- results.CEd$all.tested.d / results.CE$all.tested.y *100
results.CEp$diagnosed.p <- results.CEd$diagnosed.d / results.CE$diagnosed.y *100
results.CEp$cured.p <- results.CEd$cured.d / results.CE$cured.y *100

results.CEp %>% filter(scenario.x %in% c("BaseCase")) #,"HighUp","LowUp","HighSub","LowSub"))
results.CEd %>% filter(scenario.x %in% c("BaseCase")) 

## Just for EIA scenario
results.cfEIA <- results.df %>% filter(scenario=="NoST.EIA") %>% select(setting,scenario,all.tested,diagnosed,cured,totalcost,totalcost_xT)
results.CE.EIA <- results.df %>% filter(scenario=="EIA") %>% select(setting,scenario,totalcost,totalcost_xT,all.tested,diagnosed,cured)
results.CE.EIA <- left_join(results.CE.EIA,results.cfEIA,by="setting")
results.CE.EIAd <- results.CE.EIA %>% select(setting,scenario.x,scenario.y)
results.CE.EIAd$totalcost.d <- with(results.CE.EIA,totalcost.x - totalcost.y)
results.CE.EIAd$totalcost_xT.d <- with(results.CE.EIA,totalcost_xT.x - totalcost_xT.y)
results.CE.EIAd$all.tested.d <- with(results.CE.EIA,all.tested.x - all.tested.y)
results.CE.EIAd$diagnosed.d <- with(results.CE.EIA,diagnosed.x - diagnosed.y)
results.CE.EIAd$cured.d <- with(results.CE.EIA,cured.x - cured.y)
results.CE.EIAd$costperdiagnosis <- with(results.CE.EIAd,totalcost_xT.d/diagnosed.d)
results.CE.EIAd$costpercure <- with(results.CE.EIAd,totalcost.d/cured.d)


results.CEd %>% filter(scenario.x %in% c("NoST","BaseCase")) 
results.CE %>% filter(scenario.x %in% c("NoST","BaseCase")) %>% select(setting,scenario.x,totalcost_xT.x,diagnosed.x)
results.CE %>% filter(scenario.x %in% c("NoST","BaseCase")) %>% select(setting,scenario.x,totalcost.x,cured.x)


#### cascade of care
numbers$outcomef <- as.factor(numbers$outcome)
numbers$outcomef <- with(numbers, reorder(outcome, number, mean))
numbers$outcomef <- with(numbers,factor(outcomef, levels = rev(levels(outcomef))))
numbers$scenariof <- as.factor(numbers$scenario)
numbers$scenariof <- with(numbers, reorder(scenario, number, mean))
f3 <- numbers %>% filter(outcomef %in% c("all.tested","Ab.pos","diagnosed","treated","cured") & scenariof %in% c("NoST","BaseCase")) %>% ggplot(aes(x=outcomef,y=number,fill=scenariof)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~setting,scales="free") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Cascade of care") + ylab("Number of people") + scale_x_discrete(labels=c("All tested","Antibody positive","Diagnosed","Treated","Cured")) + scale_fill_discrete(labels=c("No HCVST","Base Case HCVST")) + theme(axis.text.x=element_text(angle=30, vjust=.8, hjust=0.8)) 

pdf("fig3.pdf",width=7,height=7)
f3
dev.off()

#### sensitivity analysis on numbers diagnosed
numbers$group <- "Sensitivity analysis"
numbers$group[numbers$scenario=="BaseCase"] <- "Base Case"
numbers$group[numbers$scenario %in% c("NoST","NoST.EIA")] <- "No HCVST"
numbers$group2 <- "RDT standard of care"
numbers$group2[numbers$scenario %in% c("EIA","NoST.EIA")] <- "EIA standard of care"
# numbers %>% filter(outcomef %in% c("diagnosed","treated")) %>% ggplot(aes(x=scenariof,y=number,fill=group)) + geom_bar(position="dodge",stat="identity") + facet_grid(setting~outcomef,scales="free") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Modelled Scenario") + ylab("Number of people diagnosed") 
f4 <- numbers %>% filter(outcomef %in% c("diagnosed")) %>% ggplot(aes(x=scenariof,y=number,fill=group,color=group2)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~setting,scales="free_x") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Modelled Scenario") + ylab("Number of people diagnosed")   + coord_flip() + scale_color_manual(values=c("black",NA),guide=FALSE)  + scale_x_discrete(labels=scen_names)



pdf("fig4.pdf",width=7,height=7)
f4
dev.off()

#### sensitivity analysis on total cost
costs$group <- "Sensitivity analysis"
costs$group[costs$scenario=="BaseCase"] <- "Base Case"
costs$group[costs$scenario %in% c("NoST","NoST.EIA")] <- "No HCVST"
costs$group2 <- "RDT standard of care"
costs$group2[costs$scenario %in% c("EIA","NoST.EIA")] <- "EIA standard of care"
costs$scenariof <- as.factor(costs$scenario)
costs$scenariof <- with(costs, reorder(scenario, cost, mean))
f5a <- costs %>% filter(outcome %in% c("totalcost_xT")) %>% ggplot(aes(x=scenariof,y=cost/1000,fill=group,color=group2)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~setting,scales="free") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Modelled Scenario") + ylab("Total cost (Thousands of dollars)") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_x_discrete(labels=scen_names)  + coord_flip() + scale_color_manual(values=c("black",NA),guide=FALSE)
# including treatment costs
# f5b <- costs %>% filter(outcome %in% c("totalcost")) %>% ggplot(aes(x=scenariof,y=cost/1000,fill=group)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~setting,scales="free") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Modelled Scenario") + ylab("Total cost (Thousands of dollars)") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# present cost per diagnosis (not incremental)
costs2 <- results.CE %>% select(setting,scenario=scenario.x,totalcost_xT.x,diagnosed.x,all.tested.x)
costs2$costperdiagnosis <- with(costs2,totalcost_xT.x/ diagnosed.x)
costs2$costpertest <- with(costs2,totalcost_xT.x/ all.tested.x)
costs2$group <- "Sensitivity analysis"
costs2$group[costs2$scenario=="BaseCase"] <- "Base Case"
costs2$group[costs2$scenario %in% c("NoST","NoST.EIA")] <- "No HCVST"
costs2$group2 <- "RDT standard of care"
costs2$group2[costs2$scenario %in% c("EIA","NoST.EIA")] <- "EIA standard of care"
# costs2 <- costs2 %>% group_by(scenario) %>% arrange(costperdiagnosis)
costs2$scenariof <- as.factor(costs2$scenario)
costs2$scenariof <- with(costs2, reorder(scenario, costperdiagnosis, mean))

f5c <- costs2 %>% ggplot(aes(x=scenariof,y=costperdiagnosis,fill=group,color=group2)) + geom_bar(position="dodge",stat="identity") + facet_wrap(~setting,scales="free_x") + theme_classic() + theme(legend.title = element_blank(),legend.position="bottom") + xlab("Modelled Scenario") + ylab("Cost per diagnosis (2019 USD)") + scale_x_discrete(labels=scen_names)  + coord_flip() + scale_color_manual(values=c("black",NA),guide=FALSE)

pdf("fig5.pdf",width=7,height=7)
# f5a
f5c
dev.off()

#### Tornado plots

icer <- results.CEd %>% filter(!scenario.x %in% c("EIA","NoST.EIA")) %>% bind_rows(results.CE.EIAd) %>% select(setting,scenario.x,costperdiagnosis,costpercure)
bc <- icer %>% filter(scenario.x=="BaseCase") 
icer <- left_join(icer,bc,by="setting")
icer$diffDiagnosis <- icer$costperdiagnosis.x - icer$costperdiagnosis.y
icer$diffCure <- icer$costpercure.x - icer$costpercure.y
icerD <- icer %>% arrange(scenario.x.x, desc(abs(diffDiagnosis)))
icerC <- icer %>% arrange(setting, desc(abs(diffCure))) 

# why can't I re-order them??
f6a <- icerD %>% mutate(scenario.x.x = fct_inorder(scenario.x.x)) %>% filter(!scenario.x.x %in% c("BaseCase","NoST")) %>% ggplot() + geom_segment(aes(x=scenario.x.x,xend=scenario.x.x,yend=costperdiagnosis.y,y=costperdiagnosis.x,size=2))  + coord_flip() + facet_wrap(~setting,scales="free_x") + theme_classic()  + geom_hline(data=bc,aes(yintercept=costperdiagnosis)) + theme(legend.position="none")   + xlab("Scenario") +ylab("Incremental cost per diagnosis (USD)") + scale_x_discrete(labels=scen_names) # + geom_hline(yintercept=0,linetype="dotted")

f6b <- icerC %>% filter(!scenario.x.x %in% c("BaseCase","NoST")) %>% ggplot() + geom_segment(aes(x=scenario.x.x,xend=scenario.x.x,y=costpercure.y,yend=costpercure.x,size=2))  + coord_flip() + facet_wrap(~setting,scales="free_x") + theme_classic()  + geom_hline(data=bc,aes(yintercept=costpercure)) + theme(legend.position="none")   + xlab("Scenario") +ylab("Incremental cost per cure (USD)") + scale_x_discrete(labels=scen_names) # + geom_hline(yintercept=0,linetype="dotted")

pdf("fig6.pdf",width=9,height=7)
f6a
dev.off()

pdf("fig7.pdf",width=8,height=7)
f6b
dev.off()

