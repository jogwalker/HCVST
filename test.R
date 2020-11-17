# test CEdecisiontree package

# library(devtools)
# install_github("Health-Economics-in-R/CEdecisiontree@dev")
library(CEdecisiontree)

data("cost")
data("probs")

# I need to test a simple model to understand what's going on
cost
probs
mt <- define_model(transmat=list(vals=cost,prob=probs))
sum(dectree_expected_values(model=mt))
sum(Cdectree_expected_values(vals=as.matrix(cost),p=as.matrix(probs))) # this gives a different answer.. no values for endpoints, which actually makes sense because they are back calculated to decision nodes.

## nathan says to use the dectree_expected_values version (not C) - this is the formula  p_{12} (c_{12} + p_{24}c_{24} + p_{25}c_{25}) + p_{13} (c_{13} + p_{36}c_{36} + p_{37}c_{37})
## so what I had wrong was not multiplying the first probability by EVERYTHING
# in any case it doesn't matter much because I want the total cost from node 1 for the main analysis. 



# my model
t1 <- assignSelfReport(param_in,struc1.long)
test <- makeMatrix(t1)
is_prob_matrix(test[["prob"]])
selfM <- define_model(transmat=test)
bp <- branch_joint_probs(selfM)
ct <- dectree_expected_values(model=selfM) # this one works with model input, the alternative C function works with vals and probs
ct2 <- Cdectree_expected_values(vals=test$vals,p=test$prob) # this looks completely different than the above.
sum(ct2) # this looks better
sum(ct) # i think they are cumulative.

testv <- test
testv$vals[!is.na(testv$vals)] <- 1
selfMv <- define_model(transmat=testv)

ctv2 <- Cdectree_expected_values(vals=testv$vals,p=testv$prob)
ctv1 <- dectree_expected_values(model=selfMv) # this is the same as ctv1 except 1 has been added to all values from node 3 onwards.

# so what should the value be for link to care (6)?
test$vals[6,7]*test$prob[6,7]*test$vals[7,8]*test$prob[7,8]  + test$vals[6,7]*test$prob[6,7]*test$vals[7,9]*test$prob[7,9] + test$vals[6,7]*test$prob[6,7]*test$vals[7,10]*test$prob[7,10] + test$vals[6,11]*test$prob[6,11]
# add back to 5

test$vals[5,6]*test$prob[5,6]*test$vals[6,7]*test$prob[6,7]*test$vals[7,8]*test$prob[7,8]  + test$vals[5,6]*test$prob[5,6]*test$vals[6,7]*test$prob[6,7]*test$vals[7,9]*test$prob[7,9] + test$vals[5,6]*test$prob[5,6]*test$vals[6,7]*test$prob[6,7]*test$vals[7,10]*test$prob[7,10] + test$vals[5,6]*test$prob[5,6]*test$vals[6,11]*test$prob[6,11] + test$vals[5,12]*test$prob[5,12]
# this still doesn't make sense

test$prob[5,6]*test$prob[6,7]*test$prob[7,8]  + test$prob[5,6]*test$prob[6,7]*test$prob[7,9] + test$prob[5,6]*test$prob[6,7]*test$prob[7,10] + test$prob[5,6]*test$prob[6,11] + test$prob[5,12]# these always add up to 1.


effects<-probs[1,2]*cost[1,2]*probs[2,4]*cost[2,4] + probs[1,2]*cost[1,2]*probs[2,5]*cost[2,5] +
  probs[1,3]*cost[1,3]*probs[3,6]*cost[3,6] + probs[1,3]*cost[1,3]*probs[3,7]*cost[3,7]
# effects<- probs[1,2]*probs[2,4] + probs[1,2]*probs[2,5] + probs[1,3]*probs[3,6] + probs[1,3]*probs[3,7] # sums to 1
effects<-cost[1,2] + probs[1,2]*cost[2,4]*probs[2,4] + probs[1,2]*cost[2,5]*probs[2,5] +
  probs[1,3]*cost[3,6]*probs[3,6] + probs[1,3]*cost[3,7]*probs[3,7]
# so this equals 2.8

sum(probs*cost,na.rm=T)

#t2 <- assignSelfReport(param_in,struc2.long) ## theres is a problem with struc2!!
# test2 <- makeMatrix(t2)
# costlist2 <- Cdectree_expected_values(vals=test2[["cost"]],p=test2[["probs"]])

which(struc1$Endpoint==0)[32] #15,48,50,67 are 0 <- this is correct because those are the ones that only go on to retest next year
ct2[struc1$Endpoint==0]
ct2[struc1$Endpoint==1]

ct[struc1$Endpoint==1]



