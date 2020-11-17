# read in model structure and define parameters
setwd("~/git/HCVST/")
library(tidyverse)
library(readxl)
library(xlsx)
library(CEdecisiontree)
library(matrixcalc)

struc1 <- read_excel("diagram_input1.xlsx",sheet="Self-report") # 1. self report
struc2 <- read_excel("diagram_input1.xlsx",sheet="Peer-led") # 2. peer led
struc1names <- data.frame(name=struc1$Name,number=struc1$Number)
struc2names <- data.frame(name=struc2$Name,number=struc2$Number)

# convert input into long list 

struc1.long <- struc1 %>% pivot_longer(names_to="Label",values_to="NumberTo",contains("Child")) %>% rename("NumberFrom"="Number","NameFrom"="Name") %>% mutate(NameTo=NA) %>% filter(!is.na(NumberTo) | Endpoint==1)
struc2.long <- struc2 %>% pivot_longer(names_to="Label",values_to="NumberTo",contains("Child")) %>% rename("NumberFrom"="Number","NameFrom"="Name") %>% mutate(NameTo=NA) %>% filter(!is.na(NumberTo) | Endpoint==1)

for(i in 1:nrow(struc2.long)) {
  
  if(i <= nrow(struc1.long)) {
    if(struc1.long$Endpoint[i]==0) {
      struc1.long$NameTo[i] <- struc1names$name[struc1names$number==struc1.long$NumberTo[i]]
    }
  }
  if(struc2.long$Endpoint[i]==0) {
    struc2.long$NameTo[i] <- struc2names$name[struc2names$number==struc2.long$NumberTo[i]]
  }
}

# save in Excel 
write.xlsx(struc1.long,"diagram_long.xlsx",sheetName = "Self-report long")
write.xlsx(struc2.long,"diagram_long.xlsx",sheetName = "Peer-led long",append=TRUE)

## read in parameters

param_in <- read_excel("parameters.xlsx",sheet="Parameters")

assignSelfReport <- function(param,s1,rnd) { # s is struc1.long

  val <- param$Value
  names(val) <- param$Label
  
  s1$Probs <- NA  
  s1$Cost <- NA # need NA for NA cells and 0 in no cost cells
  
  s1$Probs[s1$NumberFrom==1 & s1$NumberTo==17] <- (1-val["m"]-val["o"])#*val["a"]*val["c"]
  s1$Probs[s1$NumberFrom==1 & s1$NumberTo==20] <- (val["m"]*val["n"]+val["o"])#*val["a"]*val["c"]
  s1$Probs[s1$NumberFrom==1 & s1$NumberTo==2] <- (val["m"]-val["m"]*val["n"])#*val["a"]*val["c"]
  s1$Probs[s1$NumberFrom==2 & s1$NumberTo==15] <- 1-val["b"]
  s1$Probs[s1$NumberFrom==2 & s1$NumberTo==3] <- val["b"]
  s1$Probs[s1$NumberFrom==3 & s1$NumberTo==14] <- 1-val["p"]
  s1$Probs[s1$NumberFrom==3 & s1$NumberTo==4] <- val["p"]
  s1$Probs[s1$NumberFrom==4 & s1$NumberTo==13] <- 1-val["d"]
  s1$Probs[s1$NumberFrom==4 & s1$NumberTo==5] <- val["d"]
  s1$Probs[s1$NumberFrom==5 & s1$NumberTo==12] <- 1-val["q"]
  s1$Probs[s1$NumberFrom==5 & s1$NumberTo==6] <- val["q"]
  s1$Probs[s1$NumberFrom==6 & s1$NumberTo==11] <- 1-val["r"]
  s1$Probs[s1$NumberFrom==6 & s1$NumberTo==7] <- val["r"]
  s1$Probs[s1$NumberFrom==7 & s1$NumberTo==10] <- val["t"]
  s1$Probs[s1$NumberFrom==7 & s1$NumberTo==8] <- val["s"]
  s1$Probs[s1$NumberFrom==7 & s1$NumberTo==9] <- 1-val["s"]-val["t"]
  s1$Probs[s1$NumberFrom==15 & s1$NumberTo==16] <- val["u"]
  s1$Probs[s1$NumberFrom==20 & s1$NumberTo==19] <- 1 - val["b"]*val["v"]*val["w"]*val["z1"] -val["b"]*(1-val["v"])*val["w"]*val["z1"]- (1-val["w"])*val["z3"] -(1-val["b"])*val["w"]*val["z2"]#val["b"]*val["w"]*(1-val["z1"]) + (1-val["b"])*val["w"]*(1-val["z2"]) + (1-val["w"])*(1-val["z3"])  
  s1$Probs[s1$NumberFrom==20 & s1$NumberTo==21] <- val["b"]*val["v"]*val["w"]*val["z1"]
  s1$Probs[s1$NumberFrom==20 & s1$NumberTo==33] <- val["b"]*(1-val["v"])*val["w"]*val["z1"]
  s1$Probs[s1$NumberFrom==20 & s1$NumberTo==52] <- (1-val["w"])*val["z3"]
  s1$Probs[s1$NumberFrom==20 & s1$NumberTo==50] <- (1-val["b"])*val["w"]*val["z2"]
  s1$Probs[s1$NumberFrom==21 & s1$NumberTo==22] <- 1-val["p"]
  s1$Probs[s1$NumberFrom==21 & s1$NumberTo==23] <- val["p"]
  s1$Probs[s1$NumberFrom==23 & s1$NumberTo==24] <- val["d"]
  s1$Probs[s1$NumberFrom==23 & s1$NumberTo==32] <- 1-val["d"]
  s1$Probs[s1$NumberFrom==24 & s1$NumberTo==25] <- val["q"]
  s1$Probs[s1$NumberFrom==24 & s1$NumberTo==31] <- 1-val["q"]
  s1$Probs[s1$NumberFrom==25 & s1$NumberTo==26] <- val["r"]
  s1$Probs[s1$NumberFrom==25 & s1$NumberTo==30] <- 1-val["r"]
  s1$Probs[s1$NumberFrom==26 & s1$NumberTo==27] <- val["s"]
  s1$Probs[s1$NumberFrom==26 & s1$NumberTo==28] <- 1-val["s"]-val["t"]
  s1$Probs[s1$NumberFrom==26 & s1$NumberTo==29] <- val["t"]
  s1$Probs[s1$NumberFrom==33 & s1$NumberTo==34] <- 1-val["y"]
  s1$Probs[s1$NumberFrom==33 & s1$NumberTo==35] <- val["y"]
  s1$Probs[s1$NumberFrom==35 & s1$NumberTo==36] <- val["x"]
  s1$Probs[s1$NumberFrom==35 & s1$NumberTo==48] <- 1-val["x"]
  s1$Probs[s1$NumberFrom==36 & s1$NumberTo==37] <- val["p"]
  s1$Probs[s1$NumberFrom==36 & s1$NumberTo==47] <- 1-val["p"]
  s1$Probs[s1$NumberFrom==37 & s1$NumberTo==38] <- val["d"]
  s1$Probs[s1$NumberFrom==37 & s1$NumberTo==46] <- 1-val["d"]
  s1$Probs[s1$NumberFrom==38 & s1$NumberTo==39] <- val["q"]
  s1$Probs[s1$NumberFrom==38 & s1$NumberTo==45] <- 1-val["q"]
  s1$Probs[s1$NumberFrom==39 & s1$NumberTo==40] <- val["r"]
  s1$Probs[s1$NumberFrom==39 & s1$NumberTo==44] <- 1-val["r"]
  s1$Probs[s1$NumberFrom==40 & s1$NumberTo==41] <- val["s"]
  s1$Probs[s1$NumberFrom==40 & s1$NumberTo==42] <- 1-val["s"]-val["t"]
  s1$Probs[s1$NumberFrom==40 & s1$NumberTo==43] <- val["t"]
  s1$Probs[s1$NumberFrom==48 & s1$NumberTo==49] <- val["u"]
  s1$Probs[s1$NumberFrom==50 & s1$NumberTo==51] <- val["u"]
  s1$Probs[s1$NumberFrom==52 & s1$NumberTo==53] <- 1-val["y"]
  s1$Probs[s1$NumberFrom==52 & s1$NumberTo==54] <- val["y"]
  s1$Probs[s1$NumberFrom==54 & s1$NumberTo==55] <- val["b"]
  s1$Probs[s1$NumberFrom==54 & s1$NumberTo==67] <- 1-val["b"]
  s1$Probs[s1$NumberFrom==55 & s1$NumberTo==56] <- val["p"]
  s1$Probs[s1$NumberFrom==55 & s1$NumberTo==66] <- 1-val["p"]
  s1$Probs[s1$NumberFrom==56 & s1$NumberTo==57] <- val["d"]
  s1$Probs[s1$NumberFrom==56 & s1$NumberTo==65] <- 1-val["d"]
  s1$Probs[s1$NumberFrom==57 & s1$NumberTo==58] <- val["q"]
  s1$Probs[s1$NumberFrom==57 & s1$NumberTo==64] <- 1-val["q"]
  s1$Probs[s1$NumberFrom==58 & s1$NumberTo==59] <- val["r"]
  s1$Probs[s1$NumberFrom==58 & s1$NumberTo==63] <- 1-val["r"]
  s1$Probs[s1$NumberFrom==59 & s1$NumberTo==60] <- val["s"]
  s1$Probs[s1$NumberFrom==59 & s1$NumberTo==61] <- 1-val["s"]-val["t"]
  s1$Probs[s1$NumberFrom==59 & s1$NumberTo==62] <- val["t"]
  s1$Probs[s1$NumberFrom==67 & s1$NumberTo==68] <- val["u"]  

  
  s1$Probs <- round(s1$Probs,rnd)
  
  s1$Cost[s1$NumberFrom==1 & s1$NumberTo==2]<- val["f"]
  s1$Cost[s1$NumberFrom==1 & s1$NumberTo==17] <- 0 
  s1$Cost[s1$NumberFrom==1 & s1$NumberTo==20] <- val["e"] + val["z"]
  s1$Cost[s1$NumberFrom==2 & s1$NumberTo==3] <- val["k"] 
  s1$Cost[s1$NumberFrom==2 & s1$NumberTo==15] <- val["j"] 
  s1$Cost[s1$NumberFrom==3 & s1$NumberTo==4] <- val["g"] 
  s1$Cost[s1$NumberFrom==3 & s1$NumberTo==14] <- 0 
  s1$Cost[s1$NumberFrom==4 & s1$NumberTo==5] <- val["k"] 
  s1$Cost[s1$NumberFrom==4 & s1$NumberTo==13] <- val["j"] 
  s1$Cost[s1$NumberFrom==5 & s1$NumberTo==6] <- val["l"] 
  s1$Cost[s1$NumberFrom==5 & s1$NumberTo==12] <- 0 
  s1$Cost[s1$NumberFrom==6 & s1$NumberTo==7] <- val["h"] 
  s1$Cost[s1$NumberFrom==6 & s1$NumberTo==11] <- 0 
  s1$Cost[s1$NumberFrom==7 & s1$NumberTo==8] <- val["g"] 
  s1$Cost[s1$NumberFrom==7 & s1$NumberTo==9] <- val["g"] 
  s1$Cost[s1$NumberFrom==7 & s1$NumberTo==10] <- 0 
  s1$Cost[s1$NumberFrom==15 & s1$NumberTo==16] <- 0 
  s1$Cost[s1$NumberFrom==20 & s1$NumberTo==19] <- 0 
  s1$Cost[s1$NumberFrom==20 & s1$NumberTo==21] <- val["k"]+val["i"] 
  s1$Cost[s1$NumberFrom==20 & s1$NumberTo==33] <- val["i"] 
  s1$Cost[s1$NumberFrom==20 & s1$NumberTo==50] <- val["j"] +val["i"]
  s1$Cost[s1$NumberFrom==20 & s1$NumberTo==52] <- val["i"]
  s1$Cost[s1$NumberFrom==21 & s1$NumberTo==22] <- 0 
  s1$Cost[s1$NumberFrom==21 & s1$NumberTo==23] <- val["g"] 
  s1$Cost[s1$NumberFrom==23 & s1$NumberTo==24] <- val["k"] 
  s1$Cost[s1$NumberFrom==23 & s1$NumberTo==32] <- val["j"] 
  s1$Cost[s1$NumberFrom==24 & s1$NumberTo==25] <- val["l"] 
  s1$Cost[s1$NumberFrom==24 & s1$NumberTo==31] <- 0 
  s1$Cost[s1$NumberFrom==25 & s1$NumberTo==26] <- val["h"] 
  s1$Cost[s1$NumberFrom==25 & s1$NumberTo==30] <- 0 
  s1$Cost[s1$NumberFrom==26 & s1$NumberTo==27] <- val["g"] 
  s1$Cost[s1$NumberFrom==26 & s1$NumberTo==28] <- val["g"] 
  s1$Cost[s1$NumberFrom==26 & s1$NumberTo==29] <- 0 
  s1$Cost[s1$NumberFrom==33 & s1$NumberTo==34] <- 0 
  s1$Cost[s1$NumberFrom==33 & s1$NumberTo==35] <- val["f"] 
  s1$Cost[s1$NumberFrom==35 & s1$NumberTo==36] <- val["k"] 
  s1$Cost[s1$NumberFrom==35 & s1$NumberTo==48] <- val["j"] 
  s1$Cost[s1$NumberFrom==36 & s1$NumberTo==37] <- val["g"] 
  s1$Cost[s1$NumberFrom==36 & s1$NumberTo==47] <- 0 
  s1$Cost[s1$NumberFrom==37 & s1$NumberTo==38] <- val["k"] 
  s1$Cost[s1$NumberFrom==37 & s1$NumberTo==46] <- val["j"] 
  s1$Cost[s1$NumberFrom==38 & s1$NumberTo==39] <- val["l"] 
  s1$Cost[s1$NumberFrom==38 & s1$NumberTo==45] <- 0 
  s1$Cost[s1$NumberFrom==39 & s1$NumberTo==40] <- val["h"] 
  s1$Cost[s1$NumberFrom==39 & s1$NumberTo==44] <- 0 
  s1$Cost[s1$NumberFrom==40 & s1$NumberTo==41] <- val["g"] 
  s1$Cost[s1$NumberFrom==40 & s1$NumberTo==42] <- val["g"] 
  s1$Cost[s1$NumberFrom==40 & s1$NumberTo==43] <- 0 
  s1$Cost[s1$NumberFrom==48 & s1$NumberTo==49] <- 0 
  s1$Cost[s1$NumberFrom==50 & s1$NumberTo==51] <- 0 
  s1$Cost[s1$NumberFrom==52 & s1$NumberTo==53] <- 0 
  s1$Cost[s1$NumberFrom==52 & s1$NumberTo==54] <- val["f"] 
  s1$Cost[s1$NumberFrom==54 & s1$NumberTo==55] <- val["k"] 
  s1$Cost[s1$NumberFrom==54 & s1$NumberTo==67] <- val["j"] 
  s1$Cost[s1$NumberFrom==55 & s1$NumberTo==56] <- val["g"] 
  s1$Cost[s1$NumberFrom==55 & s1$NumberTo==66] <- 0 
  s1$Cost[s1$NumberFrom==56 & s1$NumberTo==57] <- val["k"] 
  s1$Cost[s1$NumberFrom==56 & s1$NumberTo==65] <- val["j"] 
  s1$Cost[s1$NumberFrom==57 & s1$NumberTo==58] <- val["l"] 
  s1$Cost[s1$NumberFrom==57 & s1$NumberTo==64] <- 0 
  s1$Cost[s1$NumberFrom==58 & s1$NumberTo==59] <- val["h"] 
  s1$Cost[s1$NumberFrom==58 & s1$NumberTo==63] <- 0 
  s1$Cost[s1$NumberFrom==59 & s1$NumberTo==60] <- val["g"] 
  s1$Cost[s1$NumberFrom==59 & s1$NumberTo==61] <- val["g"] 
  s1$Cost[s1$NumberFrom==59 & s1$NumberTo==62] <- 0 
  s1$Cost[s1$NumberFrom==67 & s1$NumberTo==68] <- 0 

  return(s1)
}


# ## something wrong here with row sums
# assignPeerLed <- function(param,s2) { # s is struc2.long
#   
#   val <- param$Value
#   names(val) <- param$Label
#   
#   s2$Probs <- NA  
#   s2$Cost <- NA # need NA for NA cells and 0 in no cost cells
#   
#   s2$Probs[s2$NumberFrom==1 & s2$NumberTo==2] <- (val["m"]-val["m"]*val["n"])#*val["a"]*val["c"]
#   s2$Probs[s2$NumberFrom==1 & s2$NumberTo==17] <- (1-val["m"]-val["o"])#*val["a"]*val["c"]
#   s2$Probs[s2$NumberFrom==1 & s2$NumberTo==18] <- (val["m"]*val["n"]+val["o"])#*val["a"]*val["c"]
#   s2$Probs[s2$NumberFrom==2  & s2$NumberTo==3] <- val["b"]
#   s2$Probs[s2$NumberFrom==2  & s2$NumberTo==15] <- 1-val["b"]
#   s2$Probs[s2$NumberFrom==3  & s2$NumberTo==4] <- val["p"]
#   s2$Probs[s2$NumberFrom==3  & s2$NumberTo==14] <- 1-val["p"]
#   s2$Probs[s2$NumberFrom==4  & s2$NumberTo==5] <- val["d"]
#   s2$Probs[s2$NumberFrom==4  & s2$NumberTo==13] <- 1-val["d"]
#   s2$Probs[s2$NumberFrom==5  & s2$NumberTo==6] <- val["q"]
#   s2$Probs[s2$NumberFrom==5  & s2$NumberTo==12] <- 1-val["q"]
#   s2$Probs[s2$NumberFrom==6  & s2$NumberTo==7] <- val["r"]
#   s2$Probs[s2$NumberFrom==6  & s2$NumberTo==11] <- 1-val["r"]
#   s2$Probs[s2$NumberFrom==7  & s2$NumberTo==8] <- val["s"]
#   s2$Probs[s2$NumberFrom==7  & s2$NumberTo==9] <- 1-val["s"]-val["t"]
#   s2$Probs[s2$NumberFrom==7  & s2$NumberTo==10] <- val["t"]
#   s2$Probs[s2$NumberFrom==15 & s2$NumberTo==16] <- val["u"]
#   s2$Probs[s2$NumberFrom==18 & s2$NumberTo==19] <- 1-val["z"]
#   s2$Probs[s2$NumberFrom==18 & s2$NumberTo==20] <- val["b"]*val["v"]*val["w"]
#   s2$Probs[s2$NumberFrom==18 & s2$NumberTo==33] <- val["b"]*(1-val["v"])*val["w"]
#   s2$Probs[s2$NumberFrom==18 & s2$NumberTo==52] <- (1-val["b"])*val["w"]
#   s2$Probs[s2$NumberFrom==18 & s2$NumberTo==54] <- 1-val["w"]
#   s2$Probs[s2$NumberFrom==20 & s2$NumberTo==21] <- val["z1"]
#   s2$Probs[s2$NumberFrom==20 & s2$NumberTo==32] <- 1-val["z1"]
#   s2$Probs[s2$NumberFrom==21 & s2$NumberTo==22] <- val["p"]
#   s2$Probs[s2$NumberFrom==21 & s2$NumberTo==73] <- 1-val["p"]
#   s2$Probs[s2$NumberFrom==22 & s2$NumberTo==23] <- val["d"]
#   s2$Probs[s2$NumberFrom==22 & s2$NumberTo==31] <- 1-val["d"]
#   s2$Probs[s2$NumberFrom==23 & s2$NumberTo==24] <- val["q"]
#   s2$Probs[s2$NumberFrom==23 & s2$NumberTo==30] <- 1-val["q"]
#   s2$Probs[s2$NumberFrom==24 & s2$NumberTo==25] <- val["r"]
#   s2$Probs[s2$NumberFrom==24 & s2$NumberTo==29] <- 1-val["r"]
#   s2$Probs[s2$NumberFrom==25 & s2$NumberTo==26] <- val["s"]
#   s2$Probs[s2$NumberFrom==25 & s2$NumberTo==27] <- 1-val["s"]-val["t"]
#   s2$Probs[s2$NumberFrom==25 & s2$NumberTo==28] <- val["t"]
#   s2$Probs[s2$NumberFrom==33 & s2$NumberTo==34] <- val["z2"]
#   s2$Probs[s2$NumberFrom==33 & s2$NumberTo==51] <- 1-val["z2"]
#   s2$Probs[s2$NumberFrom==34 & s2$NumberTo==35] <- 1-val["y"]
#   s2$Probs[s2$NumberFrom==34 & s2$NumberTo==36] <- val["y"]
#   s2$Probs[s2$NumberFrom==36 & s2$NumberTo==37] <- val["x"]
#   s2$Probs[s2$NumberFrom==36 & s2$NumberTo==49] <- 1-val["x"]
#   s2$Probs[s2$NumberFrom==37 & s2$NumberTo==38] <- val["p"]
#   s2$Probs[s2$NumberFrom==37 & s2$NumberTo==48] <- 1-val["p"]
#   s2$Probs[s2$NumberFrom==38 & s2$NumberTo==39] <- val["d"]
#   s2$Probs[s2$NumberFrom==38 & s2$NumberTo==47] <- 1-val["d"]
#   s2$Probs[s2$NumberFrom==39 & s2$NumberTo==40] <- val["q"]
#   s2$Probs[s2$NumberFrom==39 & s2$NumberTo==46] <- 1-val["q"]
#   s2$Probs[s2$NumberFrom==40 & s2$NumberTo==41] <- val["r"]
#   s2$Probs[s2$NumberFrom==40 & s2$NumberTo==45] <- 1-val["r"]
#   s2$Probs[s2$NumberFrom==41 & s2$NumberTo==42] <- val["s"]
#   s2$Probs[s2$NumberFrom==41 & s2$NumberTo==43] <- 1-val["s"]-val["t"]
#   s2$Probs[s2$NumberFrom==41 & s2$NumberTo==44] <- val["t"]
#   s2$Probs[s2$NumberFrom==49 & s2$NumberTo==50] <- val["u"]
#   s2$Probs[s2$NumberFrom==52 & s2$NumberTo==53] <- val["u"]
#   s2$Probs[s2$NumberFrom==54 & s2$NumberTo==55] <- val["z3"]
#   s2$Probs[s2$NumberFrom==54 & s2$NumberTo==72] <- 1-val["z3"]
#   s2$Probs[s2$NumberFrom==55 & s2$NumberTo==56] <- 1-val["y"]
#   s2$Probs[s2$NumberFrom==55 & s2$NumberTo==57] <- val["y"]
#   s2$Probs[s2$NumberFrom==57 & s2$NumberTo==58] <- val["b"]
#   s2$Probs[s2$NumberFrom==57 & s2$NumberTo==70] <- 1-val["b"]
#   s2$Probs[s2$NumberFrom==58 & s2$NumberTo==59] <- val["p"]
#   s2$Probs[s2$NumberFrom==58 & s2$NumberTo==69] <- 1-val["p"]
#   s2$Probs[s2$NumberFrom==59 & s2$NumberTo==60] <- val["d"]
#   s2$Probs[s2$NumberFrom==59 & s2$NumberTo==68] <- 1-val["d"]
#   s2$Probs[s2$NumberFrom==60 & s2$NumberTo==61] <- val["q"]
#   s2$Probs[s2$NumberFrom==60 & s2$NumberTo==67] <- 1-val["q"]
#   s2$Probs[s2$NumberFrom==61 & s2$NumberTo==62] <- val["r"]
#   s2$Probs[s2$NumberFrom==61 & s2$NumberTo==66] <- 1-val["r"]
#   s2$Probs[s2$NumberFrom==62 & s2$NumberTo==63] <- val["s"]
#   s2$Probs[s2$NumberFrom==62 & s2$NumberTo==64] <- 1-val["s"]-val["t"]
#   s2$Probs[s2$NumberFrom==62 & s2$NumberTo==65] <- val["t"]
#   s2$Probs[s2$NumberFrom==70 & s2$NumberTo==71] <- val["u"]  
#   
#   s2$Probs <- round(s2$Probs,8)
#     
#   s2$Cost[s2$NumberFrom==1 & s2$NumberTo==2] <- val["f"]
#   s2$Cost[s2$NumberFrom==1 & s2$NumberTo==17] <- 0
#   s2$Cost[s2$NumberFrom==1 & s2$NumberTo==18] <- val["e"]
#   s2$Cost[s2$NumberFrom==2  & s2$NumberTo==3] <- val["k"]
#   s2$Cost[s2$NumberFrom==2  & s2$NumberTo==15] <- val["j"]
#   s2$Cost[s2$NumberFrom==3  & s2$NumberTo==4] <- val["g"]
#   s2$Cost[s2$NumberFrom==3  & s2$NumberTo==14] <- 0
#   s2$Cost[s2$NumberFrom==4  & s2$NumberTo==5] <- val["k"]
#   s2$Cost[s2$NumberFrom==4  & s2$NumberTo==13] <- val["j"]
#   s2$Cost[s2$NumberFrom==5  & s2$NumberTo==6] <- val["l"]
#   s2$Cost[s2$NumberFrom==5  & s2$NumberTo==12] <- 0
#   s2$Cost[s2$NumberFrom==6  & s2$NumberTo==7] <- val["h"]
#   s2$Cost[s2$NumberFrom==6  & s2$NumberTo==11] <- 0
#   s2$Cost[s2$NumberFrom==7  & s2$NumberTo==8] <- val["g"]
#   s2$Cost[s2$NumberFrom==7  & s2$NumberTo==9] <- val["g"]
#   s2$Cost[s2$NumberFrom==7  & s2$NumberTo==10] <- 0
#   s2$Cost[s2$NumberFrom==15 & s2$NumberTo==16] <- 0
#   s2$Cost[s2$NumberFrom==18 & s2$NumberTo==19] <- 0
#   s2$Cost[s2$NumberFrom==18 & s2$NumberTo==20] <- 0
#   s2$Cost[s2$NumberFrom==18 & s2$NumberTo==33] <- 0
#   s2$Cost[s2$NumberFrom==18 & s2$NumberTo==52] <- 0
#   s2$Cost[s2$NumberFrom==18 & s2$NumberTo==54] <- 0
#   s2$Cost[s2$NumberFrom==20 & s2$NumberTo==21] <- val["i"]
#   s2$Cost[s2$NumberFrom==20 & s2$NumberTo==32] <- 0
#   s2$Cost[s2$NumberFrom==21 & s2$NumberTo==22] <- val["g"]
#   s2$Cost[s2$NumberFrom==21 & s2$NumberTo==73] <- 0
#   s2$Cost[s2$NumberFrom==22 & s2$NumberTo==23] <- val["k"]
#   s2$Cost[s2$NumberFrom==22 & s2$NumberTo==31] <- val["j"]
#   s2$Cost[s2$NumberFrom==23 & s2$NumberTo==24] <- val["l"]
#   s2$Cost[s2$NumberFrom==23 & s2$NumberTo==30] <- 0
#   s2$Cost[s2$NumberFrom==24 & s2$NumberTo==25] <- val["h"]
#   s2$Cost[s2$NumberFrom==24 & s2$NumberTo==29] <- 0
#   s2$Cost[s2$NumberFrom==25 & s2$NumberTo==26] <- val["g"]
#   s2$Cost[s2$NumberFrom==25 & s2$NumberTo==27] <- val["g"]
#   s2$Cost[s2$NumberFrom==25 & s2$NumberTo==28] <- 0
#   s2$Cost[s2$NumberFrom==33 & s2$NumberTo==34] <- val["i"]
#   s2$Cost[s2$NumberFrom==33 & s2$NumberTo==51] <- 0
#   s2$Cost[s2$NumberFrom==34 & s2$NumberTo==35] <- 0
#   s2$Cost[s2$NumberFrom==34 & s2$NumberTo==36] <- val["f"]
#   s2$Cost[s2$NumberFrom==36 & s2$NumberTo==37] <- val["k"]
#   s2$Cost[s2$NumberFrom==36 & s2$NumberTo==49] <- val["j"]
#   s2$Cost[s2$NumberFrom==37 & s2$NumberTo==38] <- val["g"]
#   s2$Cost[s2$NumberFrom==37 & s2$NumberTo==48] <- 0
#   s2$Cost[s2$NumberFrom==38 & s2$NumberTo==39] <- val["k"]
#   s2$Cost[s2$NumberFrom==38 & s2$NumberTo==47] <- val["j"]
#   s2$Cost[s2$NumberFrom==39 & s2$NumberTo==40] <- val["l"]
#   s2$Cost[s2$NumberFrom==39 & s2$NumberTo==46] <- 0
#   s2$Cost[s2$NumberFrom==40 & s2$NumberTo==41] <- val["h"]
#   s2$Cost[s2$NumberFrom==40 & s2$NumberTo==45] <- 0
#   s2$Cost[s2$NumberFrom==41 & s2$NumberTo==42] <- val["g"]
#   s2$Cost[s2$NumberFrom==41 & s2$NumberTo==43] <- val["g"]
#   s2$Cost[s2$NumberFrom==41 & s2$NumberTo==44] <- 0
#   s2$Cost[s2$NumberFrom==49 & s2$NumberTo==50] <- 0
#   s2$Cost[s2$NumberFrom==52 & s2$NumberTo==53] <- 0
#   s2$Cost[s2$NumberFrom==54 & s2$NumberTo==55] <- val["i"]
#   s2$Cost[s2$NumberFrom==54 & s2$NumberTo==72] <- 0
#   s2$Cost[s2$NumberFrom==55 & s2$NumberTo==56] <- 0
#   s2$Cost[s2$NumberFrom==55 & s2$NumberTo==57] <- val["f"]
#   s2$Cost[s2$NumberFrom==57 & s2$NumberTo==58] <- val["k"]
#   s2$Cost[s2$NumberFrom==57 & s2$NumberTo==70] <- val["j"]
#   s2$Cost[s2$NumberFrom==58 & s2$NumberTo==59] <- val["g"]
#   s2$Cost[s2$NumberFrom==58 & s2$NumberTo==69] <- 0
#   s2$Cost[s2$NumberFrom==59 & s2$NumberTo==60] <- val["k"]
#   s2$Cost[s2$NumberFrom==59 & s2$NumberTo==68] <- val["j"]
#   s2$Cost[s2$NumberFrom==60 & s2$NumberTo==61] <- val["l"]
#   s2$Cost[s2$NumberFrom==60 & s2$NumberTo==67] <- 0
#   s2$Cost[s2$NumberFrom==61 & s2$NumberTo==62] <- val["h"]
#   s2$Cost[s2$NumberFrom==61 & s2$NumberTo==66] <- 0
#   s2$Cost[s2$NumberFrom==62 & s2$NumberTo==63] <- val["g"]
#   s2$Cost[s2$NumberFrom==62 & s2$NumberTo==64] <- val["g"]
#   s2$Cost[s2$NumberFrom==62 & s2$NumberTo==65] <- 0
#   s2$Cost[s2$NumberFrom==70 & s2$NumberTo==71] <- 0
# 
#   return(s2)
# }


makeMatrix <- function(s) {
  size <- max(s$NumberTo,na.rm=T)
  mP <- matrix(nrow=size,ncol=size)
  mC <- mP
  
  for(i in 1:nrow(s)) {
    mP[s$NumberFrom[i],s$NumberTo[i]] <- s$Probs[i]
    mC[s$NumberFrom[i],s$NumberTo[i]] <- s$Cost[i]
  }
  return(list(prob=mP,vals=mC))
}
# 

# # calculate cost at first
totalcosts <- function(mP,mC) {
  mt <- define_model(transmat=list(vals=mC,prob=mP))
  all <- dectree_expected_values(model=mt)
  return(all)#[1])
}
  # mC1 <- mC
  # mC1[!is.na(mC)] <- 1
  # probs <- Cdectree_expected_values(vals=mC1,p=mP) # value of 1 for each.. this should work like a QALY
  # sum(all)
  # return(all)


node_probs <- function(mat) {
  p <- mat$prob
  p[is.na(p)] <- 0
  out <- c(1,rep(0,nrow(p)-1))
  
  for(k in 1:length(out)) {
    
    pow <- matrix.power(p,k)
    vect <- colSums(pow*out)
    
    if (sum(vect)==0) {
      break
    }
    out <- out + vect
  }
  return(out)
}
  


## how many treated? diagnosed? cured? tested?
# mat <- georgia_out$trustoraltest$mat
numbers <- function(mat,str,param) {
  M <- define_model(transmat=mat)
  bp <- node_probs(M)

  cascade <- data.frame(groups = unique(str$SumGroup), p=NA)
  for(j in 1:nrow(cascade)) {
    cascade$p[j] <- sum(bp[str$Number[str$SumGroup==cascade$groups[j]]],na.rm=T)
  }
  cascade$number <- cascade$p*param$Value[param$Label=="c"]*param$Value[param$Label=="a"]

  return(cascade)
}


# #




