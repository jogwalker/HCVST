# read in model structure and define parameters

setwd("~/git/HCVST/")

library(tidyverse)
library(readxl)
library(xlsx)

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

