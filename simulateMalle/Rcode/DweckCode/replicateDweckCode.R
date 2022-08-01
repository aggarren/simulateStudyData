library(readr)
library(psych)
library(knitr)
library(zoo)
library(dplyr)
combine <- function(csv1, csv2){
  data1 <- read.csv(csv1)
  data2 <- read.csv(csv2)
  data1 <- as.matrix(data1)
  data2 <- as.matrix(data2)
  data <- cbind(data1,data2)
  data <- t(data)
  return(data)
}

getName <- function(x){
  if(x == 1){name <- "staplerCarPC1"}
  if(x == 2){name <- "staplerCarPC2"}
  if(x == 3){name <- "staplerCarPC3"}
  if(x == 4){name <- "computerRobotPC1"}
  if(x == 5){name <- "computerRobotPC2"}
  if(x == 6){name <- "computerRobotPC3"}
  if(x == 7){name <- "microbeBeetlePC1"}
  if(x == 8){name <- "microbeBeetlePC2"}
  if(x == 9){name <- "microbeBeetlePC3"}
  if(x == 10){name <- "fishBluejayPC1"}
  if(x == 11){name <- "fishBluejayPC2"}
  if(x == 12){name <- "fishBluejayPC3"}
  if(x == 13){name <- "frogMousePC1"}
  if(x == 14){name <- "frogMousePC2"}
  if(x == 15){name <- "frogMousePC3"}
  if(x == 16){name <- "goatDogPC1"}
  if(x == 17){name <- "goatDogPC2"}
  if(x == 18){name <- "goatDogPC3"}
  if(x == 19){name <- "bearDolphinPC1"}
  if(x == 20){name <- "bearDolphinPC2"}
  if(x == 21){name <- "bearDolphinPC3"}
  if(x == 22){name <- "elephantChimpPC1"}
  if(x == 23){name <- "elephantChimpPC2"}
  if(x == 24){name <- "elephantChimpPC3"}
  if(x == 25){name <- "fetusPVSPC1"}
  if(x == 26){name <- "fetusPVSPC2"}
  if(x == 27){name <- "fetusPVSPC3"}
  if(x == 28){name <- "infantPC1"}
  if(x == 29){name <- "infantPC2"}
  if(x == 30){name <- "infantPC3"}
  if(x == 31){name <- "childPC1"}
  if(x == 32){name <- "childPC2"}
  if(x == 33){name <- "childPC3"}
  if(x == 34){name <- "adultPC1"}
  if(x == 35){name <- "adultPC2"}
  if(x == 36){name <- "adultPC3"}
  return(name)
}

forCharacter <- function(data){
  # run pca with 3 dimensions with varimax rotation
  pcaRotated <- principal(data, nfactors=3,rotate="varimax")
  pcaRotatedLoadings <- as.data.frame.matrix(pcaRotated$loadings)

  # code a priori mental capacity categories
  pcaRotatedLoadings[c(38, 39, 40, 31, 33), "mc_cat"] <- "biological"
    #"hungry", "tired", "pain",  "nauseated", "safe"
  pcaRotatedLoadings[c(34, 12, 37,  29, 35, 32), "mc_cat"] <- "affective"
    #"happy", "depressed", "fear", "angry", "calm", "joy"
  pcaRotatedLoadings[c(9, 4, 8, 23, 5),"mc_cat"] <- "perceptual"
    #"sounds", "seeing", "temperature", "odors", "depth"
  pcaRotatedLoadings[c(10, 25, 13,6, 21), "mc_cat"] <- "cognitive"
    #"computations", "thoughts", "reasoning", "remembering", "beliefs"
  pcaRotatedLoadings[c(24, 3, 14, 22, 1),"mc_cat"] <- "autonomous"
    #"free_will", "choices", "self_restraint", "intentions", "goal"
  pcaRotatedLoadings[c(28, 2, 7, 20,17, 19, 15),"mc_cat"] <- "social"
    #"love", "recognizing", "communicating", "guilt", "disrespected", "embarrassed", "emo_recog"
  pcaRotatedLoadings[c(27, 11, 36,30, 18, 26, 16),"mc_cat"] <- "other"
    #"conscious", "self_aware", "pleasure", "desires", "morality", "personality", "pride"
  pcaRotatedLoadings$mc_cat <- factor(pcaRotatedLoadings$mc_cat)

  # examine loadings
  mcRotated = rownames(pcaRotatedLoadings)
  # ... for PC1
  pcaRotatedPC1 <- pcaRotatedLoadings %>%
    mutate(mc = mcRotated) %>%
    arrange(desc(RC1)) %>%
    select(RC1, mc, mc_cat)
  # ... for PC2
  pcaRotatedPC2 <- pcaRotatedLoadings %>%
    mutate(mc = mcRotated) %>%
    arrange(desc(RC2)) %>%
    select(RC2, mc, mc_cat)
  # ... for PC3
  pcaRotatedPC3 <- pcaRotatedLoadings %>%
    mutate(mc = mcRotated) %>%
    arrange(desc(RC3)) %>%
    select(RC3, mc, mc_cat)
#reduce to top 10 factor loadings by conditions and dimensions (ABS)
  pcaRotatedPC1 <- pcaRotatedPC1 %>% mutate(abs_PC1=abs(RC1)) %>% top_n(10,abs_PC1)
  pcaRotatedPC2 <- pcaRotatedPC2 %>% mutate(abs_PC2=abs(RC2)) %>% top_n(10,abs_PC2)
  pcaRotatedPC3 <- pcaRotatedPC3 %>% mutate(abs_PC3=abs(RC3)) %>% top_n(10,abs_PC3)
  PCList <- list(pcaRotatedPC1,pcaRotatedPC2,pcaRotatedPC3)
  return(PCList)
  }
#get solutions
    data <- combine("3stapler.csv","5car.csv")
    list <-forCharacter(data)
      staplerCarPC1 <- as.data.frame(list[1])
      staplerCarPC2 <- as.data.frame(list[2])
      staplerCarPC3 <- as.data.frame(list[3])
   data <- combine("7computer.csv","9robot.csv")
      list <-forCharacter(data)
      computerRobotPC1 <- as.data.frame(list[1])
      computerRobotPC2 <- as.data.frame(list[2])
      computerRobotPC3 <- as.data.frame(list[3])
  data <- combine("11microbe.csv","13beetle.csv")
      list <-forCharacter(data)
      microbeBeetlePC1 <- as.data.frame(list[1])
      microbeBeetlePC2 <- as.data.frame(list[2])
      microbeBeetlePC3 <- as.data.frame(list[3])
    data <- combine("15fish.csv","17bluejay.csv")
      list <-forCharacter(data)
      fishBluejayPC1 <- as.data.frame(list[1])
      fishBluejayPC2 <- as.data.frame(list[2])
      fishBluejayPC3 <- as.data.frame(list[3])
    data <- combine("19frog.csv","21mouse.csv")
      list <-forCharacter(data)
      frogMousePC1 <- as.data.frame(list[1])
      frogMousePC2 <- as.data.frame(list[2])
      frogMousePC3 <- as.data.frame(list[3])
    data <- combine("23goat.csv","25dog.csv")
      list <-forCharacter(data)
      goatDogPC1 <- as.data.frame(list[1])
      goatDogPC2 <- as.data.frame(list[2])
      goatDogPC3 <- as.data.frame(list[3])
    data <- combine("27bear.csv","29dolphin.csv")
      list <-forCharacter(data)
      bearDolphinPC1 <- as.data.frame(list[1])
      bearDolphinPC2 <- as.data.frame(list[2])
      bearDolphinPC3 <- as.data.frame(list[3])
    data <- combine("31elephant.csv","33chimp.csv")
      list <-forCharacter(data)
      elephantChimpPC1 <- as.data.frame(list[1])
      elephantChimpPC2 <- as.data.frame(list[2])
      elephantChimpPC3 <- as.data.frame(list[3])
    data <- combine("35fetus.csv","37pvs.csv")
      list <-forCharacter(data)
      fetusPVSPC1 <- as.data.frame(list[1])
      fetusPVSPC2 <- as.data.frame(list[2])
      fetusPVSPC3 <- as.data.frame(list[3])
    list <- forCharacter(t(read.csv("39infant.csv")))
      infantPC1 <- as.data.frame(list[1])
      infantPC2 <- as.data.frame(list[2])
      infantPC3 <- as.data.frame(list[3])
    list <- forCharacter(t(read.csv("41child.csv")))
      childPC1 <- as.data.frame(list[1])
      childPC2 <- as.data.frame(list[2])
      childPC3 <- as.data.frame(list[3])
    list <- forCharacter(t(read.csv("43adult.csv")))
      adultPC1 <- as.data.frame(list[1])
      adultPC2 <- as.data.frame(list[2])
      adultPC3 <- as.data.frame(list[3])

#compare all possible combinations

characterPCs <-  list(staplerCarPC1,staplerCarPC2,staplerCarPC3,
                      computerRobotPC1,computerRobotPC2,computerRobotPC3,
                      microbeBeetlePC1,microbeBeetlePC2,microbeBeetlePC3,
                      fishBluejayPC1,fishBluejayPC2,fishBluejayPC3,
                      frogMousePC1,frogMousePC2,frogMousePC3,
                      goatDogPC1,goatDogPC2,goatDogPC3,
                      bearDolphinPC1,bearDolphinPC2,bearDolphinPC3,
                      elephantChimpPC1,elephantChimpPC2,elephantChimpPC3,
                      fetusPVSPC1,fetusPVSPC2,fetusPVSPC3,
                      infantPC1,infantPC2,infantPC3,
                      childPC1,childPC2,childPC3,
                      adultPC1,adultPC2,adultPC3)
matchRotated <- as.data.frame(c())
for(i in 1:length(characterPCs)){
  print(i)
  for(j in i:length(characterPCs)){
    if(i != j){
      string1 <- getName(i)
      string2 <- getName(j)
      stringName <- toString(paste("match",string1,string2,sep="_"))
      newMatch <- as.data.frame(characterPCs[[i]]) %>% mutate(comparison = stringName) 
      newMatch <- filter(newMatch, characterPCs[[i]]$mc %in% characterPCs[[j]]$mc)
      names(newMatch)[names(newMatch)=='RC1'] <- 'PC'
      names(newMatch)[names(newMatch)=='RC2'] <- 'PC'
      names(newMatch)[names(newMatch)=='RC3'] <- 'PC'
      names(newMatch)[names(newMatch)=='abs_PC1'] <- 'abs_PC'
      names(newMatch)[names(newMatch)=='abs_PC2'] <- 'abs_PC'
      names(newMatch)[names(newMatch)=='abs_PC3'] <- 'abs_PC'
      if(length(t(newMatch))>0){
        print(newMatch)
        if(length(t(matchRotated))==0){
          matchRotated <- newMatch
        }
        else {
          matchRotated <- rbind(matchRotated,newMatch)
        }
      }
    }
  }
 }
topMatchRotated <- matchRotated %>%
    count(comparison) %>%
    arrange(desc(n)) %>%
    left_join(matchRotated) %>%
    select(comparison, n, mc, PC)

