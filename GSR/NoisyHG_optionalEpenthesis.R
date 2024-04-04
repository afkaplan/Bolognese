library(tidyverse)
library(stringr)


########################Setup




#Put starting weights here
weights <- tribble(
  ~constraint, ~weight,
  "Dep", 15,
  "Max", 5,
  "SonPeriph", 37,
  "OCP", 35,
  #"DepNonPWD", 60,
  "DepMF", 35,
  "NoDupMinPPh", 34,
  "DepS1", 28,
  "SonSeq", 33,
  "OnsetPwd", 55,
  "CrispEdgePWd", 55,
  "AlignNomAL", 40,
  "NoComplicated", 15,
)


#This function calculates the winner of a tableau after harmony scores are calculated.  If there's a tie, it picks one randomly.
winner <- function(x) {
  randomChoice = sample(1:2, 1)
  x %>% 
    filter(H == max(H)) %>% 
    select(candidate) %>% 
    slice_sample(n = 1)
}

#Number of iterations:
iters <- 10000





#This function calculates harmony scores for each candidate.  It takes 2 arguments: x = the input candidate, i = which row in the table of perturbed weights we're using.
#Don't forget to add new constraints!
calcHarmonyNoisy <- function(x) {
  return(x %>% 
           mutate(H = Dep*rnorm(1, mean = weights$weight[weights$constraint == "Dep"]) + 
                    Max*rnorm(1, mean = weights$weight[weights$constraint == "Max"]) +
                    SonPeriph*rnorm(1, mean = weights$weight[weights$constraint == "SonPeriph"]) +
                    #OCP*rnorm(1, mean = weights$weight[weights$constraint == "OCP"]) +
                    #DepNonPWD*rnorm(1, mean = weights$weight[weights$constraint == "DepNonPWD"]) +
                    DepMF*rnorm(1, mean = weights$weight[weights$constraint == "DepMF"]) +
                    NoDupMinPPh*rnorm(1, mean = weights$weight[weights$constraint == "NoDupMinPPh"]) +
                    DepS1*rnorm(1, mean = weights$weight[weights$constraint == "DepS1"]) +
                    #SonSeq*rnorm(1, mean = weights$weight[weights$constraint == "SonSeq"]) +
                    OnsetPwd*rnorm(1, mean = weights$weight[weights$constraint == "OnsetPwd"]) +
                    CrispEdgePWd*rnorm(1, mean = weights$weight[weights$constraint == "CrispEdgePWd"]) +
                    AlignNomAL*rnorm(1, mean = weights$weight[weights$constraint == "AlignNomAL"]) +
                    NoComplicated*rnorm(1, mean = weights$weight[weights$constraint == "NoComplicated"])
                  )
         )
}

#A function for calculating non-noisy scores
calcHarmony <- function(x) {
  return(x %>%
           mutate(H = Dep*weights$weight[weights$constraint == "Dep"] +
                    Max*weights$weight[weights$constraint == "Max"] +
                    SonPeriph*weights$weight[weights$constraint == "SonPeriph"] +
                    #OCP*weights$weight[weights$constraint == "OCP"] +
                    #DepNonPWD*weights$weight[weights$constraint == "DepNonPWD"] +
                    #DepMF*weights$weight[weights$constraint == "DepMF"] +
                    NoDupMinPPh*weights$weight[weights$constraint == "NoDupMinPPh"] +
                    #DepS1*weights$weight[weights$constraint == "DepS1"] +
                    SonSeq*weights$weight[weights$constraint == "SonSeq"] +
                    OnsetPwd*weights$weight[weights$constraint == "OnsetPwd"] +
                    CrispEdgePWd*weights$weight[weights$constraint == "CrispEdgePWd"] +
                    AlignNomAL*weights$weight[weights$constraint == "AlignNomAL"] +
                    NoComplicated*weights$weight[weights$constraint == "NoComplicated"]
                  
           )
  )
}


###############################Run the simulation




startingPoints <- c("NOM_arspand",
                    "NOM_vad",
                    "at_ACC_adE",
                    "at_ACC_da",
                    "NOM_ACC_iNdveNna",
                    "NOM_ACC_vad",
                    "vad_NOM",
                    "NOM_DAT_diz",
                    #"NOM2_diz",
                    "NOM_DAT_la_da",
                    "NOM_DAT_arspand",
                    "a_m_tskoord"
                    )


#The heart of the program: this loop computes as many derivations as there are rows in the table of perturbed weights.  It outputs a vector of winners.

winners <- tribble(~input, ~candidate)

for (c in seq_along(startingPoints)) {
  winners_local <- tribble(~input, ~candidate)
  i <- 1
  while(i <= iters) {
    i <- i + 1
    winners_local <- add_row(winners_local, input = startingPoints[[c]], candidate = winner(calcHarmonyNoisy(get(startingPoints[[c]]))))
    if(i %% 500 == 0) {print(str_c(startingPoints[[c]], i, sep = " "))}
  }
  winners <- rbind(winners, winners_local)
}
  

winners <- winners %>% 
  group_by(input, candidate) %>% 
  summarize(n = n()) %>% 
  rename(surface = candidate) %>% 
  mutate(attested = 0,
         attested = replace(attested,
                            surface == "at_l_adE"|
                              surface == "at_al_da"|
                              surface == "am_l_adE"|
                              surface == "am_al_da"|
                              surface == "al_l_iNdveNna"|
                              surface == "al_le_vad"|
                              surface == "l_arspand"|
                              surface == "at_diz"|
                              surface == "al_t_diz"|
                              surface == "al_vad"|
                              surface == "vad_el"|
                              surface == "et_diz"|
                              surface == "t_diz"|
                              surface == "at_la_da"|
                              surface == "al_t_arspand"|
                              surface == "a_m_tskoord"|
                              surface == "a_[me_tskoord", 
                            1))

winners


saveRDS(winners, "NoisyHGResults_withNoComplicated.rds")
write_csv(winners, "NoisyHGResults_withNoComplicated.csv")



