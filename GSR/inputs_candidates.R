library(tidyverse)

#Put input activity levels here.
#Let's assume duplexes have an activity level of .3.


nomL <- .1
nomAL <- .8
duplex <- .45
accL <- .95
accAL <- .7
datT <- .3
nomT <- .3



#the following should be greater than 0
ineq_allevad <- Dep*(nomAL- nomL+accL-accAL-1)+Max*(nomAL+accL -nomL-accAL)
ineq_allevad_alal <- Dep*accL - Dep + Max*accL - Max*accAL - Dep*accAL 


#the following should be less than 0
ineq_atalda <- Max*(accL-accAL) - Dep*(accAL-accL+1)



NOM_arspand <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "l_arspand", -(1-nomL), nomL, 0, 0, 0, 0, 0, -(1-nomL), 0, 0, 0, 0,
  "a[l_arspand", -(1-nomAL), nomAL, 0, 0, 0, 0, 0, -(1-nomAL), 0, 0, -1, -1,
  "a[m_arspand", -(1-2*duplex), duplex, 0, 0, 0, -1, -1, -(1-duplex), 0, 0, -1, 0,
  "[al_arspand", -(1-nomAL), nomAL, 0, 0, 0, 0, 0, -(1-nomAL), 0, -1, 0, 0,
  "[am_arspand", -(1-duplex), duplex, 0, 0, 0, -1, -1, -(1-duplex), 0, -1, 0, 0
)


NOM_vad <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "al_vad", -(1-nomAL), nomAL, 0, 0, 0, 0, 0, -(1-nomAL), 0, 0, 0, 0,
  "l_vad", -(1-nomL), nomL, -1, 0, 0, 0, 0, -(1-nomL), 0, 0, 0, 0,
  "am_vad", -(1-duplex), duplex, -1, 0, 0, -1, -1, -(1-duplex), 0, 0, 0, 0,
  "le_vad", -(1-nomL)-1, nomL, -1, 0, 0, 0, 0, -(1-nomL)-1, 0, 0, 0, 0
)

at_ACC_adE <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "at_l_adE", -(1-2*duplex)-(1-accL), duplex*2 + accL, 0, 0, 0, 0, 0, duplex*2, 0, 0, 0, 0,
  "at_a[l_adE", -(1-2*duplex)-(1-accAL), duplex*2 + accAL, 0, 0, 0, 0, 0, duplex*2, 0, 0, -1, 0,
  "at_[al_adE", -(1-2*duplex)-(1-accAL), duplex*2 + accAL, 0, 0, 0, 0, 0, duplex*2, 0, -1, 0, 0,
  "al_t_l_adE", -(1-nomAL) -(1-datT) -(1-accL), nomAL + datT + accL, 0, 0, 0, 0, 0, -(1-nomAL) -(1-datT), 0, 0, 0, 0,
  "al_t_a[l_adE", -(1-nomAL) -(1-datT) -(1-accAL), nomAL + datT + accAL, 0, 0, 0, 0, 0, -(1-nomAL), 0, 0, -1, 0,
  "al_t_[al_adE", -(1-nomAL) -(1-datT) -(1-accAL), nomAL + datT + accAL, 0, 0, 0, 0, 0, -(1-nomAL), 0, -1, 0, 0,
  "l_t_l_adE", -(1-nomL) -(1-datT) -(1-accL), nomL + datT + accL, -1, 0, 0, 0, 0, -(1-nomL)-(1-datT)-(1-accL), 0, 0, 0, 0,
  "l_t_a[l_adE", -(1-nomL) -(1-datT) -(1-accAL), nomL + datT + accAL, -1, 0, 0, 0, 0, -(1-nomL), 0, 0, -1, 0,
  "l_t_[al_adE", -(1-nomL) -(1-datT) -(1-accAL), nomL + datT + accAL, -1, 0, 0, 0, 0, -(1-nomL), 0, -1, 0, 0,
  "le_t_l_adE", -(1-nomL) -(1-datT) -(1-accL)-1, nomL + datT + accL, 0, 0, 0, 0, 0, -(1-nomL)-(1-datT)-1, 0, 0, 0, 0,
  "le_t_a[l_adE", -(1-nomL) -(1-datT) -(1-accAL)-1, nomL + datT + accAL, 0, 0, 0, 0, 0, -(1-nomL)-1, 0, 0, -1, 0,
  "le_t_[al_adE", -(1-nomL) -(1-datT) -(1-accAL)-1, nomL + datT + accAL, 0, 0, 0, 0, 0, -(1-nomL)-1, 0, -1, 0, 0
)



at_ACC_da <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "at_al_da", -(1-(2*duplex))-(1-accAL), duplex*2 + accAL, 0, 0, 0, 0, 0, -(1-2*duplex), 0, 0, 0, 0,
  "at_l_da", -(1-(2*duplex))-(1-accL), duplex*2 + accL, -1, 0, 0, 0, 0, -(1-2*duplex), 0, 0, 0, 0,
  "at_le_da", -(1-2*duplex)-(1-accL)-1, duplex*2 + accL, 0, 0, -1, 0, 0, -(1-2*duplex), 0, 0, 0, 0,
  "al_t_al_da", -(1-nomAL) -(1-datT) -(1-accAL), nomAL + datT + accAL, 0, 0, 0, 0, 0, -(1-nomAL), 0, 0, 0, 0,
  "al_t_l_da", -(1-nomAL) -(1-datT) -(1-accL), nomAL + datT + accL, -1, 0, 0, 0, 0, -(1-nomAL)-(1-datT), 0, 0, 0, 0,
  "al_t_le_da", -(1-nomAL) -(1-datT) -(1-accL)-1, nomAL + datT + accL, 0, 0, -1, 0, 0, -(1-nomAL)-(1-datT), 0, 0, 0, 0,
  "l_t_al_da", -(1-nomL) -(1-datT) -(1-accAL), nomL + datT + accAL, -1, 0, 0, 0, 0, -(1-nomL) -(1-datT) -(1-accAL), 0, 0, 0, 0,
  "l_t_l_da", -(1-nomL) -(1-datT) -(1-accL), nomL + datT + accL, -1, 0, 0, 0, 0, -(1-nomL)-(1-datT) -(1-accL), 0, 0, 0, 0,
  "l_t_le_da", -(1-nomL) -(1-datT) -(1-accL)-1, nomL + datT + accL, -1, 0, -1, 0, 0, -(1-nomL)-(1-datT) -(1-accL) -1, 0, 0, 0, 0,
  "le_t_al_da", -(1-nomL) -(1-datT) -(1-accAL), nomL + datT + accAL, 0, 0, 0, 0, 0, -(1-nomL)-1, 0, 0, 0, 0,
  "le_t_l_da", -(1-nomL) -(1-datT) -(1-accL), nomL + datT + accL, 0, 0, 0, 0, 0, -(1-nomL)-(1-datT)-1, 0, 0, 0, 0,
  "le_t_le_da", -(1-nomL) -(1-datT) -(1-accL)-2, nomL + datT + accL, 0, 0, -1, 0, 0, -(1-nomL)-(1-datT) -1, 0, 0, 0 ,0
)


NOM_DAT_la_da <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "at_la_da", -(1-2*duplex), 2*duplex, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  "al_t_la_da", -(1-nomAL)-(1-datT), nomAL+datT, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  "l_t_la_da", -(1-nomL)-(1-datT), nomL+datT, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0,
  "le_t_la_da", -(1-nomL)-(1-datT)-1, nomL+datT, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0
)


NOM_ACC_iNdveNna <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "al_l_iNdveNna", (-(1-nomAL))+(-(1-accL)), nomAL+accL, 0, 0, 0, 0, 0, -(1-nomAL), 0, 0, 0, 0,
  "al_a[l_iNdveNna", (-(1-nomAL))+(-(1-accAL)), nomAL+accAL, 0, 0, 0, 0, 0, -(1-nomAL), 0, 0, -1, 0,
  "al_[al_iNdveNna", (-(1-nomAL))+(-(1-accAL)), nomAL+accAL, 0, 0, 0, 0, 0, -(1-nomAL), 0, -1, 0, 0,
  "l_l_iNdveNna", (-(1-nomL))+(-(1-accL)), nomL+accL, -1, 0, 0, 0, 0, -(1-nomL), 0, 0, 0, 0,
  "l_a[l_iNdveNna", (-(1-nomL))+(-(1-accAL)), nomL+accAL, 0, 0, 0, 0, 0, -(1-nomL), 0, 0, -1, 0,
  "l_[al_iNdveNna", (-(1-nomL))+(-(1-accAL)), nomL+accAL, 0, 0, 0, 0, 0, -(1-nomL), 0, -1, 0, 0,
  "le_l_iNdveNna", (-(1-nomL))+(-(1-accL))-1, nomL+accL, 0, 0, -1, 0, 0, -1-(1-nomL), 0, 0, 0, 0,
  "am_l_iNdveNna", (-(1-duplex))+(-(1-accL)), duplex+accL, 0, 0, 0, -1, 0, -(1-duplex), 0, 0, 0, 0
)


NOM_ACC_vad <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "al_le_vad", (-(1-nomAL))+(-(1-accL))-1, nomAL+accL, 0, 0, -1, 0, 0, -(1-nomAL), 0, 0, 0, 0,
  "l_al_vad", (-(1-nomL))+(-(1-accAL)), nomL+accAL, 0, -1, 0, 0, 0, -(1-nomL), 0, 0, 0, 0,
  "le_l_vad", (-(1-nomL))+(-(1-accL))-1, nomL+accL, 0, -1, -1, 0, 0, -1-(1-nomL), 0, 0, 0, 0,
  "al_al_vad", (-(1-nomAL))+(-(1-accAL)), nomAL+accAL, 0, -1, 0, 0, 0, -(1-nomAL), 0, 0, 0, -1,
  "am_al_vad", (-(1-duplex))+(-(1-accAL)), duplex+accAL, 0, -1, 0, -1, 0, -(1-duplex), 0, 0, 0, 0
)


vad_NOM <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "vad_el", -(1-nomL)-1, nomL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  "vad_al", -(1-nomAL), nomAL, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1,
  "vad_l", -(1-nomL), nomL, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  "vad_am", -(1-duplex), duplex, -1, 0, 0, -1, 0, 0, 0, 0, 0, 0
)


NOM_DAT_diz <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "al_t_diz", (-(1-nomAL))+(-(1-datT)), nomAL+datT, 0, 0, 0, 0, 0, (-(1-nomAL))+(-(1-datT)), 0, 0, 0, 0,
  "at_diz", (-(1-2*duplex)), 2*duplex, 0, 0, 0, 0, -1, (-(1-2*duplex)), 0, 0, 0, 0,
  "l_t_diz", (-(1-nomL))+(-(1-datT)), nomL+datT, -1, 0, 0, 0, 0, (-(1-nomL))+(-(1-datT)), 0, 0, 0, 0,
  "le_t_diz", (-(1-nomL))+(-(1-datT))-1, nomL+datT, 0, 0, -1, 0, 0, (-(1-nomL))+(-(1-datT))-1, 0, 0, 0, 0
)

NOM2_diz <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "et_diz", -(1-nomT)-1, 0, 0, 0, -1, 0, 0, -(1-nomT)-1, 0, 0, 0, 0,
  "t_diz", -(1-nomT), 0, 0, 0, 0, 0, 0, -(1-nomT), -1, 0, 0, 0
)

NOM_DAT_arspand <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq, ~OnsetPwd, ~CrispEdgePWd, ~AlignNomAL,
  "al_t_arspand", (-(1-nomAL))+(-(1-datT)), nomAL+datT, 0, 0, 0, 0, 0, (-(1-nomAL)), 0, 0, 0, 0,
  "[at_arspand", (-(1-2*duplex)), 2*duplex, 0, 0, 0, 0, -1, (-(1-2*duplex)), 0, -1, 0, 0,
  "a[t_arspand", (-(1-2*duplex)), 2*duplex, 0, 0, 0, 0, -1, (-(1-2*duplex)), 0, 0, -1, 0,
  "at_[arspand", (-(1-2*duplex)), 2*duplex, 0, 0, 0, 0, -1, (-(1-2*duplex)), 0, -1, 0, 0,
  "l_t_arspand", (-(1-nomL))+(-(1-datT)), nomL+datT, -1, 0, 0, 0, 0, (-(1-nomL))+(-(1-datT)), 0, 0, 0, 0,
  "le_t_arspand", (-(1-nomL))+(-(1-datT))-1, nomL+datT, 0, 0, -1, 0, 0, -(1-nomL)-1, 0, 0, 0, 0
)


#fix the following violations. First candidate is OK.
NOM2_ACC_vad <- tribble(
  ~candidate, ~Dep, ~Max, ~SonPeriph, ~OCP, ~DepNonPWD, ~DepMF, ~NoDupMinPPh, ~DepS1, ~SonSeq,
  "t_al_vad", -(1-datT)-(1-accAL), -accL, 0, 0, 0, 0, 0, -(1-datT)-(1-accAL), 0,
  "et_al_vad", -(1-datT)-1-(1-accAL), -accL, 0, 0, -1, 0, 0, -1, 0,
  "te_l_vad", -(1-datT)-1-(1-accL), -accAL, 0, 0, -1, 0, 0, -(1-datT)-1-(1-accL), 0,
  "et_l_vad", -(1-datT)-1-(1-accL), -accAL, -1, 0, -1, 0, 0, -(1-datT)-1, 0,
)