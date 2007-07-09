# estimate dose adjusment of Aminoglcoside with single subject and each mutiple concentrations
Amism.more<-function(B,E,F,G)
{
 cat("\n")                                      # menu of dose adjustment
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){                                                                               # choose D -> C
     cat("\n")
     cat("********************************************\n")                                      # information of dose adjustment
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("  tin = desired infusion time (hr)          \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amicpar<-data.frame(input=c("D (mg)","tau (hr)","tin (hr)"),value=c(0))                    # edit table of aminoglycoside input data information 
     Amicpar<-edit(Amicpar)                                                                     # show table of Aminoglycoside input data information for user editing
     Amicpar<-check(Amicpar)                                                                    # avoid user missing input information
     C<-(Amicpar[1,2]*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amicpar[3,2]))/(Amicpar[3,2]*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amicpar[2,2]))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Amicpar[2,2]-Amicpar[3,2]))   # calculate concentratin of Aminoglycoside
     sim<-matrix(C[1 ,1])                                                                       # 取表格[1,1]之答案
     coutput<-data.frame(sim)                                                                   # 命名所取出來的[1,1]為couput
     colnames(coutput)<-list("Css_trough (mg/L)")                                               # 並命名此欄位為Css_trough(mg/L)
     cat("\n")
     show(coutput)
       cat("\n")                          
       file.menu <- c("Yes",                                             # dose adjustment again or not
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){                                                   # if yes, dose adjustment again
          Amism.more(B,E,F,G)
       } else {
             if (pick == 2){                                             # is no, show output file(包含病人資訊,預測參數跟劑量調整部分)
             Amism.output(B,E,F,G,Amicpar,coutput)
             cal.again()
             }
         } 
  } else {
    if (pick == 2){                                                                               # choose C -> D
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("  tin = desired infusion time (hr)          \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amidpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)","tin (hr)"),value=c(0))
     Amidpar<-edit(Amidpar)
     Amidpar<-check(Amidpar)
     d<-Amidpar[1,2]/(((1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidpar[3,2]))/(Amidpar[3,2]*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidpar[2,2]))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Amidpar[2,2]-Amidpar[3,2])))  # calculate does of Aminoglycoside
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes",                                             # dose adjustment again or not
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){                                                   # if yes, dose adjustment again
          Amism.more(B,E,F,G)
       } else {
             if (pick == 2){                                             # is no, show output file(包含病人資訊,預測參數跟劑量調整部分)
             Amism.output(B,E,F,G,Amidpar,doutput)
             cal.again()
             }
         } 
    } else {
    if (pick == 3){                                                                               # choose D <-> C(綜合上面兩者，先算濃度再算劑量)
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("  tin = desired infusion time (hr)          \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amicpar<-data.frame(input=c("D (mg)","tau (hr)","tin (hr)"),value=c(0))
     Amicpar<-edit(Amicpar)
     Amicpar<-check(Amicpar)
     C<-(Amicpar[1,2]*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amicpar[3,2]))/(Amicpar[3,2]*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amicpar[2,2]))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Amicpar[2,2]-Amicpar[3,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()          # 按enter之後會繼續進行
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("  tin = desired infusion time (hr)          \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amidpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)","tin (hr)"),value=c(0))
     Amidpar<-edit(Amidpar)
     Amidpar<-check(Amidpar)
     d<-Amidpar[1,2]/(((1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidpar[3,2]))/(Amidpar[3,2]*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidpar[2,2]))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Amidpar[2,2]-Amidpar[3,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes",                                             # dose adjustment again or not 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){                                                   # if yes, dose adjustment again
          Amism.more(B,E,F,G)
       } else {
             if (pick == 2){                                             # is no, show output file(包含病人資訊,預測參數跟劑量調整部分)
             Amismcd.output(B,E,F,G,Amicpar,coutput,Amidpar,doutput)
             cal.again()
             }
         } 
      } else {
      if (pick == 4){                      # 選4表示其不進行 dose adjustment，所以會呈現outout report包含病人資料跟預測出的餐數值
        Amism.pkoutput(B,E,F,G)
        cal.again()
        }   
      }
    }  
  }
}

Vansm.more<-function(A,B,E,F)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("  tin = desired infusion time (hr)          \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Vancpar<-data.frame(input=c("D (mg)","tau (hr)","tin (hr)"),value=c(0))
     Vancpar<-edit(Vancpar)
     Vancpar<-check(Vancpar)
     C<-(Vancpar[1,2]*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vancpar[3,2]))/(Vancpar[3,2]*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vancpar[2,2]))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Vancpar[2,2]-Vancpar[3,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Vansm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Vansm.output(A,B,E,F,Vancpar,coutput)
             cal.again()
             }
         } 
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("  tin = desired infusion time (hr)          \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Vandpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)","tin (hr)"),value=c(0))
     Vandpar<-edit(Vandpar)
     Vandpar<-check(Vandpar)
     d<-Vandpar[1,2]/(((1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vandpar[3,2]))/(Vandpar[3,2]*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vandpar[2,2]))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Vandpar[2,2]-Vandpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Vansm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Vansm.output(A,B,E,F,Vandpar,doutput)
             cal.again()
             }
         } 
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("  tin = desired infusion time (hr)          \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Vancpar<-data.frame(input=c("D (mg)","tau (hr)","tin (hr)"),value=c(0))
     Vancpar<-edit(Vancpar)
     Vancpar<-check(Vancpar)
     C<-(Vancpar[1,2]*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vancpar[3,2]))/(Vancpar[3,2]*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vancpar[2,2]))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Vancpar[2,2]-Vancpar[3,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("  tin = desired infusion time (hr)          \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Vandpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)","tin (hr)"),value=c(0))
     Vandpar<-edit(Vandpar)
     Vandpar<-check(Vandpar)
     d<-Vandpar[1,2]/(((1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vandpar[3,2]))/(Vandpar[3,2]*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vandpar[2,2]))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Vandpar[2,2]-Vandpar[3,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Vansm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Vansmcd.output(A,B,E,F,Vancpar,coutput,Vandpar,doutput)
             cal.again()
             }
         } 
      } else {
      if (pick == 4){
        Vansm.pkoutput(A,B,E,F)
        cal.again()
        }   
      }
    }  
  }
}


Carsm.more<-function(B,E,F)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     CBZcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     CBZcpar<-edit(CBZcpar)
     CBZcpar<-check(CBZcpar)
     C<-1.2*CBZcpar[1,2]/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZcpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZcpar[2,2])-(1/(1-exp(-1.2*CBZcpar[2,2])))*exp(-1.2*CBZcpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Carsm.more(B,E,F)
       } else {
             if (pick == 2){
             Carsm.output(B,E,F,CBZcpar,coutput)
             cal.again()
             }
         } 
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     CBZdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     CBZdpar<-edit(CBZdpar)
     CBZdpar<-check(CBZdpar)
     d<-CBZdpar[1,2]/(1.2/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZdpar[2,2])-(1/(1-exp(-1.2*CBZdpar[2,2])))*exp(-1.2*CBZdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Carsm.more(B,E,F)
       } else {
             if (pick == 2){
             Carsm.output(B,E,F,CBZdpar,doutput)
             cal.again()
             }
         } 
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     CBZcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     CBZcpar<-edit(CBZcpar)
     CBZcpar<-check(CBZcpar)
     C<-1.2*CBZcpar[1,2]/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZcpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZcpar[2,2])-(1/(1-exp(-1.2*CBZcpar[2,2])))*exp(-1.2*CBZcpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     CBZdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     CBZdpar<-edit(CBZdpar)
     CBZdpar<-check(CBZdpar)
     d<-CBZdpar[1,2]/(1.2/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZcpar[2,2])-(1/(1-exp(-1.2*CBZdpar[2,2])))*exp(-1.2*CBZdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Carsm.more(B,E,F)
       } else {
             if (pick == 2){
             Carsmcd.output(B,E,F,CBZcpar,coutput,CBZdpar,doutput)
             cal.again()
             }
         } 
      } else {
      if (pick == 4){
      	Carsm.pkoutput(B,E,F)
        cal.again()
        }   
      }
    }  
  }
}

Enfsm.more<-function(A,B,E)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Enfcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Enfcpar<-edit(Enfcpar)
     Enfcpar<-check(Enfcpar)
     C<-(samplesStats("ka"))*Enfcpar[1,2]/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfcpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfcpar[2,2])-(1/(1-exp(-(samplesStats("ka"))*Enfcpar[2,2])))*exp(-(samplesStats("ka"))*Enfcpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Enfsm.more(A,B,E)
       } else {
             if (pick == 2){
             Enfsm.output(A,B,E,Enfcpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Enfdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Enfdpar<-edit(Enfdpar)
     Enfdpar<-check(Enfdpar)
     d<-Enfdpar[1,2]/((samplesStats("ka"))/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfdpar[2,2])-(1/(1-exp(-(samplesStats("ka"))*Enfdpar[2,2])))*exp(-(samplesStats("ka"))*Enfdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Enfsm.more(A,B,E)
       } else {
             if (pick == 2){
             Enfsm.output(A,B,E,Enfdpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Enfcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Enfcpar<-edit(Enfcpar)
     Enfcpar<-check(Enfcpar)
     C<-(samplesStats("ka"))*Enfcpar[1,2]/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfcpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfcpar[2,2])-(1/(1-exp(-(samplesStats("ka"))*Enfcpar[2,2])))*exp(-(samplesStats("ka"))*Enfcpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Enfdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Enfdpar<-edit(Enfdpar)
     Enfdpar<-check(Enfdpar)
     d<-Enfdpar[1,2]/((samplesStats("ka"))/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfdpar[2,2])-(1/(1-exp(-(samplesStats("ka"))*Enfdpar[2,2])))*exp(-(samplesStats("ka"))*Enfdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Enfsm.more(A,B,E)
       } else {
             if (pick == 2){
             Enfsmcd.output(A,B,E,Enfcpar,coutput,Enfdpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Enfsm.pkoutput(A,B,E)
        cal.again()  
        }   
      }
    }  
  }
}

Indsm.more<-function(A,B,E)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Indcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Indcpar<-edit(Indcpar)
     Indcpar<-check(Indcpar)
     C<-(samplesStats("ka"))*Indcpar[1,2]/(65.7*((samplesStats("ka"))-(samplesStats("cl_F"))/65.7))*((1/(1-exp(-(samplesStats("cl_F"))/65.7*Indcpar[2,2])))*exp(-(samplesStats("cl_F"))/65.7*Indcpar[2,2])-(1/(1-exp(-(samplesStats("ka"))*Indcpar[2,2])))*exp(-(samplesStats("ka"))*Indcpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Indsm.more(A,B,E)
       } else {
             if (pick == 2){
             Indsm.output(A,B,E,Indcpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Inddpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Inddpar<-edit(Inddpar)
     Inddpar<-check(Inddpar)
     d<-Inddpar[1,2]/((samplesStats("ka"))/(65.7*((samplesStats("ka"))-(samplesStats("cl_F"))/65.7))*((1/(1-exp(-(samplesStats("cl_F"))/65.7*Inddpar[2,2])))*exp(-(samplesStats("cl_F"))/65.7*Inddpar[2,2])-(1/(1-exp(-(samplesStats("ka"))*Inddpar[2,2])))*exp(-(samplesStats("ka"))*Inddpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Indsm.more(A,B,E)
       } else {
             if (pick == 2){
             Indsm.output(A,B,E,Inddpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Indcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Indcpar<-edit(Indcpar)
     Indcpar<-check(Indcpar)
     C<-(samplesStats("ka"))*Indcpar[1,2]/(65.7*((samplesStats("ka"))-(samplesStats("cl_F"))/65.7))*((1/(1-exp(-(samplesStats("cl_F"))/65.7*Indcpar[2,2])))*exp(-(samplesStats("cl_F"))/65.7*Indcpar[2,2])-(1/(1-exp(-(samplesStats("ka"))*Indcpar[2,2])))*exp(-(samplesStats("ka"))*Indcpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Inddpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Inddpar<-edit(Inddpar)
     Inddpar<-check(Inddpar)
     d<-Inddpar[1,2]/((samplesStats("ka"))/(65.7*((samplesStats("ka"))-(samplesStats("cl_F"))/65.7))*((1/(1-exp(-(samplesStats("cl_F"))/65.7*Inddpar[2,2])))*exp(-(samplesStats("cl_F"))/65.7*Inddpar[2,2])-(1/(1-exp(-(samplesStats("ka"))*Inddpar[2,2])))*exp(-(samplesStats("ka"))*Inddpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Indsm.more(A,B,E)
       } else {
             if (pick == 2){
             Indsmcd.output(A,B,E,Indcpar,coutput,Inddpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Indsm.pkoutput(A,B,E)
        cal.again()  
        }   
      }
    }  
  }
}


Ritsm.more<-function(A,B,E)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Ritcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Ritcpar<-edit(Ritcpar)
     Ritcpar<-check(Ritcpar)
     C<-(samplesStats("ka"))*Ritcpar[1,2]/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Ritcpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Ritcpar[2,2]-0.778))-(1/(1-exp(-(samplesStats("ka"))*Ritcpar[2,2])))*exp(-(samplesStats("ka"))*(Ritcpar[2,2]-0.778)))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Ritsm.more(A,B,E)
       } else {
             if (pick == 2){
             Ritsm.output(A,B,E,Ritcpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Ritdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Ritdpar<-edit(Ritdpar)
     Ritdpar<-check(Ritdpar)
     d<-Ritdpar[1,2]/((samplesStats("ka"))/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Ritdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Ritdpar[2,2]-0.778))-(1/(1-exp(-(samplesStats("ka"))*Ritdpar[2,2])))*exp(-(samplesStats("ka"))*(Ritdpar[2,2]-0.778))))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Ritsm.more(A,B,E)
       } else {
             if (pick == 2){
             Ritsm.output(A,B,E,Ritdpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Ritcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Ritcpar<-edit(Ritcpar)
     Ritcpar<-check(Ritcpar)
     C<-(samplesStats("ka"))*Ritcpar[1,2]/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Ritcpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Ritcpar[2,2]-0.778))-(1/(1-exp(-(samplesStats("ka"))*Ritcpar[2,2])))*exp(-(samplesStats("ka"))*(Ritcpar[2,2]-0.778)))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Ritdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Ritdpar<-edit(Ritdpar)
     Ritdpar<-check(Ritdpar)
     d<-Ritdpar[1,2]/((samplesStats("ka"))/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Ritdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Ritdpar[2,2]-0.778))-(1/(1-exp(-(samplesStats("ka"))*Ritdpar[2,2])))*exp(-(samplesStats("ka"))*(Ritdpar[2,2]-0.778))))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Ritsm.more(A,B,E)
       } else {
             if (pick == 2){
             Ritsmcd.output(A,B,E,Ritcpar,coutput,Ritdpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Ritsm.pkoutput(A,B,E)
        cal.again()  
        }   
      }
    }  
  }
}




Evesm.more<-function(A,B,E)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>")  
  if (pick == 1){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mcg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Evecpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Evecpar<-edit(Evecpar)
     Evecpar<-check(Evecpar)
     C<-6.07*Evecpar[1,2]/((samplesStats("v_F"))*(6.07-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evecpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evecpar[2,2])-(1/(1-exp(-6.07*Evecpar[2,2])))*exp(-6.07*Evecpar[2,2]))*1000
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mcg/L)")
     cat("\n")
     show(coutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Evesm.more(A,B,E)
       } else {
             if (pick == 2){
             Evesm.output(A,B,E,Evecpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mcg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Evedpar<-data.frame(input=c("Css_trough (mcg/L)","tau (hr)"),value=c(0))
     Evedpar<-edit(Evedpar)
     Evedpar<-check(Evedpar)
     d<-Evedpar[1,2]/(6.07/((samplesStats("v_F"))*(6.07-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evedpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evedpar[2,2])-(1/(1-exp(-6.07*Evedpar[2,2])))*exp(-6.07*Evedpar[2,2]))*1000)
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Evesm.more(A,B,E)
       } else {
             if (pick == 2){
             Evesm.output(A,B,E,Evedpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mcg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Evecpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Evecpar<-edit(Evecpar)
     Evecpar<-check(Evecpar)
     C<-6.07*Evecpar[1,2]/((samplesStats("v_F"))*(6.07-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evecpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evecpar[2,2])-(1/(1-exp(-6.07*Evecpar[2,2])))*exp(-6.07*Evecpar[2,2]))*1000
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mcg/L)")
     cat("\n\n")
     show(coutput)
     cat("\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mcg/L)  \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Evedpar<-data.frame(input=c("Css_trough (mcg/L)","tau (hr)"),value=c(0))
     Evedpar<-edit(Evedpar)
     Evedpar<-check(Evedpar)
     d<-Evedpar[1,2]/(6.07/((samplesStats("v_F"))*(6.07-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evedpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evedpar[2,2])-(1/(1-exp(-6.07*Evedpar[2,2])))*exp(-6.07*Evedpar[2,2]))*1000)
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Evesm.more(A,B,E)
       } else {
             if (pick == 2){
             Evesmcd.output(A,B,E,Evecpar,coutput,Evedpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Evesm.pkoutput(A,B,E)
        cal.again()  
        }   
      }
    }  
  }
}

Tacsm.more<-function(A,B,E)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("*********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mcg/mL) \n")
     cat("*********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Taccpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Taccpar<-edit(Taccpar)
     Taccpar<-check(Taccpar)
     C<-4.5*Taccpar[1,2]/(314*(4.5-(samplesStats("cl_F"))/314))*((1/(1-exp(-(samplesStats("cl_F"))/314*Taccpar[2,2])))*exp(-(samplesStats("cl_F"))/314*Taccpar[2,2])-(1/(1-exp(-4.5*Taccpar[2,2])))*exp(-4.5*Taccpar[2,2]))*1000
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mcg/mL)")
     cat("\n")
     show(coutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Tacsm.more(A,B,E)
       } else {
             if (pick == 2){
             Tacsm.output(A,B,E,Taccpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mcg/mL) \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Tacdpar<-data.frame(input=c("Css_trough (mcg/mL)","tau (hr)"),value=c(0))
     Tacdpar<-edit(Tacdpar)
     Tacdpar<-check(Tacdpar)
     d<-Tacdpar[1,2]/(4.5/(314*(4.5-(samplesStats("cl_F"))/314))*((1/(1-exp(-(samplesStats("cl_F"))/314*Tacdpar[2,2])))*exp(-(samplesStats("cl_F"))/314*Tacdpar[2,2])-(1/(1-exp(-4.5*Tacdpar[2,2])))*exp(-4.5*Tacdpar[2,2]))*1000)
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Tacsm.more(A,B,E)
       } else {
             if (pick == 2){
             Tacsm.output(A,B,E,Tacdpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
       cat("\n")
     cat("*********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mcg/mL)\n")
     cat("*********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Taccpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Taccpar<-edit(Taccpar)
     Taccpar<-check(Taccpar)
     C<-4.5*Taccpar[1,2]/(314*(4.5-(samplesStats("cl_F"))/314))*((1/(1-exp(-(samplesStats("cl_F"))/314*Taccpar[2,2])))*exp(-(samplesStats("cl_F"))/314*Taccpar[2,2])-(1/(1-exp(-4.5*Taccpar[2,2])))*exp(-4.5*Taccpar[2,2]))*1000
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mcg/mL)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mcg/mL) \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Tacdpar<-data.frame(input=c("Css_trough (mcg/mL)","tau (hr)"),value=c(0))
     Tacdpar<-edit(Tacdpar)
     Tacdpar<-check(Tacdpar)
     d<-Tacdpar[1,2]/(4.5/(314*(4.5-(samplesStats("cl_F"))/314))*((1/(1-exp(-(samplesStats("cl_F"))/314*Tacdpar[2,2])))*exp(-(samplesStats("cl_F"))/314*Tacdpar[2,2])-(1/(1-exp(-4.5*Tacdpar[2,2])))*exp(-4.5*Tacdpar[2,2]))*1000)
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Tacsm.more(A,B,E)
       } else {
             if (pick == 2){
             Tacsmcd.output(A,B,E,Taccpar,coutput,Tacdpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Tacsm.pkoutput(A,B,E)
        cal.again() 
        }   
      }
    }  
  }
}

# Imatinib mesylate
Imasm.more<-function(A,B,E)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("**********************************************************\n")
     cat("  --input data--                                          \n")
     cat("  D = desired dose (mg)                                   \n")
     cat("  tau = desired dosing interval (hr)                      \n")
     cat("                                                          \n")
     cat("  --output data--                                         \n")
     cat("  Css_trough = predicted steady-state trough conc. (mg/L) \n")
     cat("**********************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Imacpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Imacpar<-edit(Imacpar)
     Imacpar<-check(Imacpar)
     C<-((Imacpar[1,2]/(1.5*(samplesStats("cl_F"))))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Imacpar[2,2]-1.5))))/(1-(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Imacpar[2,2])))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Imasm.more(A,B,E)
       } else {
             if (pick == 2){
             Imasm.output(A,B,E,Imacpar,coutput)
             cal.again()
             }
         } 
  } else {
    if (pick == 2){
     cat("\n")
     cat("**********************************************************\n\n")
     cat("  --input data--                                          \n")
     cat("  Css_trough = predicted steady-state trough conc. (mg/L) \n")
     cat("  tau = desired dosing interval (hr)                      \n")
     cat("                                                          \n")
     cat("  --output data--                                         \n")
     cat("  Dose = predicted dose (mg)                              \n")
     cat("**********************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Imadpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Imadpar<-edit(Imadpar)
     Imadpar<-check(Imadpar)
     d<-Imadpar[1,2]/(((1/(1.5*(samplesStats("cl_F"))))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Imadpar[2,2]-1.5))))/(1-(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Imadpar[2,2]))))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Imasm.more(A,B,E)
       } else {
             if (pick == 2){
             Imasm.output(A,B,E,Imadpar,doutput)
             cal.again()
             }
         } 
    } else {
    if (pick == 3){
     cat("\n")
     cat("**********************************************************\n")
     cat("  --input data--                                          \n")
     cat("  D = desired dose (mg)                                   \n")
     cat("  tau = desired dosing interval (hr)                      \n")
     cat("                                                          \n")
     cat("  --output data--                                         \n")
     cat("  Css_trough = predicted steady-state trough conc. (mg/L) \n")
     cat("**********************************************************\n\n")
     cat("\n\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Imacpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Imacpar<-edit(Imacpar)
     Imacpar<-check(Imacpar)
     C<-((Imacpar[1,2]/(1.5*(samplesStats("cl_F"))))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Imacpar[2,2]-1.5))))/(1-(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Imacpar[2,2])))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("**********************************************************\n\n")
     cat("  --input data--                                          \n")
     cat("  Css_trough = predicted steady-state trough conc. (mg/L) \n")
     cat("  tau = desired dosing interval (hr)                      \n")
     cat("                                                          \n")
     cat("  --output data--                                         \n")
     cat("  Dose = predicted dose (mg)                              \n")
     cat("**********************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Imadpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Imadpar<-edit(Imadpar)
     Imadpar<-check(Imadpar)
     d<-Imadpar[1,2]/(((1/(1.5*(samplesStats("cl_F"))))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Imadpar[2,2]-1.5))))/(1-(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Imadpar[2,2]))))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Imasm.more(A,B,E)
       } else {
             if (pick == 2){
             Imasmcd.output(A,B,E,Imacpar,coutput,Imadpar,doutput)
             cal.again()
             }
         } 
      } else {
      if (pick == 4){
      	Imasm.pkoutput(A,B,E)
        cal.again()
        }   
      }
    }  
  }
}



Amianhirsm.more<-function(A,B,E,F)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amianhircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amianhircpar<-edit(Amianhircpar)
     Amianhircpar<-check(Amianhircpar)
     C<-0.85*1.85*Amianhircpar[1,2]/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhircpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhircpar[2,2])-(1/(1-exp(-1.85*Amianhircpar[2,2])))*exp(-1.85*Amianhircpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amianhirsm.output(A,B,E,F,Amianhircpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amianhirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amianhirdpar<-edit(Amianhirdpar)
     Amianhirdpar<-check(Amianhirdpar)
     d<-Amianhirdpar[1,2]/(0.85*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirdpar[2,2])-(1/(1-exp(-1.85*Amianhirdpar[2,2])))*exp(-1.85*Amianhirdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amianhirsm.output(A,B,E,F,Amianhirdpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amianhircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amianhircpar<-edit(Amianhircpar)
     Amianhircpar<-check(Amianhircpar)
     C<-0.85*1.85*Amianhircpar[1,2]/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhircpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhircpar[2,2])-(1/(1-exp(-1.85*Amianhircpar[2,2])))*exp(-1.85*Amianhircpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amianhirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amianhirdpar<-edit(Amianhirdpar)
     Amianhirdpar<-check(Amianhirdpar)
     d<-Amianhirdpar[1,2]/(0.85*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirdpar[2,2])-(1/(1-exp(-1.85*Amianhirdpar[2,2])))*exp(-1.85*Amianhirdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amianhirsmcd.output(A,B,E,F,Amianhircpar,coutput,Amianhirdpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Amianhirsm.pkoutput(A,B,E,F)
        cal.again()
        }   
      }
    }  
  }
}


Amianhinfusionsm.more<-function(A,B,E,F)
{
 cat("\n")
  file.menu <- c("R -> Css_trough",
                 "Css_trough -> R",
                 "Css_trough <-> R",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("*************************************************\n")
     cat("  --input data--                                 \n")
     cat("  R = constant intravenous infusion rate (mg/hr) \n")
     cat("                                                 \n")
     cat("  --output data--                                \n")
     cat("  Css_trough = predicted trough conc (mg/L)      \n")
     cat("*************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amianhinfusioncpar<-data.frame(input=c("R (mg/hr)"),value=c(0))
     Amianhinfusioncpar<-edit(Amianhinfusioncpar)
     Amianhinfusioncpar<-check(Amianhinfusioncpar)
     C<-0.85*Amianhinfusioncpar[1,2]/(samplesStats("cl"))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhinfusionsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amianhinfusionsm.output(A,B,E,F,Amianhinfusioncpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("*************************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  R = constant intravenous infusion rate (mg/hr) \n")
     cat("*************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amianhinfusiondpar<-data.frame(input=c("Css_trough (mg/L)"),value=c(0))
     Amianhinfusiondpar<-edit(Amianhinfusiondpar)
     Amianhinfusiondpar<-check(Amianhinfusiondpar)
     d<-Amianhinfusiondpar[1,2]*samplesStats("cl")/0.85
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("R (mg/hr)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhinfusionsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amianhinfusionsm.output(A,B,E,F,Amianhinfusiondpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("*************************************************\n")
     cat("  --input data--                                 \n")
     cat("  R = constant intravenous infusion rate (mg/hr) \n")
     cat("                                                 \n")
     cat("  --output data--                                \n")
     cat("  Css_trough = predicted trough conc (mg/L)      \n")
     cat("*************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amianhinfusioncpar<-data.frame(input=c("R (mg/hr)"),value=c(0))
     Amianhinfusioncpar<-edit(Amianhinfusioncpar)
     Amianhinfusioncpar<-check(Amianhinfusioncpar)
     C<-0.85*Amianhinfusioncpar[1,2]/(samplesStats("cl"))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("*************************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  R = constant intravenous infusion rate (mg/hr) \n")
     cat("*************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amianhinfusiondpar<-data.frame(input=c("Css_trough (mg/L)"),value=c(0))
     Amianhinfusiondpar<-edit(Amianhinfusiondpar)
     Amianhinfusiondpar<-check(Amianhinfusiondpar)
     d<-Amianhinfusiondpar[1,2]*samplesStats("cl")/0.85
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("R (mg/hr)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhinfusionsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amianhinfusionsmcd.output(A,B,E,F,Amianhinfusioncpar,coutput,Amianhinfusiondpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Amianhinfusionsm.pkoutput(A,B,E,F)
        cal.again()
        }   
      }
    }  
  }
}


Amidihirsm.more<-function(A,B,E,F)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amidihircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amidihircpar<-edit(Amidihircpar)
     Amidihircpar<-check(Amidihircpar)
     C<-1.85*0.8*Amidihircpar[1,2]/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihircpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihircpar[2,2])-(1/(1-exp(-1.85*Amidihircpar[2,2])))*exp(-1.85*Amidihircpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amidihirsm.output(A,B,E,F,Amidihircpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amidihirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amidihirdpar<-edit(Amidihirdpar)
     Amidihirdpar<-check(Amidihirdpar)
     d<-Amidihirdpar[1,2]/(0.8*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirdpar[2,2])-(1/(1-exp(-1.85*Amidihirdpar[2,2])))*exp(-1.85*Amidihirdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amidihirsm.output(A,B,E,F,Amidihirdpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amidihircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amidihircpar<-edit(Amidihircpar)
     Amidihircpar<-check(Amidihircpar)
     C<-1.85*0.8*Amidihircpar[1,2]/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihircpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihircpar[2,2])-(1/(1-exp(-1.85*Amidihircpar[2,2])))*exp(-1.85*Amidihircpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amidihirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amidihirdpar<-edit(Amidihirdpar)
     Amidihirdpar<-check(Amidihirdpar)
     d<-Amidihirdpar[1,2]/(0.8*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirdpar[2,2])-(1/(1-exp(-1.85*Amidihirdpar[2,2])))*exp(-1.85*Amidihirdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihirsm.more(A,B)
       } else {
             if (pick == 2){
             Amidihirsmcd.output(A,B,E,F,Amidihircpar,coutput,Amidihirdpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Amidihirsm.pkoutput(A,B,E,F)
        cal.again()
        }   
      }
    }  
  }
}


Amidihinfusionsm.more<-function(A,B,E,F)
{
 cat("\n")
  file.menu <- c("R -> Css_trough",
                 "Css_trough -> R",
                 "Css_trough <-> R",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("*************************************************\n")
     cat("  --input data--                            \n")
     cat("  R = constant intravenous infusion rate (mg/hr) \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("*************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amidihinfusioncpar<-data.frame(input=c("R (mg/hr)"),value=c(0))
     Amidihinfusioncpar<-edit(Amidihinfusioncpar)
     Amidihinfusioncpar<-check(Amidihinfusioncpar)
     C<-0.8*Amidihinfusioncpar[1,2]/(samplesStats("cl"))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihinfusionsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amidihinfusionsm.output(A,B,E,F,Amidihinfusioncpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("*************************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  R = constant intravenous infusion rate (mg/hr) \n")
     cat("*************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amidihinfusiondpar<-data.frame(input=c("Css_trough (mg/L)"),value=c(0))
     Amidihinfusiondpar<-edit(Amidihinfusiondpar)
     Amidihinfusiondpar<-check(Amidihinfusiondpar)
     d<-Amidihinfusiondpar[1,2]*samplesStats("cl")/0.8
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("R (mg/hr)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihinfusionsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amidihinfusionsm.output(A,B,E,F,Amidihinfusiondpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("*************************************************\n")
     cat("  --input data--                            \n")
     cat("  R = constant intravenous infusion rate (mg/hr) \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("*************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amidihinfusioncpar<-data.frame(input=c("R (mg/hr)"),value=c(0))
     Amidihinfusioncpar<-edit(Amidihinfusioncpar)
     Amidihinfusioncpar<-check(Amidihinfusioncpar)
     C<-0.8*Amidihinfusioncpar[1,2]/(samplesStats("cl"))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("*************************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  R = constant intravenous infusion rate (mg/hr) \n")
     cat("*************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amidihinfusiondpar<-data.frame(input=c("Css_trough (mg/L)"),value=c(0))
     Amidihinfusiondpar<-edit(Amidihinfusiondpar)
     Amidihinfusiondpar<-check(Amidihinfusiondpar)
     d<-Amidihinfusiondpar[1,2]*samplesStats("cl")/0.8
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("R (mg/hr)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihinfusionsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Amidihinfusionsmcd.output(A,B,E,F,Amidihinfusioncpar,coutput,Amidihinfusiondpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Amidihinfusionsm.pkoutput(A,B,E,F)
        cal.again()
        }   
      }
    }  
  }
}


Oxtirsm.more<-function(A,B,E,F)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Oxtircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Oxtircpar<-edit(Oxtircpar)
     Oxtircpar<-check(Oxtircpar)
     C<-1.85*0.65*Oxtircpar[1,2]/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtircpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtircpar[2,2])-(1/(1-exp(-1.85*Oxtircpar[2,2])))*exp(-1.85*Oxtircpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Oxtirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Oxtirsm.output(A,B,E,F,Oxtircpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Oxtirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Oxtirdpar<-edit(Oxtirdpar)
     Oxtirdpar<-check(Oxtirdpar)
     d<-Oxtirdpar[1,2]/(0.65*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirdpar[2,2])-(1/(1-exp(-1.85*Oxtirdpar[2,2])))*exp(-1.85*Oxtirdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
     cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Oxtirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Oxtirsm.output(A,B,E,F,Oxtirdpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Oxtircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Oxtircpar<-edit(Oxtircpar)
     Oxtircpar<-check(Oxtircpar)
     C<-1.85*0.65*Oxtircpar[1,2]/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtircpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtircpar[2,2])-(1/(1-exp(-1.85*Oxtircpar[2,2])))*exp(-1.85*Oxtircpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Oxtirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Oxtirdpar<-edit(Oxtirdpar)
     Oxtirdpar<-check(Oxtirdpar)
     d<-Oxtirdpar[1,2]/(0.65*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirdpar[2,2])-(1/(1-exp(-1.85*Oxtirdpar[2,2])))*exp(-1.85*Oxtirdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
     cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Oxtirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Oxtirsmcd.output(A,B,E,F,Oxtircpar,coutput,Oxtirdpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Oxtirsm.pkoutput(A,B,E,F)
        cal.again()
        }   
      }
    }  
  }
}


Theirsm.more<-function(A,B,E,F)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Theircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Theircpar<-edit(Theircpar)
     Theircpar<-check(Theircpar)
     C<-1.85*Theircpar[1,2]/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theircpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theircpar[2,2])-(1/(1-exp(-1.85*Theircpar[2,2])))*exp(-1.85*Theircpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Theirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Theirsm.output(A,B,E,F,Theircpar,coutput)
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Theirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Theirdpar<-edit(Theirdpar)
     Theirdpar<-check(Theirdpar)
     d<-Theirdpar[1,2]/(1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirdpar[2,2])-(1/(1-exp(-1.85*Theirdpar[2,2])))*exp(-1.85*Theirdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Theirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Theirsm.output(A,B,E,F,Theirdpar,doutput)
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mg)                     \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Theircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Theircpar<-edit(Theircpar)
     Theircpar<-check(Theircpar)
     C<-1.85*Theircpar[1,2]/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theircpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theircpar[2,2])-(1/(1-exp(-1.85*Theircpar[2,2])))*exp(-1.85*Theircpar[2,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Theirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Theirdpar<-edit(Theirdpar)
     Theirdpar<-check(Theirdpar)
     d<-Theirdpar[1,2]/(1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirdpar[2,2])))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirdpar[2,2])-(1/(1-exp(-1.85*Theirdpar[2,2])))*exp(-1.85*Theirdpar[2,2])))
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Theirsm.more(A,B,E,F)
       } else {
             if (pick == 2){
             Theirsmcd.output(A,B,E,F,Theircpar,coutput,Theirdpar,doutput)
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
      	Theirsm.pkoutput(A,B,E,F)
        cal.again()
        }   
      }
    }  
  }
}



