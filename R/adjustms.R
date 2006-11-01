#estimate dose adjusment with mutiple conc.

Amims.more<-function()
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
     cat("           (x) button at upper right corner.                     \n\n")
     Amimscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0),tin=c(0))
     Amimscpar<-edit(Amimscpar)
     Amimscpar<-mscheck(Amimscpar)
     C<-(Amimscpar$D*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amimscpar$tin))/(Amimscpar$tin*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amimscpar$tau))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Amimscpar$tau-Amimscpar$tin))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amims.more()
       } else {
             if (pick == 2){
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
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amidpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0),tin=c(0))
     Amimsdpar<-edit(Amimsdpar)
     Amimsdpar<-mscheck(Amimsdpar)
     d<-Amimsdpar$Css/(((1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amimsdpar$tin))/(Amimsdpar$tin*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amimsdpar$tau))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Amimsdpar$tau-Amimsdpar$tin)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amims.more()
       } else {
             if (pick == 2){
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
     Amimscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0),tin=c(0))
     Amimscpar<-edit(Amimscpar)
     Amimscpar<-mscheck(Amimscpar)
     C<-(Amimscpar$D*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amimscpar$tin))/(Amimscpar$tin*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amimscpar$tau))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Amimscpar$tau-Amimscpar$tin))
     sim<-matrix(C[ ,1])
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
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amimsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0),tin=c(0))
     Amimsdpar<-edit(Amimsdpar)
     Amimsdpar<-mscheck(Amimsdpar)
     d<-Amimsdpar$Css/(((1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amimsdpar$tin))/(Amimsdpar$tin*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amimsdpar$tau))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Amimsdpar$tau-Amimsdpar$tin)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amims.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}

Vanms.more<-function()
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
     cat("           (x) button at upper right corner.                     \n\n")
     Vanmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0),tin=c(0))
     Vanmscpar<-edit(Vanmscpar)
     Vanmscpar<-mscheck(Vanmscpar)
     C<-(Vanmscpar$D*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vanmscpar$tin))/(Vanmscpar$tin*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vanmscpar$tau))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Vanmscpar$tau-Vanmscpar$tin))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Vanms.more()
       } else {
             if (pick == 2){
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
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Vandpar<-data.frame(subject=c(1,2),Css=c(0),tau=c(0),tin=c(0))
     Vanmsdpar<-edit(Vanmsdpar)
     Vanmsdpar<-mscheck(Vanmsdpar)
     d<-Vanmsdpar$Css/(((1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vanmsdpar$tin))/(Vanmsdpar$tin*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vanmsdpar$tau))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Vanmsdpar$tau-Vanmsdpar$tin)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Vanms.more()
       } else {
             if (pick == 2){
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
     Vanmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0),tin=c(0))
     Vanmscpar<-edit(Vanmscpar)
     Vanmscpar<-mscheck(Vanmscpar)
     C<-(Vanmscpar$D*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vanmscpar$tin))/(Vanmscpar$tin*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vanmscpar$tau))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Vanmscpar$tau-Vanmscpar$tin))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css (mg/L)")
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
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Vanmsdpar<-data.frame(subject=c(1,2),Css=c(0),tau=c(0),tin=c(0))
     Vanmsdpar<-edit(Vanmsdpar)
     Vanmsdpar<-mscheck(Vanmsdpar)
     d<-Vanmsdpar$Css/(((1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vanmsdpar$tin))/(Vanmsdpar$tin*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Vanmsdpar$tau))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Vanmsdpar$tau-Vanmsdpar$tin)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Vanms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}



Carms.more<-function()
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
     CBZmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     CBZmscpar<-edit(CBZmscpar)
     CBZmscpar<-mscheck(CBZmscpar)
     C<-1.2*CBZmscpar$D/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmscpar$tau)-(1/(1-exp(-1.2*CBZmscpar$tau)))*exp(-1.2*CBZmscpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Carms.more()
       } else {
             if (pick == 2){
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
     CBZmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     CBZmsdpar<-edit(CBZmsdpar)
     CBZmsdpar<-mscheck(CBZmsdpar)
     d<-CBZmsdpar$Css/(1.2/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmsdpar$tau)-(1/(1-exp(-1.2*CBZmscpar$tau)))*exp(-1.2*CBZmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Carms.more()
       } else {
             if (pick == 2){
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
     CBZmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     CBZmscpar<-edit(CBZmscpar)
     CBZmscpar<-mscheck(CBZmscpar)
     C<-1.2*CBZmscpar$D/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmscpar$tau)-(1/(1-exp(-1.2*CBZmscpar$tau)))*exp(-1.2*CBZmscpar$tau))
     sim<-matrix(C[ ,1])
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
     CBZmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     CBZmsdpar<-edit(CBZmsdpar)
     CBZmsdpar<-mscheck(CBZmsdpar)
     d<-CBZmsdpar$Css/(1.2/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmscpar$tau)-(1/(1-exp(-1.2*CBZmsdpar$tau)))*exp(-1.2*CBZmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Carms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}

Digms.more<-function()
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
     cat("  Css_trough = predicted trough conc (ng/mL)\n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Digmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Digmscpar<-edit(Digmscpar)
     Digmscpar<-mscheck(Digmscpar)
     C<-Digmscpar$D*1000/((samplesStats("cl_F"))*Digmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (ng/mL)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Digms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (ng/mL)  \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Digmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Digmsdpar<-edit(Digmsdpar)
     Digmsdpar<-mscheck(Digmsdpar)
     d<-Digmsdpar$Css*(samplesStats("cl_F"))*Digmsdpar$tau/1000
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Digms.more()
       } else {
             if (pick == 2){
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
     cat("  Css_trough = predicted trough conc (ng/mL)\n")
     cat("********************************************\n\n")
     cat("\n\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Digmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Digmscpar<-edit(Digmscpar)
     Digmscpar<-mscheck(Digmscpar)
     C<-Digmscpar$D*1000/((samplesStats("cl_F"))*Digmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (ng/mL)")
     cat("\n")
     show(coutput)
     cat("\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (ng/mL)  \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Digmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Digmsdpar<-edit(Digmsdpar)
     Digmsdpar<-mscheck(Digmsdpar)
     d<-Digmsdpar$Css*(samplesStats("cl_F"))*Digmsdpar$tau/1000
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Digms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}



Litms.more<-function()
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
     cat("  Css_trough = predicted trough conc (mEq/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Litmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Litmsdpar<-edit(Litmsdpar)
     Litmscpar<-mscheck(Litmscpar)
     C<-(Litmscpar$D/36.9458)/((samplesStats("cl_F"))*Litmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css (mEq/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Litms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mEq/L)  \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Litmsdpar<-data.frame(subject=c(1,2),Css=c(0),tau=c(0))
     Litmsdpar<-edit(Litmsdpar)
     Litmsdpar<-mscheck(Litmsdpar)
     d<-Litmsdpar$Css*(samplesStats("cl_F"))*Litmsdpar$tau*36.9458
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Litms.more()
       } else {
             if (pick == 2){
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
     cat("  Css_trough = predicted trough conc (mEq/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Litmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Litmscpar<-edit(Litmscpar)
     Litmscpar<-mscheck(Litmscpar)
     C<-(Litmscpar$D/36.9458)/((samplesStats("cl_F"))*Litmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css (mEq/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mEq/L)  \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Litmsdpar<-data.frame(subject=c(1,2),Css=c(0),tau=c(0))
     Litmsdpar<-edit(Litmsdpar)
     Litmsdpar<-mscheck(Litmsdpar)
     d<-Litmsdpar$Css*(samplesStats("cl_F"))*Litmsdpar$tau*36.9458
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Litms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again() 
        }   
      }
    }  
  }
}


Litcitms.more<-function()
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
     cat("  Css_trough = predicted trough conc (mEq/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Litcitmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Litcitmscpar<-edit(Litcitmscpar)
     Litcitmscpar<-mscheck(Litcitmscpar)
     C<-(Litcitmscpar$D/94)/((samplesStats("cl_F"))*Litcitmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css (mEq/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Litcitms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mEq/L)  \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Litcitmsdpar<-data.frame(subject=c(1,2),Css=c(0),tau=c(0))
     Litcitmsdpar<-edit(Litcitmsdpar)
     Litcitmsdpar<-mscheck(Litcitmsdpar)
     d<-Litcitmsdpar$Css*(samplesStats("cl_F"))*Litcitmsdpar$tau*94
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Litcitms.more()
       } else {
             if (pick == 2){
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
     cat("  Css_trough = predicted trough conc (mEq/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Litcitmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Litcitmscpar<-edit(Litcitmscpar)
     Litcitmscpar<-mscheck(Litcitmscpar)
     C<-(Litcitmscpar$D/94)/((samplesStats("cl_F"))*Litcitmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css (mEq/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mEq/L)  \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Litcitmsdpar<-data.frame(subject=c(1,2),Css=c(0),tau=c(0))
     Litcitmsdpar<-edit(Litcitmsdpar)
     Litcitmsdpar<-mscheck(Litcitmsdpar)
     d<-Litcitmsdpar$Css*(samplesStats("cl_F"))*Litcitmsdpar$tau*94
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Litcitms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again() 
        }   
      }
    }  
  }
}

Enfms.more<-function()
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
     Enfmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Enfmscpar<-edit(Enfmscpar)
     Enfmscpar<-mscheck(Enfmscpar)
     C<-(samplesStats("ka"))*Enfmscpar$D/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfmscpar$tau)-(1/(1-exp(-(samplesStats("ka"))*Enfmscpar$tau)))*exp(-(samplesStats("ka"))*Enfmscpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Enfms.more()
       } else {
             if (pick == 2){
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
     Enfmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Enfmsdpar<-edit(Enfmsdpar)
     Enfmsdpar<-mscheck(Enfmsdpar)
     d<-Enfmsdpar$Css/((samplesStats("ka"))/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfmsdpar$tau)-(1/(1-exp(-(samplesStats("ka"))*Enfmsdpar$tau)))*exp(-(samplesStats("ka"))*Enfmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Enfms.more()
       } else {
             if (pick == 2){
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
     Enfmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Enfmscpar<-edit(Enfmscpar)
     Enfmscpar<-mscheck(Enfmscpar)
     C<-(samplesStats("ka"))*Enfmscpar$D/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfmscpar$tau)-(1/(1-exp(-(samplesStats("ka"))*Enfmscpar$tau)))*exp(-(samplesStats("ka"))*Enfmscpar$tau))
     sim<-matrix(C[ ,1])
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
     Enfmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Enfmsdpar<-edit(Enfmsdpar)
     Enfmsdpar<-mscheck(Enfmsdpar)
     d<-Enfmsdpar$Css/((samplesStats("ka"))/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enfmsdpar$tau)-(1/(1-exp(-(samplesStats("ka"))*Enfmsdpar$tau)))*exp(-(samplesStats("ka"))*Enfmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Enfms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()  
        }   
      }
    }  
  }
}

Indms.more<-function()
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
     Indmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Indmscpar<-edit(Indmscpar)
     Indmscpar<-mscheck(Indmscpar)
     C<-(samplesStats("ka"))*Indmscpar$D/(65.7*((samplesStats("ka"))-(samplesStats("cl_F"))/65.7))*((1/(1-exp(-(samplesStats("cl_F"))/65.7*Indmscpar$tau)))*exp(-(samplesStats("cl_F"))/65.7*Indmscpar$tau)-(1/(1-exp(-(samplesStats("ka"))*Indmscpar$tau)))*exp(-(samplesStats("ka"))*Indmscpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Indms.more()
       } else {
             if (pick == 2){
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
     Indmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Indmsdpar<-edit(Indmsdpar)
     Indmsdpar<-mscheck(Indmsdpar)
     d<-Indmsdpar$Css/((samplesStats("ka"))/(65.7*((samplesStats("ka"))-(samplesStats("cl_F"))/65.7))*((1/(1-exp(-(samplesStats("cl_F"))/65.7*Indmsdpar$tau)))*exp(-(samplesStats("cl_F"))/65.7*Indmsdpar$tau)-(1/(1-exp(-(samplesStats("ka"))*Indmsdpar$tau)))*exp(-(samplesStats("ka"))*Indmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Indms.more()
       } else {
             if (pick == 2){
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
     Indmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Indmscpar<-edit(Indmscpar)
     Indmscpar<-mscheck(Indmscpar)
     C<-(samplesStats("ka"))*Indmscpar$D/(65.7*((samplesStats("ka"))-(samplesStats("cl_F"))/65.7))*((1/(1-exp(-(samplesStats("cl_F"))/65.7*Indmscpar$tau)))*exp(-(samplesStats("cl_F"))/65.7*Indmscpar$tau)-(1/(1-exp(-(samplesStats("ka"))*Indmscpar$tau)))*exp(-(samplesStats("ka"))*Indmscpar$tau))
     sim<-matrix(C[ ,1])
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
     Indmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Indmsdpar<-edit(Indmsdpar)
     Indmsdpar<-mscheck(Indmsdpar)
     d<-Indmsdpar$Css/((samplesStats("ka"))/(65.7*((samplesStats("ka"))-(samplesStats("cl_F"))/65.7))*((1/(1-exp(-(samplesStats("cl_F"))/65.7*Indmsdpar$tau)))*exp(-(samplesStats("cl_F"))/65.7*Indmsdpar$tau)-(1/(1-exp(-(samplesStats("ka"))*Indmsdpar$tau)))*exp(-(samplesStats("ka"))*Indmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Indms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()  
        }   
      }
    }  
  }
}


Ritms.more<-function()
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
     Ritmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Ritmscpar<-edit(Ritmscpar)
     Ritmscpar<-mscheck(Ritmscpar)
     C<-(samplesStats("ka"))*Ritmscpar$D/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Ritmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Ritmscpar$tau-0.778))-(1/(1-exp(-(samplesStats("ka"))*Ritmscpar$tau)))*exp(-(samplesStats("ka"))*(Ritmscpar$tau-0.778)))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Ritms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
    cat("\n")
     cat("*******************************************\n")
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
     Ritmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Ritmsdpar<-edit(Ritmsdpar)
     Ritmsdpar<-mscheck(Ritmsdpar)
     d<-Ritmsdpar$Css/((samplesStats("ka"))/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Ritmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Ritmsdpar$tau-0.778))-(1/(1-exp(-(samplesStats("ka"))*Ritmsdpar$tau)))*exp(-(samplesStats("ka"))*(Ritmsdpar$tau-0.778))))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Ritms.more()
       } else {
             if (pick == 2){
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
     Ritmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Ritmscpar<-edit(Ritmscpar)
     Ritmscpar<-mscheck(Ritmscpar)
     C<-(samplesStats("ka"))*Ritmscpar$D/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Ritmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Ritmscpar$tau-0.778))-(1/(1-exp(-(samplesStats("ka"))*Ritmscpar$tau)))*exp(-(samplesStats("ka"))*(Ritmscpar$tau-0.778)))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     readline()
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Ritmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Ritmsdpar<-edit(Ritmsdpar)
     Ritmsdpar<-mscheck(Ritmsdpar)
     d<-Ritmsdpar$Css/((samplesStats("ka"))/((samplesStats("v_F"))*((samplesStats("ka"))-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Ritmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Ritmsdpar$tau-0.778))-(1/(1-exp(-(samplesStats("ka"))*Ritmsdpar$tau)))*exp(-(samplesStats("ka"))*(Ritmsdpar$tau-0.778))))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Ritms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()  
        }   
      }
    }  
  }
}




Evems.more<-function()
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
     Evemscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Evemscpar<-edit(Evemscpar)
     Evemscpar<-mscheck(Evemscpar)
     C<-6.07*Evemscpar$D/((samplesStats("v_F"))*(6.07-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evemscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evemscpar$tau)-(1/(1-exp(-6.07*Evemscpar$tau)))*exp(-6.07*Evemscpar$tau))*1000
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mcg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Evems.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
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
     cat("           (x) button at upper right corner.                     \n\n")
     Evemsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Evemsdpar<-edit(Evemsdpar)
     Evemsdpar<-mscheck(Evemsdpar)
     d<-Evemsdpar$Css/(6.07/((samplesStats("v_F"))*(6.07-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evemsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evemsdpar$tau)-(1/(1-exp(-6.07*Evemsdpar$tau)))*exp(-6.07*Evemsdpar$tau))*1000)
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Evems.more()
       } else {
             if (pick == 2){
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
     cat("  Css_trough = predicted trough conc (mcg/L)\n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Evemscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Evemscpar<-edit(Evemscpar)
     Evemscpar<-mscheck(Evemscpar)
     C<-6.07*Evemscpar$D/((samplesStats("v_F"))*(6.07-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evemscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evemscpar$tau)-(1/(1-exp(-6.07*Evemscpar$tau)))*exp(-6.07*Evemscpar$tau))*1000
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mcg/L)")
     cat("\n")
     show(coutput)
     cat("\n\n")
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
     cat("           (x) button at upper right corner.                     \n\n")
     Evemsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Evemsdpar<-edit(Evemsdpar)
     Evemsdpar<-mscheck(Evemsdpar)
     d<-Evemsdpar$Css/(6.07/((samplesStats("v_F"))*(6.07-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evemsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Evemsdpar$tau)-(1/(1-exp(-6.07*Evemsdpar$tau)))*exp(-6.07*Evemsdpar$tau))*1000)
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Evems.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      if (pick == 4){
        cal.again()  
        }   
      }
    }  
  }
}

Tacms.more<-function()
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
     cat("           (x) button at upper right corner.                     \n\n")
     Tacmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Tacmscpar<-edit(Tacmscpar)
     Tacmscpar<-mscheck(Tacmscpar)
     C<-4.5*Tacmscpar$D/(314*(4.5-(samplesStats("cl_F"))/314))*((1/(1-exp(-(samplesStats("cl_F"))/314*Tacmscpar$tau)))*exp(-(samplesStats("cl_F"))/314*Tacmscpar$tau)-(1/(1-exp(-4.5*Tacmscpar$tau)))*exp(-4.5*Tacmscpar$tau))*1000
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mcg/mL)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Tacms.more()
       } else {
             if (pick == 2){
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
     Tacmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Tacmsdpar<-edit(Tacmsdpar)
     Tacmsdpar<-mscheck(Tacmsdpar)
     d<-Tacmsdpar$Css/(4.5/(314*(4.5-(samplesStats("cl_F"))/314))*((1/(1-exp(-(samplesStats("cl_F"))/314*Tacmsdpar$tau)))*exp(-(samplesStats("cl_F"))/314*Tacmsdpar$tau)-(1/(1-exp(-4.5*Tacmsdpar$tau)))*exp(-4.5*Tacmsdpar$tau))*1000)
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
      cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Tacms.more()
       } else {
             if (pick == 2){
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
     cat("  Css_trough = predicted trough conc (mcg/mL) \n")
     cat("*********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Tacmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Tacmscpar<-edit(Tacmscpar)
     Tacmscpar<-mscheck(Tacmscpar)
     C<-4.5*Tacmscpar$D/(314*(4.5-(samplesStats("cl_F"))/314))*((1/(1-exp(-(samplesStats("cl_F"))/314*Tacmscpar$tau)))*exp(-(samplesStats("cl_F"))/314*Tacmscpar$tau)-(1/(1-exp(-4.5*Tacmscpar$tau)))*exp(-4.5*Tacmscpar$tau))*1000
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mcg/mL)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("*********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mcg/mL) \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("*********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Tacmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Tacmsdpar<-edit(Tacmsdpar)
     Tacmsdpar<-mscheck(Tacmsdpar)
     d<-Tacmsdpar$Css/(4.5/(314*(4.5-(samplesStats("cl_F"))/314))*((1/(1-exp(-(samplesStats("cl_F"))/314*Tacmsdpar$tau)))*exp(-(samplesStats("cl_F"))/314*Tacmsdpar$tau)-(1/(1-exp(-4.5*Tacmsdpar$tau)))*exp(-4.5*Tacmsdpar$tau))*1000)
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Tacms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again() 
        }   
      }
    }  
  }
}


Enoms.more<-function()
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
      cat("\n")
     cat("*******************************************************************\n")
     cat("  --input data--                                                \n")
     cat("  D = desired dose (IU,1mg=100IU)                               \n")
     cat("  tau = desired dosing interval (hr)                            \n")
     cat("                                                                \n")
     cat("  --output data--                                               \n")
     cat("  Amax = predicted anti-Xa maximal activity at stedy state (IU/mL)  \n")
     cat("*******************************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Enomscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Enomscpar<-edit(Enomscpar)
     Enomscpar<-mscheck(Enomscpar)
     C<-Enomscpar$D*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(log(0.34*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enomscpar$tau))/((samplesStats("cl"))/(samplesStats("v_F"))*(1-exp(-0.34*Enomscpar$tau)))))/(0.34-(samplesStats("cl_F"))/(samplesStats("v_F"))))/(samplesStats("v_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enomscpar$tau))/1000
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Amax (IU/mL)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Enoms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("****************************************************************\n")
     cat("  --input data--                                                \n")
     cat("  Amax = desired anti-Xa maximal activity at stedy state (IU/mL)\n")
     cat("  tau = desired dosing interval (hr)                            \n")
     cat("                                                                \n")
     cat("  --output data--                                               \n")
     cat("  Dose = predicted dose (IU,1mg=100IU)                          \n")
     cat("****************************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Enomsdpar<-data.frame(subject=c(1,2),Amax=c(0),tau=c(0))
     Enomsdpar<-edit(Enomsdpar)
     Enomsdpar<-mscheck(Enomsdpar)
     d<-Enomsdpar$Amax/(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(log(0.34*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enomsdpar$tau))/((samplesStats("cl_F"))/(samplesStats("v_F"))*(1-exp(-0.34*Enomsdpar$tau)))))/(0.34-(samplesStats("cl_F"))/(samplesStats("v_F"))))/(samplesStats("v_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enomsdpar$tau))/1000)
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (IU)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Enoms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("*******************************************************************\n")
     cat("  --input data--                                                \n")
     cat("  D = desired dose (IU,1mg=100IU)                               \n")
     cat("  tau = desired dosing interval (hr)                            \n")
     cat("                                                                \n")
     cat("  --output data--                                               \n")
     cat("  Amax = predicted anti-Xa maximal activity at stedy state (IU/mL)  \n")
     cat("*******************************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Enomscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Enomscpar<-edit(Enomscpar)
     Enomscpar<-mscheck(Enomscpar)
     C<-Enomscpar$D*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(log(0.34*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enomscpar$tau))/((samplesStats("cl_F"))/(samplesStats("v_F"))*(1-exp(-0.34*Enomscpar$tau)))))/(0.34-(samplesStats("cl_F"))/(samplesStats("v_F"))))/(samplesStats("v_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enomscpar$tau))/1000
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Amax (IU/mL)")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("****************************************************************\n")
     cat("  --input data--                                                \n")
     cat("  Amax = desired anti-Xa maximal activity at stedy state (IU/mL)\n")
     cat("  tau = desired dosing interval (hr)                            \n")
     cat("                                                                \n")
     cat("  --output data--                                               \n")
     cat("  Dose = predicted dose (IU,1mg=100IU)                          \n")
     cat("****************************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Enomsdpar<-data.frame(subject=c(1,2),Amax=c(0),tau=c(0))
     Enomsdpar<-edit(Enomsdpar)
     Enomsdpar<-mscheck(Enomsdpar)
     d<-Enomsdpar$Amax/(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(log(0.34*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enomsdpar$tau))/((samplesStats("cl_F"))/(samplesStats("v_F"))*(1-exp(-0.34*Enomsdpar$tau)))))/(0.34-(samplesStats("cl_F"))/(samplesStats("v_F"))))/(samplesStats("v_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enomsdpar$tau))/1000)
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (IU)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Enoms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again() 
        }   
      }
    }  
  }
}


Carms.more<-function()
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
     CBZmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     CBZmscpar<-edit(CBZmscpar)
     CBZmscpar<-mscheck(CBZmscpar)
     C<-1.2*CBZmscpar$D/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmscpar$tau)-(1/(1-exp(-1.2*CBZmscpar$tau)))*exp(-1.2*CBZmscpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Carms.more()
       } else {
             if (pick == 2){
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
     cat("     Please enter all parameters values at Data Editor          \n\n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n")
     CBZmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     CBZmsdpar<-edit(CBZmsdpar)
     CBZmsdpar<-mscheck(CBZmsdpar)
     d<-CBZmsdpar$Css_trough/(1.2/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmsdpar$tau)-(1/(1-exp(-1.2*CBZmsdpar$tau)))*exp(-1.2*CBZmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Carms.more()
       } else {
             if (pick == 2){
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
     CBZmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     CBZmscpar<-edit(CBZmscpar)
     CBZmscpar<-mscheck(CBZmscpar)
     C<-1.2*CBZmscpar$D/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmscpar$tau)-(1/(1-exp(-1.2*CBZmscpar$tau)))*exp(-1.2*CBZmscpar$tau))
     sim<-matrix(C[ ,1])
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
     CBZmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     CBZmsdpar<-edit(CBZmsdpar)
     CBZmsdpar<-mscheck(CBZmsdpar)
     d<-CBZmsdpar$Css_trough/(1.2/((samplesStats("v_F"))*(1.2-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*CBZmsdpar$tau)-(1/(1-exp(-1.2*CBZmsdpar$tau)))*exp(-1.2*CBZmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Carms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}




# Amianhirms.more<-function()
Amianhirms.more<-function()
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
     Amianhirmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amianhirmscpar<-edit(Amianhirmscpar)
     Amianhirmscpar<-mscheck(Amianhirmscpar)
     C<-1.85*0.85*Amianhirmscpar$D/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirmscpar$tau)-(1/(1-exp(-1.85*Amianhirmscpar$tau)))*exp(-1.85*Amianhirmscpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhirms.more()
       } else {
             if (pick == 2){
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
     cat("     Please enter all parameters values at Data Editor          \n\n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n")
     Amianhirmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amianhirmsdpar<-edit(Amianhirmsdpar)
     Amianhirmsdpar<-mscheck(Amianhirmsdpar)
     d<-Amianhirmsdpar$Css_trough/(0.85*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirmsdpar$tau)-(1/(1-exp(-1.85*Amianhirmsdpar$tau)))*exp(-1.85*Amianhirmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhirms.more()
       } else {
             if (pick == 2){
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
     Amianhirmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amianhirmscpar<-edit(Amianhirmscpar)
     Amianhirmscpar<-mscheck(Amianhirmscpar)
     C<-1.85*0.85*Amianhirmscpar$D/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirmscpar$tau)-(1/(1-exp(-1.85*Amianhirmscpar$tau)))*exp(-1.85*Amianhirmscpar$tau))
     sim<-matrix(C[ ,1])
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
     Amianhirmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amianhirmsdpar<-edit(Amianhirmsdpar)
     Amianhirmsdpar<-mscheck(Amianhirmsdpar)
     d<-Amianhirmsdpar$Css_trough/(0.85*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amianhirmsdpar$tau)-(1/(1-exp(-1.85*Amianhirmsdpar$tau)))*exp(-1.85*Amianhirmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhirms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}


#Amianhcrms.more
Amianhcrms.more<-function()
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
     cat("  D = descred dose (mg)                     \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amianhcrmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amianhcrmscpar<-edit(Amianhcrmscpar)
     Amianhcrmscpar<-mscheck(Amianhcrmscpar)
     C<-Amianhcrmscpar$D*0.85/(samplesStats("cl_F")*Amianhcrmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhcrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = descred trough conc (mg/L)   \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n\n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n")
     Amianhcrmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amianhcrmsdpar<-edit(Amianhcrmsdpar)
     Amianhcrmsdpar<-mscheck(Amianhcrmsdpar)
     d<-Amianhcrmsdpar$Css_trough*samplesStats("cl_F")*Amianhcrmsdpar$tau/0.85
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhcrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = descred dose (mg)                     \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amianhcrmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amianhcrmscpar<-edit(Amianhcrmscpar)
     Amianhcrmscpar<-mscheck(Amianhcrmscpar)
     C<-Amianhcrmscpar$D*0.85/(samplesStats("cl_F")*Amianhcrmscpar$tau)
     sim<-matrix(C[ ,1])
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
     cat("  Css_trough = descred trough conc (mg/L)   \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amianhcrmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amianhcrmsdpar<-edit(Amianhcrmsdpar)
     Amianhcrmsdpar<-mscheck(Amianhcrmsdpar)
     d<-Amianhcrmsdpar$Css_trough*samplesStats("cl_F")*Amianhcrmsdpar$tau/0.85
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhcrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}


Amianhinfusionms.more<-function()
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
     Amianhinfusionmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amianhinfusionmscpar<-edit(Amianhinfusionmscpar)
     Amianhinfusionmscpar<-mscheck(Amianhinfusionmscpar)
     C<-Amianhinfusionmscpar$D*0.85/(samplesStats("cl_F")*Amianhinfusionmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhinfusionms.more()
       } else {
             if (pick == 2){
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
     Amianhinfusiondpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amianhinfusionmsdpar<-edit(Amianhinfusionmsdpar)
     Amianhinfusionmsdpar<-mscheck(Amianhinfusionmsdpar)
     d<-Amianhinfusionmsdpar$Css_trough*samplesStats("cl_F")*Amianhinfusionmsdpar$tau/0.85
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhinfusionms.more()
       } else {
             if (pick == 2){
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
     Amianhinfusionmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amianhinfusionmscpar<-edit(Amianhinfusionmscpar)
     Amianhinfusionmscpar<-mscheck(Amianhinfusionmscpar)
     C<-Amianhinfusionmscpar$D*0.85/(samplesStats("cl_F")*Amianhinfusionmscpar$tau)
     sim<-matrix(C[ ,1])
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
     Amianhinfusionmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amianhinfusionmsdpar<-edit(Amianhinfusionmsdpar)
     Amianhinfusionmsdpar<-mscheck(Amianhinfusionmsdpar)
     d<-Amianhinfusionmsdpar$Css_trough*samplesStats("cl_F")*Amianhinfusionmsdpar$tau/0.85
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amianhinfusionms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}


# Amidihirms.more<-function()
Amidihirms.more<-function()
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
     Amidihirmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amidihirmscpar<-edit(Amidihirmscpar)
     Amidihirmscpar<-mscheck(Amidihirmscpar)
     C<-1.85*0.8*Amidihirmscpar$D/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirmscpar$tau)-(1/(1-exp(-1.85*Amidihirmscpar$tau)))*exp(-1.85*Amidihirmscpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihirms.more()
       } else {
             if (pick == 2){
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
     cat("     Please enter all parameters values at Data Editor          \n\n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n")
     Amidihirmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amidihirmsdpar<-edit(Amidihirmsdpar)
     Amidihirmsdpar<-mscheck(Amidihirmsdpar)
     d<-Amidihirmsdpar$Css_trough/(0.8*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirmsdpar$tau)-(1/(1-exp(-1.85*Amidihirmsdpar$tau)))*exp(-1.85*Amidihirmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihirms.more()
       } else {
             if (pick == 2){
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
     Amidihirmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amidihirmscpar<-edit(Amidihirmscpar)
     Amidihirmscpar<-mscheck(Amidihirmscpar)
     C<-1.85*0.8*Amidihirmscpar$D/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirmscpar$tau)-(1/(1-exp(-1.85*Amidihirmscpar$tau)))*exp(-1.85*Amidihirmscpar$tau))
     sim<-matrix(C[ ,1])
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
     Amidihirmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amidihirmsdpar<-edit(Amidihirmsdpar)
     Amidihirmsdpar<-mscheck(Amidihirmsdpar)
     d<-Amidihirmsdpar$Css_trough/(0.8*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amidihirmsdpar$tau)-(1/(1-exp(-1.85*Amidihirmsdpar$tau)))*exp(-1.85*Amidihirmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihirms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}


#Amidihcrms.more
Amidihcrms.more<-function()
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
     cat("  D = descred dose (mg)                     \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amidihcrmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amidihcrmscpar<-edit(Amidihcrmscpar)
     Amidihcrmscpar<-mscheck(Amidihcrmscpar)
     C<-Amidihcrmscpar$D*0.8/(samplesStats("cl_F")*Amidihcrmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihcrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = descred trough conc (mg/L)   \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n\n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n")
     Amidihcrmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amidihcrmsdpar<-edit(Amidihcrmsdpar)
     Amidihcrmsdpar<-mscheck(Amidihcrmsdpar)
     d<-Amidihcrmsdpar$Css_trough*samplesStats("cl_F")*Amidihcrmsdpar$tau/0.8
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihcrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = descred dose (mg)                     \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amidihcrmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amidihcrmscpar<-edit(Amidihcrmscpar)
     Amidihcrmscpar<-mscheck(Amidihcrmscpar)
     C<-Amidihcrmscpar$D*0.8/(samplesStats("cl_F")*Amidihcrmscpar$tau)
     sim<-matrix(C[ ,1])
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
     cat("  Css_trough = descred trough conc (mg/L)   \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amidihcrmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amidihcrmsdpar<-edit(Amidihcrmsdpar)
     Amidihcrmsdpar<-mscheck(Amidihcrmsdpar)
     d<-Amidihcrmsdpar$Css_trough*samplesStats("cl_F")*Amidihcrmsdpar$tau/0.8
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihcrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}


Amidihinfusionms.more<-function()
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
     Amidihinfusionmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amidihinfusionmscpar<-edit(Amidihinfusionmscpar)
     Amidihinfusionmscpar<-mscheck(Amidihinfusionmscpar)
     C<-Amidihinfusionmscpar$D*0.8/(samplesStats("cl_F")*Amidihinfusionmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihinfusionms.more()
       } else {
             if (pick == 2){
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
     Amidihinfusiondpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amidihinfusionmsdpar<-edit(Amidihinfusionmsdpar)
     Amidihinfusionmsdpar<-mscheck(Amidihinfusionmsdpar)
     d<-Amidihinfusionmsdpar$Css_trough*samplesStats("cl_F")*Amidihinfusionmsdpar$tau/0.8
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihinfusionms.more()
       } else {
             if (pick == 2){
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
     Amidihinfusionmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Amidihinfusionmscpar<-edit(Amidihinfusionmscpar)
     Amidihinfusionmscpar<-mscheck(Amidihinfusionmscpar)
     C<-Amidihinfusionmscpar$D*0.8/(samplesStats("cl_F")*Amidihinfusionmscpar$tau)
     sim<-matrix(C[ ,1])
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
     Amidihinfusionmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Amidihinfusionmsdpar<-edit(Amidihinfusionmsdpar)
     Amidihinfusionmsdpar<-mscheck(Amidihinfusionmsdpar)
     d<-Amidihinfusionmsdpar$Css_trough*samplesStats("cl_F")*Amidihinfusionmsdpar$tau/0.8
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Amidihinfusionms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}

# Oxtirms.more<-function()
Oxtirms.more<-function()
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
     Oxtirmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Oxtirmscpar<-edit(Oxtirmscpar)
     Oxtirmscpar<-mscheck(Oxtirmscpar)
     C<-1.85*0.65*Oxtirmscpar$D/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirmscpar$tau)-(1/(1-exp(-1.85*Oxtirmscpar$tau)))*exp(-1.85*Oxtirmscpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Oxtirms.more()
       } else {
             if (pick == 2){
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
     cat("     Please enter all parameters values at Data Editor          \n\n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n")
     Oxtirmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Oxtirmsdpar<-edit(Oxtirmsdpar)
     Oxtirmsdpar<-mscheck(Oxtirmsdpar)
     d<-Oxtirmsdpar$Css_trough/(0.65*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirmsdpar$tau)-(1/(1-exp(-1.85*Oxtirmsdpar$tau)))*exp(-1.85*Oxtirmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Oxtirms.more()
       } else {
             if (pick == 2){
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
     Oxtirmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Oxtirmscpar<-edit(Oxtirmscpar)
     Oxtirmscpar<-mscheck(Oxtirmscpar)
     C<-1.85*0.65*Oxtirmscpar$D/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirmscpar$tau)-(1/(1-exp(-1.85*Oxtirmscpar$tau)))*exp(-1.85*Oxtirmscpar$tau))
     sim<-matrix(C[ ,1])
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
     Oxtirmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Oxtirmsdpar<-edit(Oxtirmsdpar)
     Oxtirmsdpar<-mscheck(Oxtirmsdpar)
     d<-Oxtirmsdpar$Css_trough/(0.65*1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Oxtirmsdpar$tau)-(1/(1-exp(-1.85*Oxtirmsdpar$tau)))*exp(-1.85*Oxtirmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Oxtirms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}


#Oxtcrms.more
Oxtcrms.more<-function()
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
     cat("  D = descred dose (mg)                     \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Oxtcrmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Oxtcrmscpar<-edit(Oxtcrmscpar)
     Oxtcrmscpar<-mscheck(Oxtcrmscpar)
     C<-0.65*Oxtcrmscpar$D/(samplesStats("cl_F")*Oxtcrmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Oxtcrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = descred trough conc (mg/L)   \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n\n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n")
     Oxtcrmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Oxtcrmsdpar<-edit(Oxtcrmsdpar)
     Oxtcrmsdpar<-mscheck(Oxtcrmsdpar)
     d<-Oxtcrmsdpar$Css_trough*samplesStats("cl_F")*Oxtcrmsdpar$tau/0.65
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Oxtcrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = descred dose (mg)                     \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Oxtcrmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Oxtcrmscpar<-edit(Oxtcrmscpar)
     Oxtcrmscpar<-mscheck(Oxtcrmscpar)
     C<-0.65*Oxtcrmscpar$D/(samplesStats("cl_F")*Oxtcrmscpar$tau)
     sim<-matrix(C[ ,1])
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
     cat("  Css_trough = descred trough conc (mg/L)   \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Oxtcrmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Oxtcrmsdpar<-edit(Oxtcrmsdpar)
     Oxtcrmsdpar<-mscheck(Oxtcrmsdpar)
     d<-Oxtcrmsdpar$Css_trough*samplesStats("cl_F")*Oxtcrmsdpar$tau/0.65
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Oxtcrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}




# Theirms.more<-function()
Theirms.more<-function()
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
     Theirmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Theirmscpar<-edit(Theirmscpar)
     Theirmscpar<-mscheck(Theirmscpar)
     C<-1.85*Theirmscpar$D/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirmscpar$tau)-(1/(1-exp(-1.85*Theirmscpar$tau)))*exp(-1.85*Theirmscpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Theirms.more()
       } else {
             if (pick == 2){
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
     cat("     Please enter all parameters values at Data Editor          \n\n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n")
     Theirmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Theirmsdpar<-edit(Theirmsdpar)
     Theirmsdpar<-mscheck(Theirmsdpar)
     d<-Theirmsdpar$Css_trough/(1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirmsdpar$tau)-(1/(1-exp(-1.85*Theirmsdpar$tau)))*exp(-1.85*Theirmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Theirms.more()
       } else {
             if (pick == 2){
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
     Theirmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Theirmscpar<-edit(Theirmscpar)
     Theirmscpar<-mscheck(Theirmscpar)
     C<-1.85*Theirmscpar$D/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirmscpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirmscpar$tau)-(1/(1-exp(-1.85*Theirmscpar$tau)))*exp(-1.85*Theirmscpar$tau))
     sim<-matrix(C[ ,1])
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
     Theirmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Theirmsdpar<-edit(Theirmsdpar)
     Theirmsdpar<-mscheck(Theirmsdpar)
     d<-Theirmsdpar$Css_trough/(1.85/((samplesStats("v_F"))*(1.85-(samplesStats("cl_F"))/(samplesStats("v_F"))))*((1/(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Theirmsdpar$tau)-(1/(1-exp(-1.85*Theirmsdpar$tau)))*exp(-1.85*Theirmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Theirms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}


#Thecrms.more
Thecrms.more<-function()
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
     cat("  D = descred dose (mg)                     \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Thecrmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Thecrmscpar<-edit(Thecrmscpar)
     Thecrmscpar<-mscheck(Thecrmscpar)
     C<-Thecrmscpar$D/(samplesStats("cl_F")*Thecrmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Thecrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = descred trough conc (mg/L)   \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n\n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n")
     Thecrmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Thecrmsdpar<-edit(Thecrmsdpar)
     Thecrmsdpar<-mscheck(Thecrmsdpar)
     d<-Thecrmsdpar$Css_trough*samplesStats("cl_F")*Thecrmsdpar$tau
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Thecrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
    cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = descred dose (mg)                     \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Thecrmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Thecrmscpar<-edit(Thecrmscpar)
     Thecrmscpar<-mscheck(Thecrmscpar)
     C<-Thecrmscpar$D/(samplesStats("cl_F")*Thecrmscpar$tau)
     sim<-matrix(C[ ,1])
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
     cat("  Css_trough = descred trough conc (mg/L)   \n")
     cat("  tau = descred dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Thecrmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Thecrmsdpar<-edit(Thecrmsdpar)
     Thecrmsdpar<-mscheck(Thecrmsdpar)
     d<-Thecrmsdpar$Css_trough*samplesStats("cl_F")*Thecrmsdpar$tau
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Thecrms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}



Theinfusionms.more<-function()
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
     Theinfusionmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Theinfusionmscpar<-edit(Theinfusionmscpar)
     Theinfusionmscpar<-mscheck(Theinfusionmscpar)
     C<-Theinfusionmscpar$D/(samplesStats("cl_F")*Theinfusionmscpar$tau)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Theinfusionms.more()
       } else {
             if (pick == 2){
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
     Theinfusiondpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Theinfusionmsdpar<-edit(Theinfusionmsdpar)
     Theinfusionmsdpar<-mscheck(Theinfusionmsdpar)
     d<-Theinfusionmsdpar$Css_trough*(samplesStats("cl_F"))*Theinfusionmsdpar$tau
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Theinfusionms.more()
       } else {
             if (pick == 2){
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
     Theinfusionmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Theinfusionmscpar<-edit(Theinfusionmscpar)
     Theinfusionmscpar<-mscheck(Theinfusionmscpar)
     C<-Theinfusionmscpar$D/(samplesStats("cl_F")*Theinfusionmscpar$tau)
     sim<-matrix(C[ ,1])
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
     Theinfusionmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Theinfusionmsdpar<-edit(Theinfusionmsdpar)
     Theinfusionmsdpar<-mscheck(Theinfusionmsdpar)
     d<-Theinfusionmsdpar$Css_trough*(samplesStats("cl_F"))*Theinfusionmsdpar$tau
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Theinfusionms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}


#PedDigms.more
PedDigms.more<-function(B)
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
     cat("  D = desired dose (mcg)                    \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mcg/L)\n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     PedDigmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     PedDigmscpar<-edit(PedDigmscpar)
     PedDigmscpar<-mscheck(PedDigmscpar)
     C<-PedDigmscpar$D*1000/((samplesStats("cl_F"))*PedDigmscpar$tau*B)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mcg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          PedDigms.more(B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mcg/L)  \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = predicted dose (mcg)               \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     PedDigmsdpar<-data.frame(subject=c(1,2),Css=c(0),tau=c(0))
     PedDigmsdpar<-edit(PedDigmsdpar)
     PedDigmsdpar<-mscheck(PedDigmsdpar)
     d<-PedDigmsdpar$Css*(samplesStats("cl_F"))*PedDigmsdpar$tau*B/1000
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mcg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          PedDigms.more(B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  D = desired dose (mcg)                    \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Css_trough = predicted trough conc (mcg/L)\n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     PedDigmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     PedDigmscpar<-edit(PedDigmscpar)
     PedDigmscpar<-mscheck(PedDigmscpar)
     C<-PedDigmscpar$D*1000/((samplesStats("cl_F"))*PedDigmscpar$tau*B)
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mcg/L)")
     cat("\n")
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
     cat("  Dose = predicted dose (mcg)               \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     PedDigmsdpar<-data.frame(subject=c(1,2),Css=c(0),tau=c(0))
     PedDigmsdpar<-edit(PedDigmsdpar)
     PedDigmsdpar<-mscheck(PedDigmsdpar)
     d<-PedDigmsdpar$Css*(samplesStats("cl_F"))*PedDigmsdpar$tau*B/1000
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mcg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          PedDigms.more(B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again() 
        }   
      }
    }  
  }
  }
  

#Valms.more
Valms.more<-function(B)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n\n")
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
     cat("           (x) button at upper right corner.                    \n\n")
     Valmmcpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Valmmcpar<-edit(Valmmcpar)
     Valmmcpar<-mscheck(Valmmcpar)
     C<-B*Valmmcpar$D/(11.5*(B-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valmmcpar$tau)))*exp(-(samplesStats("cl_F"))/11.5*Valmmcpar$tau)-(1/(1-exp(-B*Valmmcpar$tau)))*exp(-B*Valmmcpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough(mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Valms.more(B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n\n")
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
     cat("           (x) button at upper right corner.                    \n\n")
     Valmmdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Valmmdpar<-edit(Valmmdpar)
     Valmmdpar<-mscheck(Valmmdpar)
     d<-Valmmdpar$Css/(B/(11.5*(B-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valmmdpar$tau)))*exp(-(samplesStats("cl_F"))/11.5*Valmmdpar$tau)-(1/(1-exp(-B*Valmmdpar$tau)))*exp(-B*Valmmdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose(mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Valms.more(B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
    cat("\n")
     cat("********************************************\n\n")
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
     cat("           (x) button at upper right corner.                    \n\n")
     Valmmcpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Valmmcpar<-edit(Valmmcpar)
     Valmmcpar<-mscheck(Valmmcpar)
     C<-B*Valmmcpar$D/(11.5*(B-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valmmcpar$tau)))*exp(-(samplesStats("cl_F"))/11.5*Valmmcpar$tau)-(1/(1-exp(-B*Valmmcpar$tau)))*exp(-B*Valmmcpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough(mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n\n")
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
     cat("           (x) button at upper right corner.                    \n\n")
     Valmmdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Valmmdpar<-edit(Valmmdpar)
     Valmmdpar<-mscheck(Valmmdpar)
     d<-Valmmdpar$Css/(B/(11.5*(B-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valmmdpar$tau)))*exp(-(samplesStats("cl_F"))/11.5*Valmmdpar$tau)-(1/(1-exp(-B*Valmmdpar$tau)))*exp(-B*Valmmdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose(mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Valms.more(B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
  }
  
  
#Cycms,more()
Cycms.more<-function(A,B){
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
     cat("  Css_trough = predicted trough conc (mcg/L)\n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Cycmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Cycmscpar<-edit(Cycmscpar)
     Cycmscpar<-mscheck(Cycmscpar)
     C<-(0.2+10*abs(A-7)/((A+10)*60))*Cycmscpar$D*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cycmscpar$tau)))*exp(-(samplesStats("cl_F"))/(4*B)*Cycmscpar$tau)-(1/(1-exp(-0.3*Cycmscpar$tau)))*exp(-0.3*Cycmscpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough(mcg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Cycms.more(A,B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
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
     Cycmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Cycmsdpar<-edit(Cycmsdpar)
     Cycmsdpar<-mscheck(Cycmsdpar)
     d<-Cycmsdpar$Css/((0.2+10*abs(A-7)/((A+10)*60))*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cycmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(4*B)*Cycmsdpar$tau)-(1/(1-exp(-0.3*Cycmsdpar$tau)))*exp(-0.3*Cycmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose(mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Cycms.more(A,B)
       } else {
             if (pick == 2){
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
     cat("  Css_trough = predicted trough conc (mcg/L)\n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Cycmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Cycmscpar<-edit(Cycmscpar)
     Cycmscpar<-mscheck(Cycmscpar)
     C<-(0.2+10*abs(A-7)/((A+10)*60))*Cycmscpar$D*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cycmscpar$tau)))*exp(-(samplesStats("cl_F"))/(4*B)*Cycmscpar$tau)-(1/(1-exp(-0.3*Cycmscpar$tau)))*exp(-0.3*Cycmscpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough(mcg/L)")
     cat("\n")
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
     Cycmsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Cycmsdpar<-edit(Cycmsdpar)
     Cycmsdpar<-mscheck(Cycmsdpar)
     d<-Cycmsdpar$Css/((0.2+10*abs(A-7)/((A+10)*60))*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cycmsdpar$tau)))*exp(-(samplesStats("cl_F"))/(4*B)*Cycmsdpar$tau)-(1/(1-exp(-0.3*Cycmsdpar$tau)))*exp(-0.3*Cycmsdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose(mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Cycms.more(A,B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()  
        }   
      }
    }  
  }
}


# Imatinib mesylate
Imams.more<-function()
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
     Imamscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Imamscpar<-edit(Imamscpar)
     Imamscpar<-mscheck(Imamscpar)
     C<-((Imamscpar$D/(1.5*(samplesStats("cl_F"))))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Imamscpar$tau-1.5))))/(1-(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Imamscpar$tau)))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Imams.more()
       } else {
             if (pick == 2){
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
     cat("     Please enter all parameters values at Data Editor          \n\n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n")
     Imamsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Imamsdpar<-edit(Imamsdpar)
     Imamsdpar<-mscheck(Imamsdpar)
     d<-Imamsdpar$Css_trough/(((1/(1.5*(samplesStats("cl_F"))))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Imamsdpar$tau-1.5))))/(1-(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Imamsdpar$tau))))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Imams.more()
       } else {
             if (pick == 2){
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
     Imamscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Imamscpar<-edit(Imamscpar)
     Imamscpar<-mscheck(Imamscpar)
     C<-((Imamscpar$D/(1.5*(samplesStats("cl_F"))))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Imamscpar$tau-1.5))))/(1-(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Imamscpar$tau)))
     sim<-matrix(C[ ,1])
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
     Imamsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Imamsdpar<-edit(Imamsdpar)
     Imamsdpar<-mscheck(Imamsdpar)
     d<-Imamsdpar$Css_trough/(((1/(1.5*(samplesStats("cl_F"))))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*1.5))*(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Imamsdpar$tau-1.5))))/(1-(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Imamsdpar$tau))))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Imams.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}



#ChiValms.more
ChiValms.more<-function(B)
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("********************************************\n\n")
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
     cat("           (x) button at upper right corner.                    \n\n")
     ChiValmmcpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     ChiValmmcpar<-edit(ChiValmmcpar)
     ChiValmmcpar<-mscheck(ChiValmmcpar)
     C<-1.9*ChiValmmcpar$D/((0.24*B)*(1.9-(samplesStats("cl_F"))/(0.24*B)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*B)*ChiValmmcpar$tau)))*exp(-(samplesStats("cl_F"))/(0.24*B)*ChiValmmcpar$tau)-(1/(1-exp(-1.9*ChiValmmcpar$tau)))*exp(-1.9*ChiValmmcpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough(mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          ChiValms.more(B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n\n")
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
     cat("           (x) button at upper right corner.                    \n\n")
     ChiValmmdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     ChiValmmdpar<-edit(ChiValmmdpar)
     ChiValmmdpar<-mscheck(ChiValmmdpar)
     d<-ChiValmmdpar$Css/(1.9/((0.24*B)*(1.9-(samplesStats("cl_F"))/(0.24*B)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*B)*ChiValmmdpar$tau)))*exp(-(samplesStats("cl_F"))/(0.24*B)*ChiValmmdpar$tau)-(1/(1-exp(-1.9*ChiValmmdpar$tau)))*exp(-1.9*ChiValmmdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose(mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          ChiValms.more(B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
    cat("\n")
     cat("********************************************\n\n")
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
     cat("           (x) button at upper right corner.                    \n\n")
     ChiValmmcpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     ChiValmmcpar<-edit(ChiValmmcpar)
     ChiValmmcpar<-mscheck(ChiValmmcpar)
     C<-1.9*ChiValmmcpar$D/((0.24*B)*(1.9-(samplesStats("cl_F"))/(0.24*B)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*B)*ChiValmmcpar$tau)))*exp(-(samplesStats("cl_F"))/(0.24*B)*ChiValmmcpar$tau)-(1/(1-exp(-1.9*ChiValmmcpar$tau)))*exp(-1.9*ChiValmmcpar$tau))
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough(mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("********************************************\n\n")
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
     cat("           (x) button at upper right corner.                    \n\n")
     ChiValmmdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     ChiValmmdpar<-edit(ChiValmmdpar)
     ChiValmmdpar<-mscheck(ChiValmmdpar)
     d<-ChiValmmdpar$Css/(1.9/((0.24*B)*(1.9-(samplesStats("cl_F"))/(0.24*B)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*B)*ChiValmmdpar$tau)))*exp(-(samplesStats("cl_F"))/(0.24*B)*ChiValmmdpar$tau)-(1/(1-exp(-1.9*ChiValmmdpar$tau)))*exp(-1.9*ChiValmmdpar$tau)))
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose(mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          ChiValms.more(B)
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
  }
  
  
Phems.more<-function()
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
     cat("\n\n")
     cat("*******************************************************\n")
     cat("      ATTENTION :                                      \n")
     cat("      Desired daily dose must be less than Vmax.       \n")
     cat("*******************************************************\n")
     cat("\n\n")
     cat("          Pressing Enter to continue..                 \n")
     readline()
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Phemscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Phemscpar<-edit(Phemscpar)
     Phemscpar<-mscheck(Phemscpar)
     C<-(Phemscpar$D*24/Phemscpar$tau)*(samplesStats("Km"))/((samplesStats("Vmax"))-Phemscpar$D*24/Phemscpar$tau) 
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Phems.more()
       } else {
             if (pick == 2){
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
     Phemsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Phemsdpar<-edit(Phemsdpar)
     Phemsdpar<-mscheck(Phemsdpar)
     d<-((samplesStats("Vmax"))*Phemsdpar$Css)/((samplesStats("Km"))+Phemsdpar$Css)/(24/Phemsdpar$tau)
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Phems.more()
       } else {
             if (pick == 2){
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
     cat("*******************************************************\n")
     cat("      ATTENTION :                                      \n")
     cat("      Desired daily dose must be less than Vmax.       \n")
     cat("*******************************************************\n")
     cat("\n\n")
     cat("          Pressing Enter to continue..                 \n")
     readline()
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Phemscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Phemscpar<-edit(Phemscpar)
     Phemscpar<-mscheck(Phemscpar)
     C<-(Phemscpar$D*24/Phemscpar$tau)*(samplesStats("Km"))/((samplesStats("Vmax"))-Phemscpar$D*24/Phemscpar$tau) 
     sim<-matrix(C[ ,1])
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
     Phemsdpar<-data.frame(subject=c(1,2),Css_trough=c(0),tau=c(0))
     Phemsdpar<-edit(Phemsdpar)
     Phemsdpar<-mscheck(Phemsdpar)
     d<-((samplesStats("Vmax"))*Phemsdpar$Css)/((samplesStats("Km"))+Phemsdpar$Css)/(24/Phemsdpar$tau)
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Phems.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}


Warms.more<-function()
{
 cat("\n")
  file.menu <- c("Dose -> INR",
                 "INR -> Dose",
                 "Dose <-> INR",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     cat("**************************************************\n")
     cat("  --input data--                                  \n")
     cat("  D = desired dose (mg)                           \n")
     cat("  tau = desired dosing interval (day)             \n")
     cat("                                                  \n")
     cat("  --output data--                                 \n")
     cat("  INR = predicted international normalized ratio  \n")
     cat("**************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Warmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Warmscpar<-edit(Warmscpar)
     Warmscpar<-mscheck(Warmscpar)
     C<-Warcpr(Warmscpar$D,Warmscpar$tau)  
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("INR")
     cat("\n")
     show(coutput)  
     cat("\n") 
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Warms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
  } else {
    if (pick == 2){
    cat("\n")
     cat("**************************************************\n")
     cat("  --input data--                                  \n")
     cat("  INR = predicted international normalized ratio  \n")
     cat("  tau = desired dosing interval (day)             \n")
     cat("                                                  \n")
     cat("  --output data--                                 \n")
     cat("  Dose = desired/predicted dose (mg)              \n")
     cat("**************************************************\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Warmsdpar<-data.frame(subject=c(1,2),INR=c(0),tau=c(0))
     Warmsdpar<-edit(Warmsdpar)
     Warmsdpar<-mscheck(Warmsdpar)
     d<-Wardpr(Warmsdpar$INR,Warmsdpar$tau)  
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)  
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Warms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
    } else {
    if (pick == 3){
     cat("\n")
     cat("**************************************************\n")
     cat("  --input data--                                  \n")
     cat("  D = desired dose (mg)                           \n")
     cat("  tau = desired dosing interval (hr)              \n")
     cat("                                                  \n")
     cat("  --output data--                                 \n")
     cat("  INR = predicted international normalized ratio  \n")
     cat("**************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Warmscpar<-data.frame(subject=c(1,2),D=c(0),tau=c(0))
     Warmscpar<-edit(Warmscpar)
     Warmscpar<-mscheck(Warmscpar)
     C<-Warcpr(Warmscpar$D,Warmscpar$tau)  
     sim<-matrix(C[ ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("INR (mg/L)")
     cat("\n")
     show(coutput) 
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
     cat("\n")
     cat("**************************************************\n")
     cat("  --input data--                                  \n")
     cat("  INR = predicted international normalized ratio  \n")
     cat("  tau = desired dosing interval (hr)              \n")
     cat("                                                  \n")
     cat("  --output data--                                 \n")
     cat("  Dose = desired/predicted dose (mg)              \n")
     cat("**************************************************\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Warmsdpar<-data.frame(subject=c(1,2),INR=c(0),tau=c(0))
     Warmsdpar<-edit(Warmsdpar)
     Warmsdpar<-mscheck(Warmsdpar)
     d<-Wardpr(Warmsdpar$INR,Warmsdpar$tau)  
     sim<-matrix(d[ ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)  
      cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Warms.more()
       } else {
             if (pick == 2){
             cal.again()
             }
         }  
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}