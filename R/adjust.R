Ami.more<-function()
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
     Amicpar<-data.frame(input=c("D (mg)","tau (hr)","tin (hr)"),value=c(0))
     Amicpar<-edit(Amicpar)
     Amicpar<-check(Amicpar)
     C<-(Amicpar[1,2]*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amicpar[3,2]))/(Amicpar[3,2]*(samplesStats("cl_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Amicpar[2,2]))))*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(Amicpar[2,2]-Amicpar[3,2]))
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
       cat("\n")                          # dose adjustment again or not
       file.menu <- c("Yes",
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Ami.more()
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
       file.menu <- c("Yes",
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Ami.more()
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
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Ami.more()
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

Van.more<-function()
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
          Van.more()
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
          Van.more()
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
          Van.more()
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


Car.more<-function()
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
          Car.more()
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
          Car.more()
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
          Car.more()
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


Dig.more<-function()
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
     Digcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Digcpar<-edit(Digcpar)
     Digcpar<-check(Digcpar)
     C<-Digcpar[1,2]*1000/((samplesStats("cl_F"))*Digcpar[2,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (ng/mL)")
     cat("\n")
     show(coutput)
       cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Dig.more()
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
     Digdpar<-data.frame(input=c("Css_trough (ng/mL)","tau (hr)"),value=c(0))
     Digdpar<-edit(Digdpar)
     Digdpar<-check(Digdpar)
     d<-Digdpar[1,2]*(samplesStats("cl_F"))*Digdpar[2,2]/1000
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
          Dig.more()
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
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Digcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Digcpar<-edit(Digcpar)
     Digcpar<-check(Digcpar)
     C<-Digcpar[1,2]*1000/((samplesStats("cl_F"))*Digcpar[2,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (ng/mL)")
     cat("\n")
     show(coutput)
     cat("\n\n")
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
     Digdpar<-data.frame(input=c("Css_trough (ng/mL)","tau (hr)"),value=c(0))
     Digdpar<-edit(Digdpar)
     Digdpar<-check(Digdpar)
     d<-Digdpar[1,2]*(samplesStats("cl_F"))*Digdpar[2,2]/1000
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
          Dig.more()
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



Lit.more<-function()
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
     Litcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Litcpar<-edit(Litcpar)
     Litcpar<-check(Litcpar)
     C<-(Litcpar[1,2]/36.9458)/((samplesStats("cl_F"))*Litcpar[2,2])
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
          Lit.more()
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
     Litdpar<-data.frame(input=c("Css (mEq/L)","tau (hr)"),value=c(0))
     Litdpar<-edit(Litdpar)
     Litdpar<-check(Litdpar)
     d<-Litdpar[1,2]*(samplesStats("cl_F"))*Litdpar[2,2]*36.9458
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
          Lit.more()
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
     Litcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Litcpar<-edit(Litcpar)
     Litcpar<-check(Litcpar)
     C<-(Litcpar[1,2]/36.9458)/((samplesStats("cl_F"))*Litcpar[2,2])
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
     cat("           (x) button at upper right corner.                    \n\n")
     Litdpar<-data.frame(input=c("Css (mEq/L)","tau (hr)"),value=c(0))
     Litdpar<-edit(Litdpar)
     Litdpar<-check(Litdpar)
     d<-Litdpar[1,2]*(samplesStats("cl_F"))*Litdpar[2,2]*36.9458
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
          Lit.more()
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

Litcit.more<-function()
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
     Litcitcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Litcitcpar<-edit(Litcitcpar)
     Litcitcpar<-check(Litcitcpar)
     C<-(Litcitcpar[1,2]/94)/((samplesStats("cl_F"))*Litcitcpar[2,2])
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
          Litcit.more()
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
     Litcitdpar<-data.frame(input=c("Css (mEq/L)","tau (hr)"),value=c(0))
     Litcitdpar<-edit(Litcitdpar)
     Litcitdpar<-check(Litcitdpar)
     d<-Litcitdpar[1,2]*(samplesStats("cl_F"))*Litcitdpar[2,2]*94
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
          Litcit.more()
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
     Litcitcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Litcitcpar<-edit(Litcitcpar)
     Litcitcpar<-check(Litcitcpar)
     C<-(Litcitcpar[1,2]/94)/((samplesStats("cl_F"))*Litcitcpar[2,2])
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
     cat("           (x) button at upper right corner.                    \n\n")
     Litcitdpar<-data.frame(input=c("Css (mEq/L)","tau (hr)"),value=c(0))
     Litcitdpar<-edit(Litcitdpar)
     Litcitdpar<-check(Litcitdpar)
     d<-Litcitdpar[1,2]*(samplesStats("cl_F"))*Litcitdpar[2,2]*94
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
          Litcit.more()
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


Enf.more<-function()
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
          Enf.more()
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
          Enf.more()
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
          Enf.more()
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

Ind.more<-function()
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
          Ind.more()
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
          Ind.more()
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
          Ind.more()
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


Rit.more<-function()
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
          Rit.more()
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
          Rit.more()
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
          Rit.more()
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




Eve.more<-function()
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
          Eve.more()
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
          Eve.more()
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
          Eve.more()
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

Tac.more<-function()
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
          Tac.more()
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
          Tac.more()
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
          Tac.more()
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


Eno.more<-function()
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
     Enocpar<-data.frame(input=c("D (IU)","tau (hr)"),value=c(0))
     Enocpar<-edit(Enocpar)
     Enocpar<-check(Enocpar)
     C<-Enocpar[1,2]*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(log(0.34*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enocpar[2,2]))/((samplesStats("cl_F"))/(samplesStats("v_F"))*(1-exp(-0.34*Enocpar[2,2])))))/(0.34-(samplesStats("cl_F"))/(samplesStats("v_F"))))/(samplesStats("v_F"))*(1-exp(-(samplesStats("c_Fl"))/(samplesStats("v_F"))*Enocpar[2,2]))/1000
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Amax (IU/mL)")
     cat("\n")
     show(coutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Eno.more()
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
     Enodpar<-data.frame(input=c("Amax (IU/mL)","tau (hr)"),value=c(0))
     Enodpar<-edit(Enodpar)
     Enodpar<-check(Enodpar)
     d<-Enodpar[1,2]/(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(log(0.34*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enodpar[2,2]))/((samplesStats("cl_F"))/(samplesStats("v_F"))*(1-exp(-0.34*Enodpar[2,2])))))/(0.34-(samplesStats("cl_F"))/(samplesStats("v_F"))))/(samplesStats("v_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enodpar[2,2]))/1000)
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (IU)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Eno.more()
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
     Enocpar<-data.frame(input=c("D (IU)","tau (hr)"),value=c(0))
     Enocpar<-edit(Enocpar)
     Enocpar<-check(Enocpar)
     C<-Enocpar[1,2]*exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(log(0.34*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enocpar[2,2]))/((samplesStats("cl_F"))/(samplesStats("v_F"))*(1-exp(-0.34*Enocpar[2,2])))))/(0.34-(samplesStats("cl_F"))/(samplesStats("v_F"))))/(samplesStats("v_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enocpar[2,2]))/1000
     sim<-matrix(C[1 ,1])
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
     Enodpar<-data.frame(input=c("Amax (IU/mL)","tau (hr)"),value=c(0))
     Enodpar<-edit(Enodpar)
     Enodpar<-check(Enodpar)
     d<-Enodpar[1,2]/(exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*(log(0.34*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enodpar[2,2]))/((samplesStats("cl_F"))/(samplesStats("v_F"))*(1-exp(-0.34*Enodpar[2,2])))))/(0.34-(samplesStats("cl_F"))/(samplesStats("v_F"))))/(samplesStats("v_F"))*(1-exp(-(samplesStats("cl_F"))/(samplesStats("v_F"))*Enodpar[2,2]))/1000)
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (IU)")
     cat("\n")
     show(doutput)
        cat("\n")
       file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          Eno.more()
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



Amianhir.more<-function()
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
          Amianhir.more()
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
          Amianhir.more()
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
          Amianhir.more()
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



Amianhcr.more<-function()
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
     Amianhcrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amianhcrcpar<-edit(Amianhcrcpar)
     Amianhcrcpar<-check(Amianhcrcpar)
     C<-0.85*Amianhcrcpar[1,2]/(samplesStats("cl_F")*Amianhcrcpar[2,2])
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
          Amianhcr.more()
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
     Amianhcrdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amianhcrdpar<-edit(Amianhcrdpar)
     Amianhcrdpar<-check(Amianhcrdpar)
     d<-Amianhcrdpar[1,2]*samplesStats("cl_F")*Amianhcrdpar[2,2]/0.85
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
          Amianhcr.more()
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
     Amianhcrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amianhcrcpar<-edit(Amianhcrcpar)
     Amianhcrcpar<-check(Amianhcrcpar)
     C<-0.85*Amianhcrcpar[1,2]/(samplesStats("cl_F")*Amianhcrcpar[2,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
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
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amianhcrdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amianhcrdpar<-edit(Amianhcrdpar)
     Amianhcrdpar<-check(Amianhcrdpar)
     d<-Amianhcrdpar[1,2]*samplesStats("cl_F")*Amianhcrdpar[2,2]/0.85
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
          Amianhcr.more()
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

Amianhinfusion.more<-function()
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
     cat("           (x) button at upper right corner.                    \n\n")
     Amianhinfusioncpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amianhinfusioncpar<-edit(Amianhinfusioncpar)
     Amianhinfusioncpar<-check(Amianhinfusioncpar)
     C<-0.85*Amianhinfusioncpar[1,2]/(samplesStats("cl_F")*Amianhinfusioncpar[2,2])
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
          Amianhinfusion.more()
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
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amianhinfusiondpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amianhinfusiondpar<-edit(Amianhinfusiondpar)
     Amianhinfusiondpar<-check(Amianhinfusiondpar)
     d<-Amianhinfusiondpar[1,2]*samplesStats("cl_F")*Amianhinfusiondpar[2,2]/0.85
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
          Amianhinfusion.more()
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
     cat("           (x) button at upper right corner.                    \n\n")
     Amianhinfusioncpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amianhinfusioncpar<-edit(Amianhinfusioncpar)
     Amianhinfusioncpar<-check(Amianhinfusioncpar)
     C<-0.85*Amianhinfusioncpar[1,2]/(samplesStats("cl_F")*Amianhinfusioncpar[2,2])
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
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amianhinfusiondpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amianhinfusiondpar<-edit(Amianhinfusiondpar)
     Amianhinfusiondpar<-check(Amianhinfusiondpar)
     d<-Amianhinfusiondpar[1,2]*samplesStats("cl_F")*Amianhinfusiondpar[2,2]/0.85
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
          Amianhinfusion.more()
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



Amidihir.more<-function()
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
          Amidihir.more()
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
          Amidihir.more()
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
          Amidihir.more()
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



Amidihcr.more<-function()
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
     Amidihcrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amidihcrcpar<-edit(Amidihcrcpar)
     Amidihcrcpar<-check(Amidihcrcpar)
     C<-0.8*Amidihcrcpar[1,2]/(samplesStats("cl_F")*Amidihcrcpar[2,2])
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
          Amidihcr.more()
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
     Amidihcrdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amidihcrdpar<-edit(Amidihcrdpar)
     Amidihcrdpar<-check(Amidihcrdpar)
     d<-Amidihcrdpar[1,2]*samplesStats("cl_F")*Amidihcrdpar[2,2]/0.8
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
          Amidihcr.more()
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
     Amidihcrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amidihcrcpar<-edit(Amidihcrcpar)
     Amidihcrcpar<-check(Amidihcrcpar)
     C<-0.8*Amidihcrcpar[1,2]/(samplesStats("cl_F")*Amidihcrcpar[2,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
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
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Amidihcrdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amidihcrdpar<-edit(Amidihcrdpar)
     Amidihcrdpar<-check(Amidihcrdpar)
     d<-Amidihcrdpar[1,2]*samplesStats("cl_F")*Amidihcrdpar[2,2]/0.8
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
          Amidihcr.more()
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


Amidihinfusion.more<-function()
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
     cat("           (x) button at upper right corner.                    \n\n")
     Amidihinfusioncpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amidihinfusioncpar<-edit(Amidihinfusioncpar)
     Amidihinfusioncpar<-check(Amidihinfusioncpar)
     C<-0.8*Amidihinfusioncpar[1,2]/(samplesStats("cl_F")*Amidihinfusioncpar[2,2])
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
          Amidihinfusion.more()
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
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amidihinfusiondpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amidihinfusiondpar<-edit(Amidihinfusiondpar)
     Amidihinfusiondpar<-check(Amidihinfusiondpar)
     d<-Amidihinfusiondpar[1,2]*samplesStats("cl_F")*Amidihinfusiondpar[2,2]/0.8
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
          Amidihinfusion.more()
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
     cat("           (x) button at upper right corner.                    \n\n")
     Amidihinfusioncpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Amidihinfusioncpar<-edit(Amidihinfusioncpar)
     Amidihinfusioncpar<-check(Amidihinfusioncpar)
     C<-0.8*Amidihinfusioncpar[1,2]/(samplesStats("cl_F")*Amidihinfusioncpar[2,2])
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
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Amidihinfusiondpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Amidihinfusiondpar<-edit(Amidihinfusiondpar)
     Amidihinfusiondpar<-check(Amidihinfusiondpar)
     d<-Amidihinfusiondpar[1,2]*samplesStats("cl_F")*Amidihinfusiondpar[2,2]/0.8
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
          Amidihinfusion.more()
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


Oxtir.more<-function()
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
          Oxtir.more()
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
          Oxtir.more()
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
          Oxtir.more()
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



Oxtcr.more<-function()
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
     Oxtcrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Oxtcrcpar<-edit(Oxtcrcpar)
     Oxtcrcpar<-check(Oxtcrcpar)
     C<-0.65*Oxtcrcpar[1,2]/(samplesStats("cl_F")*Oxtcrcpar[2,2])
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
          Oxtcr.more()
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
     Oxtcrdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Oxtcrdpar<-edit(Oxtcrdpar)
     Oxtcrdpar<-check(Oxtcrdpar)
     d<-Oxtcrdpar[1,2]*samplesStats("cl_F")*Oxtcrdpar[2,2]/0.65
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
          Oxtcr.more()
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
     Oxtcrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Oxtcrcpar<-edit(Oxtcrcpar)
     Oxtcrcpar<-check(Oxtcrcpar)
     C<-0.65*Oxtcrcpar[1,2]/(samplesStats("cl_F")*Oxtcrcpar[2,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
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
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Oxtcrdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Oxtcrdpar<-edit(Oxtcrdpar)
     Oxtcrdpar<-check(Oxtcrdpar)
     d<-Oxtcrdpar[1,2]*samplesStats("cl_F")*Oxtcrdpar[2,2]/0.65
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
          Oxtcr.more()
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



Their.more<-function()
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
          Their.more()
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
          Their.more()
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
          Their.more()
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



Thecr.more<-function()
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
     Thecrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Thecrcpar<-edit(Thecrcpar)
     Thecrcpar<-check(Thecrcpar)
     C<-Thecrcpar[1,2]/(samplesStats("cl_F")*Thecrcpar[2,2])
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
          Thecr.more()
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
     Thecrdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Thecrdpar<-edit(Thecrdpar)
     Thecrdpar<-check(Thecrdpar)
     d<-Thecrdpar[1,2]*samplesStats("cl_F")*Thecrdpar[2,2]
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
          Thecr.more()
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
     Thecrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Thecrcpar<-edit(Thecrcpar)
     Thecrcpar<-check(Thecrcpar)
     C<-Thecrcpar[1,2]/(samplesStats("cl_F")*Thecrcpar[2,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("Css_trough (mg/L)")
     cat("\n")
     show(coutput)
     cat("\n")
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
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     Thecrdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Thecrdpar<-edit(Thecrdpar)
     Thecrdpar<-check(Thecrdpar)
     d<-Thecrdpar[1,2]*samplesStats("cl_F")*Thecrdpar[2,2]
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
          Thecr.more()
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


Theinfusion.more<-function()
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
     cat("           (x) button at upper right corner.                    \n\n")
     Theinfusioncpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Theinfusioncpar<-edit(Theinfusioncpar)
     Theinfusioncpar<-check(Theinfusioncpar)
     C<-Theinfusioncpar[1,2]/(Theinfusioncpar[2,2]*samplesStats("cl_F"))
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
          Theinfusion.more()
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
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Theinfusiondpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Theinfusiondpar<-edit(Theinfusiondpar)
     Theinfusiondpar<-check(Theinfusiondpar)
     d<-Theinfusiondpar[1,2]*(samplesStats("cl_F"))*Theinfusiondpar[2,2]
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
          Theinfusion.more()
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
     cat("  Css_trough = predicted conc (mg/L)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Theinfusioncpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Theinfusioncpar<-edit(Theinfusioncpar)
     Theinfusioncpar<-check(Theinfusioncpar)
     C<-Theinfusioncpar[1,2]/(Theinfusioncpar[2,2]*samplesStats("cl_F"))
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
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Theinfusiondpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Theinfusiondpar<-edit(Theinfusiondpar)
     Theinfusiondpar<-check(Theinfusiondpar)
     d<-Theinfusiondpar[1,2]*(samplesStats("cl_F"))*Theinfusiondpar[2,2]
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
          Theinfusion.more()
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

PedDig.more<-function(A)
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
     cat("  Css_trough = predicted trough conc (mcg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                     \n\n")
     PedDigcpar<-data.frame(input=c("D (mcg)","tau (hr)"),value=c(0))
     PedDigcpar<-edit(PedDigcpar)
     PedDigcpar<-check(PedDigcpar)
     C<-PedDigcpar[1,2]*1000/((samplesStats("cl_F"))*PedDigcpar[2,2]*A)
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
          PedDig.more(A)
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
     cat("           (x) button at upper right corner.                     \n\n")
     PedDigdpar<-data.frame(input=c("Css (mcg/L)","tau (hr)"),value=c(0))
     PedDigdpar<-edit(PedDigdpar)
     PedDigdpar<-check(PedDigdpar)
     d<-PedDigdpar[1,2]*(samplesStats("cl_F"))*PedDigdpar[2,2]*A/1000
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
          PedDig.more(A)
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
     cat("           (x) button at upper right corner.                     \n\n")
     PedDigcpar<-data.frame(input=c("D (mcg)","tau (hr)"),value=c(0))
     PedDigcpar<-check(PedDigcpar)
     PedDigcpar<-check(PedDigcpar)
     C<-PedDigcpar[1,2]*1000/((samplesStats("cl_F"))*PedDigcpar[2,2]*A)
     sim<-matrix(C[1 ,1])
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
     cat("           (x) button at upper right corner.                     \n\n")
     PedDigdpar<-data.frame(input=c("Css (mcg/L)","tau (hr)"),value=c(0))
     PedDigdpar<-edit(PedDigdpar)
     PedDigdpar<-check(PedDigdpar)
     d<-PedDigdpar[1,2]*(samplesStats("cl_F"))*PedDigdpar[2,2]*A/1000
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
          PedDig.more(A)
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
  
#Val.more
Val.more<-function(A)
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
     cat("           (x) button at upper right corner.                    \n\n")
     Valcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Valcpar<-edit(Valcpar)
     Valcpar<-check(Valcpar)
     C<-A*Valcpar[1,2]/(11.5*(A-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valcpar[2,2])))*exp(-(samplesStats("cl_F"))/11.5*Valcpar[2,2])-(1/(1-exp(-A*Valcpar[2,2])))*exp(-A*Valcpar[2,2]))
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
          Val.more(A)
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
     cat("           (x) button at upper right corner.                    \n\n")
     Valdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Valdpar<-edit(Valdpar)
     Valdpar<-check(Valdpar)
     d<-Valdpar[1,2]/(A/(11.5*(A-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valdpar[2,2])))*exp(-(samplesStats("cl_F"))/11.5*Valdpar[2,2])-(1/(1-exp(-A*Valdpar[2,2])))*exp(-A*Valdpar[2,2])))
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
          Val.more(A)
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
     cat("           (x) button at upper right corner.                    \n\n")
     Valcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Valcpar<-edit(Valcpar)
     Valcpar<-check(Valcpar)
     C<-A*Valcpar[1,2]/(11.5*(A-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valcpar[2,2])))*exp(-(samplesStats("cl_F"))/11.5*Valcpar[2,2])-(1/(1-exp(-A*Valcpar[2,2])))*exp(-A*Valcpar[2,2]))
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
     Valdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Valdpar<-edit(Valdpar)
     Valdpar<-check(Valdpar)
     d<-Valdpar[1,2]/(A/(11.5*(A-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valdpar[2,2])))*exp(-(samplesStats("cl_F"))/11.5*Valdpar[2,2])-(1/(1-exp(-A*Valdpar[2,2])))*exp(-A*Valdpar[2,2])))
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
          Val.more(A)
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
  
  
#Valtwo.more
Valtwo.more<-function(B)
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
     cat("           (x) button at upper right corner.                    \n\n")
     Valsmcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Valsmcpar<-edit(Valsmcpar)
     Valsmcpar<-check(Valsmcpar)
     C<-B*Valsmcpar[1,2]/(11.5*(B-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valsmcpar[2,2])))*exp(-(samplesStats("cl_F"))/11.5*Valsmcpar[2,2])-(1/(1-exp(-B*Valsmcpar[2,2])))*exp(-B*Valsmcpar[2,2]))
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
          Valtwo.more(B)
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
     cat("           (x) button aat upper right corner.                   \n\n")
     Valsmdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Valsmdpar<-edit(Valsmdpar)
     Valsmdpar<-check(Valsmdpar)
     d<-Valsmdpar[1,2]/(B/(11.5*(B-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valsmdpar[2,2])))*exp(-(samplesStats("cl_F"))/11.5*Valsmdpar[2,2])-(1/(1-exp(-B*Valsmdpar[2,2])))*exp(-B*Valsmdpar[2,2])))
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
          Valtwo.more(B)
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
     cat("           (x) button at upper right corner.                    \n\n")
     Valsmcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Valsmcpar<-edit(Valsmcpar)
     Valsmcpar<-check(Valsmcpar)
     C<-B*Valsmcpar[1,2]/(11.5*(B-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valsmcpar[2,2])))*exp(-(samplesStats("cl_F"))/11.5*Valsmcpar[2,2])-(1/(1-exp(-B*Valsmcpar[2,2])))*exp(-B*Valsmcpar[2,2]))
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
     cat("           (x) button at upper right corner.                    \n\n")
     Valsmdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     Valsmdpar<-edit(Valsmdpar)
     Valsmdpar<-check(Valsmdpar)
     d<-Valsmdpar[1,2]/(B/(11.5*(B-(samplesStats("cl_F"))/11.5))*((1/(1-exp(-(samplesStats("cl_F"))/11.5*Valsmdpar[2,2])))*exp(-(samplesStats("cl_F"))/11.5*Valsmdpar[2,2])-(1/(1-exp(-B*Valsmdpar[2,2])))*exp(-B*Valsmdpar[2,2])))
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
          Valtwo.more()
       } else {
             if (pick == 2){
             cal.again(B)
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
  
  
#Cyc.more
Cyc.more<-function(A,B)
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
     cat("  Css_trough = predicted trough conc (mcg/L)\n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Cyccpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Cyccpar<-edit(Cyccpar)
     Cyccpar<-check(Cyccpar)
     C<-(0.2+10*abs(A-7)/((A+10)*60))*Cyccpar[1,2]*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cyccpar[2,2])))*exp(-(samplesStats("cl_F"))/(4*B)*Cyccpar[2,2])-(1/(1-exp(-0.3*Cyccpar[2,2])))*exp(-0.3*Cyccpar[2,2]))
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
          Cyc.more(A,B)
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
     Cycdpar<-data.frame(input=c("Css_trough (mcg/L)","tau (hr)"),value=c(0))
     Cycdpar<-edit(Cycdpar)
     Cycdpar<-check(Cycdpar)
     d<-Cycdpar[1,2]/((0.2+10*abs(A-7)/((A+10)*60))*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cycdpar[2,2])))*exp(-(samplesStats("cl_F"))/(4*B)*Cycdpar[2,2])-(1/(1-exp(-0.3*Cycdpar[2,2])))*exp(-0.3*Cycdpar[2,2])))
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
          Cyc.more(A,B)
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
     cat("  Css_trough = predicted trough conc (mcg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Cyccpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Cyccpar<-edit(Cyccpar)
     Cyccpar<-check(Cyccpar)
     C<-(0.2+10*abs(A-7)/((A+10)*60))*Cyccpar[1,2]*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cyccpar[2,2])))*exp(-(samplesStats("cl_F"))/(4*B)*Cyccpar[2,2])-(1/(1-exp(-0.3*Cyccpar[2,2])))*exp(-0.3*Cyccpar[2,2]))
     sim<-matrix(C[1 ,1])
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
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Cycdpar<-data.frame(input=c("Css_trough (mcg/L)","tau (hr)"),value=c(0))
     Cycdpar<-edit(Cycdpar)
     Cycdpar<-check(Cycdpar)
     d<-Cycdpar[1,2]/((0.2+10*abs(A-7)/((A+10)*60))*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cycdpar[2,2])))*exp(-(samplesStats("cl_F"))/(4*B)*Cycdpar[2,2])-(1/(1-exp(-0.3*Cycdpar[2,2])))*exp(-0.3*Cycdpar[2,2])))
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
          Cyc.more(A,B)
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


#Cyctwo.more
Cyctwo.more<-function(A,B)
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
     cat("  Css_trough = predicted trough conc (mcg/L)\n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Cycsmcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Cycsmcpar<-edit(Cycsmcpar)
     Cycsmcpar<-check(Cycsmcpar)
     C<-(0.2+10*abs(A-7)/((A+10)*60))*Cycsmcpar[1,2]*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cycsmcpar[2,2])))*exp(-(samplesStats("cl_F"))/(4*B)*Cycsmcpar[2,2])-(1/(1-exp(-0.3*Cycsmcpar[2,2])))*exp(-0.3*Cycsmcpar[2,2]))
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
          Cyctwo.more(A,B)
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
     Cycsmdpar<-data.frame(input=c("Css_trough (mcg/L)","tau (hr)"),value=c(0))
     Cycsmdpar<-edit(Cycsmdpar)
     Cycsmdpar<-check(Cycsmdpar)
     d<-Cycsmdpar[1,2]/((0.2+10*abs(A-7)/((A+10)*60))*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cycsmdpar[2,2])))*exp(-(samplesStats("cl_F"))/(4*B)*Cycsmdpar[2,2])-(1/(1-exp(-0.3*Cycsmdpar[2,2])))*exp(-0.3*Cycsmdpar[2,2])))
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
          Cyctwo.more(A,B)
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
     cat("  Css_trough = predicted trough conc (mg/L) \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Cycsmcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Cycsmcpar<-edit(Cycsmcpar)
     Cycsmcpar<-check(Cycsmcpar)
     C<-(0.2+10*abs(A-7)/((A+10)*60))*Cycsmcpar[1,2]*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cycsmcpar[2,2])))*exp(-(samplesStats("cl_F"))/(4*B)*Cycsmcpar[2,2])-(1/(1-exp(-0.3*Cycsmcpar[2,2])))*exp(-0.3*Cycsmcpar[2,2]))
     sim<-matrix(C[1 ,1])
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
     cat("  Dose = predicted dose (mg)                \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Cycsmdpar<-data.frame(input=c("Css_trough (mcg/L)","tau (hr)"),value=c(0))
     Cycsmdpar<-edit(Cycsmdpar)
     Cycsmdpar<-check(Cycsmdpar)
     d<-Cycsmdpar[1,2]/((0.2+10*abs(A-7)/((A+10)*60))*1000*0.3/((4*B)*(0.3-(samplesStats("cl_F")/(4*B))))*((1/(1-exp(-(samplesStats("cl_F")/(4*B))*Cycsmdpar[2,2])))*exp(-(samplesStats("cl_F"))/(4*B)*Cycsmdpar[2,2])-(1/(1-exp(-0.3*Cycsmdpar[2,2])))*exp(-0.3*Cycsmdpar[2,2])))
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
          Cyctwo.more(A,B)
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
Ima.more<-function()
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
          Ima.more()
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
          Ima.more()
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
          Ima.more()
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



#ChiVal.more
ChiVal.more<-function(A)
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
     cat("           (x) button at upper right corner.                    \n\n")
     ChiValcpar<-data.frame(input=c("D (mg)","tau (hr)"),Value=c(0))
     ChiValcpar<-edit(ChiValcpar)
     ChiValcpar<-check(ChiValcpar)
     C<-1.9*ChiValcpar[1,2]/((0.24*A)*(1.9-(samplesStats("cl_F"))/(0.24*A)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValcpar[2,2])))*exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValcpar[2,2])-(1/(1-exp(-1.9*ChiValcpar[2,2])))*exp(-1.9*ChiValcpar[2,2]))
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
          ChiVal.more(A)
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
     cat("           (x) button at upper right corner.                    \n\n")
     ChiValdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     ChiValdpar<-edit(ChiValdpar)
     ChiValdpar<-check(ChiValdpar)
     d<-ChiValdpar[1,2]/(1.9/((0.24*A)*(1.9-(samplesStats("cl_F"))/(0.24*A)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValdpar[2,2])))*exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValdpar[2,2])-(1/(1-exp(-1.9*ChiValdpar[2,2])))*exp(-1.9*ChiValdpar[2,2])))
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
          ChiVal.more(A)
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
     cat("           (x) button at upper right corner.                    \n\n")
     ChiValcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     ChiValcpar<-edit(ChiValcpar)
     ChiValcpar<-check(ChiValcpar)
     C<-1.9*ChiValcpar[1,2]/((0.24*A)*(1.9-(samplesStats("cl_F"))/(0.24*A)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValcpar[2,2])))*exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValcpar[2,2])-(1/(1-exp(-1.9*ChiValcpar[2,2])))*exp(-1.9*ChiValcpar[2,2]))
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
     ChiValdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     ChiValdpar<-edit(ChiValdpar)
     ChiValdpar<-check(ChiValdpar)
     d<-ChiValdpar[1,2]/(1.9/((0.24*A)*(1.9-(samplesStats("cl_F"))/(0.24*A)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValdpar[2,2])))*exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValdpar[2,2])-(1/(1-exp(-1.9*ChiValdpar[2,2])))*exp(-1.9*ChiValdpar[2,2])))
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
          ChiVal.more(A)
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
  
  
  
  
#ChiValtwo.more
ChiValtwo.more<-function(A)
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
     cat("           (x) button at upper right corner.                    \n\n")
     ChiValcpar<-data.frame(input=c("D (mg)","tau (hr)"),Value=c(0))
     ChiValcpar<-edit(ChiValcpar)
     ChiValcpar<-check(ChiValcpar)
     C<-1.9*ChiValcpar[1,2]/((0.24*A)*(1.9-(samplesStats("cl_F"))/(0.24*A)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValcpar[2,2])))*exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValcpar[2,2])-(1/(1-exp(-1.9*ChiValcpar[2,2])))*exp(-1.9*ChiValcpar[2,2]))
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
          ChiVal.more(A)
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
     cat("           (x) button at upper right corner.                    \n\n")
     ChiValdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     ChiValdpar<-edit(ChiValdpar)
     ChiValdpar<-check(ChiValdpar)
     d<-ChiValdpar[1,2]/(1.9/((0.24*A)*(1.9-(samplesStats("cl_F"))/(0.24*A)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValdpar[2,2])))*exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValdpar[2,2])-(1/(1-exp(-1.9*ChiValdpar[2,2])))*exp(-1.9*ChiValdpar[2,2])))
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
          ChiVal.more(A)
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
     cat("           (x) button at upper right corner.                    \n\n")
     ChiValcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     ChiValcpar<-edit(ChiValcpar)
     ChiValcpar<-check(ChiValcpar)
     C<-1.9*ChiValcpar[1,2]/((0.24*A)*(1.9-(samplesStats("cl_F"))/(0.24*A)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValcpar[2,2])))*exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValcpar[2,2])-(1/(1-exp(-1.9*ChiValcpar[2,2])))*exp(-1.9*ChiValcpar[2,2]))
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
     ChiValdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(0))
     ChiValdpar<-edit(ChiValdpar)
     ChiValdpar<-check(ChiValdpar)
     d<-ChiValdpar[1,2]/(1.9/((0.24*A)*(1.9-(samplesStats("cl_F"))/(0.24*A)))*((1/(1-exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValdpar[2,2])))*exp(-(samplesStats("cl_F"))/(0.24*A)*ChiValdpar[2,2])-(1/(1-exp(-1.9*ChiValdpar[2,2])))*exp(-1.9*ChiValdpar[2,2])))
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
          ChiVal.more(A)
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
  
# Phenytoin  
Phe.more<-function()
{
 cat("\n")
  file.menu <- c("Dose -> Css_trough",
                 "Css_trough -> Dose",
                 "Dose <-> Css_trough",
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
     cat("           (x) button at upper right corner.                    \n\n")
     Phecpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Phecpar<-edit(Phecpar)
     Phecpar<-check(Phecpar)
     Phelimit(Phecpar[1,2],Phecpar[2,2])
  } else {
    if (pick == 2){
     cat("\n")
     cat("********************************************\n")
     cat("  --input data--                            \n")
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Phedpar<-data.frame(input=c("Css (mg/L)","tau (hr)"),value=c(0))
     Phedpar<-edit(Phedpar)
     Phedpar<-check(Phedpar)
     d<-((samplesStats("Vmax"))*Phedpar[1,2])/((samplesStats("Km"))+Phedpar[1,2])/(24/Phedpar[2,2])
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
          Phe.more()
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
     cat("  Css_trough = desired trough conc (mg/L)   \n")
     cat("  tau = desired dosing interval (hr)        \n")
     cat("                                            \n")
     cat("  --output data--                           \n")
     cat("  Dose = desired/predicted dose (mg)        \n")
     cat("********************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Phedpar<-data.frame(input=c("Css (mg/L)","tau (hr)"),value=c(0))
     Phedpar<-edit(Phedpar)
     Phedpar<-check(Phedpar)
     d<-((samplesStats("Vmax"))*Phedpar[1,2])/((samplesStats("Km"))+Phedpar[1,2])/(24/Phedpar[2,2])
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
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
     cat("           (x) button at upper right corner.                    \n\n")
     Phecpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(0))
     Phecpar<-edit(Phecpar)
     Phecpar<-check(Phecpar)
     Phelimit(Phecpar[1,2],Phecpar[2,2]) 
      } else {
      if (pick == 4){
        cal.again()
        }   
      }
    }  
  }
}


# Warfarin  
War.more<-function()
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
     cat("           (x) button at upper right corner.                    \n\n")
     Warcpar<-data.frame(input=c("D (mg)","tau (day)"),value=c(0))
     Warcpar<-edit(Warcpar)
     Warcpar<-check(Warcpar)
     C<-Warcpr(Warcpar[1,2],Warcpar[2,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("INR")
     cat("\n")
     show(coutput)
     cat("\n")  
     file.menu <- c("Yes", 
                      "No")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
       if (pick == 1){
          War.more()
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
     cat("           (x) button at upper right corner.                    \n\n")
     Wardpar<-data.frame(input=c("INR","tau (day)"),value=c(0))
     Wardpar<-edit(Wardpar)
     Wardpar<-check(Wardpar)
     d<-Wardpr(Wardpar[1,2],Wardpar[2,2])
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
          War.more()
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
     cat("  tau = desired dosing interval (day)             \n")
     cat("                                                  \n")
     cat("  --output data--                                 \n")
     cat("  INR = predicted international normalized ratio  \n")
     cat("**************************************************\n\n")
     cat("\n")
     cat("     Please enter all parameters values at Data Editor          \n")
     cat("     window, and close Data Editor window by clicking           \n")
     cat("           (x) button at upper right corner.                    \n\n")
     Warcpar<-data.frame(input=c("D (mg)","tau (day)"),value=c(0))
     Warcpar<-edit(Warcpar)
     Warcpar<-check(Warcpar)
     C<-Warcpr(Warcpar[1,2],Warcpar[2,2])
     sim<-matrix(C[1 ,1])
     coutput<-data.frame(sim)
     colnames(coutput)<-list("INR")
     cat("\n")
     show(coutput)
     cat("\n\n")
     cat("          Pressing Enter to continue..                  \n")
     readline()
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
     cat("           (x) button at upper right corner.                    \n\n")
     Wardpar<-data.frame(input=c("INR","tau (day)"),value=c(0))
     Wardpar<-edit(Wardpar)
     Wardpar<-check(Wardpar)
     d<-Wardpr(Wardpar[1,2],Wardpar[2,2])
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
          War.more()
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