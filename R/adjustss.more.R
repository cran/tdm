# Aminoglycoside
Ami.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
v  <- X[2,2]

  cat("\n")                                     
  file.menu <- c("Css_trough -> Dose",                       
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){                                                                              
     note_for_infusion_c_to_d();cat("\n")
     cat(" --- Suggested target aminolgycosides therapeutic ranges ---\n")
     cat(" Aminoglycosides       Target           Levels            \n")
     cat("                     plasma conc.       (mcg/L)           \n")
     cat(" ---------------------------------------------------------\n")
     cat("   amikacin             peak             20-25            \n")
     cat("                        trough           <  8           \n\n")
     cat("   gentamicin/          peak             8-10             \n")
     cat("    tobramycin/         trough           <  2             \n")
     cat("    netilmicn                                             \n")
     cat(" ---------------------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     Amidpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)","tin (hr)"),value=c(0,0,0.5))
     Amidpar<-edit(Amidpar)
     ### show(Amidpar);cat("\n\n")
##     Amidpar<-check(Amidpar)
     d<-Amidpar[1,2]/(((1-exp(-(cl)/(v)*Amidpar[3,2]))/(Amidpar[3,2]*(cl)*(1-exp(-(cl)/(v)*Amidpar[2,2]))))*
        exp(-(cl)/(v)*(Amidpar[2,2]-Amidpar[3,2])))
     Cpeak<-(d*(1-exp(-(cl)/(v)*Amidpar[3,2]))/(Amidpar[3,2]*(cl)*(1-exp(-(cl)/(v)*Amidpar[2,2]))))
     doutput<-data.frame(Parameters=c("Css_trough (mg/L)","Css_peak (mg/mL)","tau (hr)","tin (hr)"," -> Dose (mg)"),
                             Values=c(Amidpar[1,2],Cpeak,Amidpar[2,2],Amidpar[3,2],d))
     cat("\n")
     show(doutput)
     cat("\n")                          
     file.menu <- c("Dose -> Css_trough",
                    "exit")
       pick <- menu(file.menu, title = "<< D -> C Adjustment? >>")
       if (pick == 1){                                                   
          cat("\n")
          note_for_infusion_d_to_c();cat("\n")
          cat(" --- Suggested Target Aminolgycosides Therapeutic Ranges ---\n")
          cat(" Aminoglycosides       Target           Levels            \n")
          cat("                     plasma conc.       (mcg/L)           \n")
          cat(" ---------------------------------------------------------\n")
          cat("   amikacin             peak             20-25            \n")
          cat("                        trough           <  8           \n\n")
          cat("   gentamicin/          peak             8-10             \n")
          cat("    tobramycin/         trough           <  2             \n")
          cat("    netilmicn                                             \n")
          cat(" ---------------------------------------------------------\n")
          cat("\n")
          note_for_close_window()
          Amicpar<-data.frame(input=c("D (mg)","tau (hr)","tin (hr)"),value=c(d,Amidpar[2,2],Amidpar[3,2]))
          Amicpar<-edit(Amicpar)
          ### show(Amicpar);cat("\n\n")                                                                     
##          Amicpar<-check(Amicpar)
          Cpeak<- (Amicpar[1,2]*(1-exp(-(cl)/(v)*Amicpar[3,2]))/(Amicpar[3,2]*(cl)*(1-exp(-(cl)/(v)*Amicpar[2,2]))))
          C<-(Amicpar[1,2]*(1-exp(-(cl)/(v)*Amicpar[3,2]))/(Amicpar[3,2]*(cl)*(1-exp(-(cl)/(v)*Amicpar[2,2]))))*
              exp(-(cl)/(v)*(Amicpar[2,2]-Amicpar[3,2]))                                                                  
          ### sim<-matrix(C[1 ,1])                                                                       
          coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)","Inf.time (hr)"," -> Css_peak (mg/mL)"," -> Css_trough (mg/L)"),
                                  Values=c(Amicpar[1,2],Amicpar[2,2],Amicpar[3,2],Cpeak,C))
          cat("\n")
          show(coutput)
          cat("\n")
          Ami.more()
       } else {
             if (pick == 2){                                           
             	cal.again()
         } 
  } }else {
      if (pick == 2){               
           cal.again()         
      }
    }  
  }


Van.more<-function()
{
### read from saved .cvs -YJ
X  <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
v  <- X[2,2]
###
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_infusion_c_to_d();cat("\n")
     cat(" --- Suggested target Vanco therapeutic ranges ---\n")
     cat(" ---------------------------------------\n")
     cat("         peak            30-45          \n")
     cat("         trough           > 10          \n")
     cat(" ---------------------------------------\n")
     cat("\n")
     note_for_close_window()
     Vandpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)","tin (hr)"),value=c(10,12,1.0))
     Vandpar<-edit(Vandpar)
     ### show(Vandpar);cat("\n\n")
##     Vandpar<-check(Vandpar)
     Cpeak<- (Vandpar[1,2]*(1-exp(-(cl)/(v)*Vandpar[3,2]))/(Vandpar[3,2]*(cl)*(1-exp(-(cl)/(v)*Vandpar[2,2]))))
     d<-Vandpar[1,2]/(((1-exp(-(cl)/(v)*Vandpar[3,2]))/(Vandpar[3,2]*(cl)*(1-exp(-(cl)/(v)*Vandpar[2,2]))))*
        exp(-(cl)/(v)*(Vandpar[2,2]-Vandpar[3,2])))
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mg/L)","Css_peak (mg/mL)","tau (hr)","tin (hr)"," -> Dose (mg)"),
                             Values=c(Vandpar[1,2],Cpeak,Vandpar[2,2],Vandpar[3,2],d))
     cat("\n")
     show(doutput)
     cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
   if(pick ==1){
     cat("\n")
     note_for_infusion_d_to_c();cat("\n")
     cat(" --- Suggested target Vanco therapeutic ranges ---\n")
     cat(" ---------------------------------------\n")
     cat("         peak            30-45          \n")
     cat("         trough           > 10          \n")
     cat(" ---------------------------------------\n")
     cat("\n")
     cat("\n")
     note_for_close_window()
     Vancpar<-data.frame(input=c("D (mg)","tau (hr)","tin (hr)"),value=c(d,Vandpar[2,2],Vandpar[3,2]))
     Vancpar<-edit(Vancpar)
     ### show(Vancpar);cat("\n\n")
##     Vancpar<-check(Vancpar)
     Cpeak <- (Vancpar[1,2]*(1-exp(-(cl)/(v)*Vancpar[3,2]))/(Vancpar[3,2]*(cl)*(1-exp(-(cl)/(v)*Vancpar[2,2]))))
     C<-(Vancpar[1,2]*(1-exp(-(cl)/(v)*Vancpar[3,2]))/(Vancpar[3,2]*(cl)*(1-exp(-(cl)/(v)*Vancpar[2,2]))))*
        exp(-(cl)/(v)*(Vancpar[2,2]-Vancpar[3,2]))
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)","Inf.time (hr)"," -> Css_peak (mg/mL)"," -> Css_trough (mg/L)"),
                             Values=c(Vancpar[1,2],Vancpar[2,2],Vancpar[3,2],Cpeak,C))
     cat("\n")
     show(coutput)
       cat("\n")
       Van.more()
       } else {
             if (pick == 2){
             cal.again()
             	   }
    } } else {
             if (pick == 2){
             	  cal.again()   
            }  
         }
       }

  
Car.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d();cat("\n")
     cat("\n *** targeted CBZ plasma conc. range: 4-12 mg/L.\n\n")
     note_for_close_window()
     CBZdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(8,12))
     CBZdpar<-edit(CBZdpar)
     ### show(CBZdpar);cat("\n\n")
     ### CBZdpar<-check(CBZdpar)
     d<-CBZdpar[1,2]/(1.2/((v_F)*(1.2-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*CBZdpar[2,2])))*
        exp(-(cl_F)/(v_F)*CBZdpar[2,2])-(1/(1-exp(-1.2*CBZdpar[2,2])))*exp(-1.2*CBZdpar[2,2])))
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(CBZdpar[1,2],CBZdpar[2,2],d))
     cat("\n")
     show(doutput)     
       cat("\n")
       file.menu <- c("Dose -> Css_trough",
                      "exit")
       pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
       if (pick == 1){
       	 cat("\n")
     note_for_d_to_c()
     cat("\n")
     note_for_close_window();cat("\n")
     cat("\n *** targeted CBZ plasma conc. range: 4-12 mg/L.\n\n")
     CBZcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,CBZdpar[2,2]))
     CBZcpar<-edit(CBZcpar)
     ### show(CBZcpar);cat("\n\n")
##     CBZcpar<-check(CBZcpar)
     C<-1.2*CBZcpar[1,2]/((v_F)*(1.2-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*CBZcpar[2,2])))*exp(-(cl_F)/(v_F)*
        CBZcpar[2,2])-(1/(1-exp(-1.2*CBZcpar[2,2])))*exp(-1.2*CBZcpar[2,2]))
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css_trough (mg/L)"),
                             Values=c(CBZcpar[1,2],CBZcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Car.more()
       } else {
             if (pick == 2){
               	cal.again()
        }
  }} else {
        if (pick == 2){
         cal.again()   
    }  
  }
}


Dig.more<-function()
{

X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
  
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d_ng();cat("\n")
     cat("\n *** Suggested targeted digoxin serum conc. range ***\n\n")
     cat("   CHF: 0.5-0.8 ng/mL; atrial fib.: 0.8-2 ng/mL\n\n")
     note_for_close_window()
     Digdpar<-data.frame(input=c("Css_trough (ng/mL)","tau (hr)"),value=c(1.2,24))
     Digdpar<-edit(Digdpar)     ### directly calc and show targeted digoxin plasma conc (1.5 ng/mL) 
##     Digdpar<-check(Digdpar)
     ### show(Digdpar):cat("\n\n")
     d<-Digdpar[1,2]*cl_F*Digdpar[2,2]/1000
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (ng/mL)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Digdpar[1,2],Digdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
     file.menu <- c("Dose -> Css_trough",
                    "exit")
     pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
     if (pick == 1){
     cat("\n")
     note_for_d_to_c_ng();cat("\n")
     cat("\n *** Suggested targeted digoxin serum conc. range ***\n\n")
     cat("   CHF: 0.5-0.8 ng/mL; atrial fib.: 0.8-2 ng/mL\n\n")
     note_for_close_window()
     Digcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,24))
     Digcpar<-edit(Digcpar)
     ### show(Digcpar):cat("\n\n")
##     Digcpar<-check(Digcpar)
     C<-Digcpar[1,2]*1000/(cl_F*Digcpar[2,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css_trough (ng/mL)"),
                             Values=c(Digcpar[1,2],Digcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
         Dig.more()
      } else {
      	 if (pick == 2){
          cal.again()
       }
  }} else {
        if (pick == 2){
         cal.again()   
    }  
  }
}


Lit.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
    cat("\n")
     note_for_c_to_d_meq()
     cat("\n")
     note_for_close_window()
     Litdpar<-data.frame(input=c("Css (mEq/L)","tau (hr)"),value=c(0.8,12))
     Litdpar<-edit(Litdpar)
     ### show(Litdpar);cat("\n\n")
##     Litdpar<-check(Litdpar)
     d<-Litdpar[1,2]*(cl_F)*Litdpar[2,2]*36.9458
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css (mEq/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Litdpar[1,2],Litdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
     file.menu <- c("Dose -> Css_trough",
                    "exit")
     pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
     if (pick == 1){
     cat("\n")
     note_for_d_to_c_meq()
     cat("\n")
     note_for_close_window()
     Litcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Litdpar[2,2]))
     Litcpar<-edit(Litcpar)
     ### show(Litcpar);cat("\n\n")
     C<-(Litcpar[1,2]/36.9458)/((cl_F)*Litcpar[2,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css (mEq/L)"),
                             Values=c(Litcpar[1,2],Litcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Lit.more()
       } else {
       	 if (pick == 2){
            cal.again()
        }
  }} else {
        if (pick == 2){
           cal.again()   
    }  
  }
}


Litcit.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
    cat("\n")
     note_for_c_to_d_meq()
     cat("\n")
     note_for_close_window()
     Litcitdpar<-data.frame(input=c("Css (mEq/L)","tau (hr)"),value=c(0.8,12))
     Litcitdpar<-edit(Litcitdpar)
     ### show(Litcitdpar);cat("\n\n")
##     Litcitdpar<-check(Litcitdpar)
     d<-Litcitdpar[1,2]*(cl_F)*Litcitdpar[2,2]*94
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css (mEq/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Litcitdpar[1,2],Litcitdpar[2,2],d))
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
     cat("\n")
     file.menu <- c("Dose -> Css_trough",
                    "exit")
     pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
     if (pick == 1){
     cat("\n")
     note_for_d_to_c_meq()
     cat("\n")
     note_for_close_window()
     Litcitcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Litcitdpar[2,2]))
     Litcitcpar<-edit(Litcitcpar)
     ### show(Litcitcpar);cat("\n\n")
##     Litcitcpar<-check(Litcitcpar)
     C<-(Litcitcpar[1,2]/94)/((cl_F)*Litcitcpar[2,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css (mEq/L)"),
                             Values=c(Litcitcpar[1,2],Litcitcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Litcit.more()
       } else {
       	 if (pick == 2){
             cal.again()
        }
  }} else {
        if (pick == 2){
            cal.again()   
    }  
  }
}


Enf.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
ka   <- X[2,2]
v_F  <- X[3,2]
  cat("\n")
  file.menu <- c("Css_trough (mg/L) -> Dose (mg)",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d()
     cat("\n")
     note_for_close_window()
     Enfdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(3.3,12))
     Enfdpar<-edit(Enfdpar)
     ### show(Enfdpar);cat("\n\n")
##     Enfdpar<-check(Enfdpar)
     d<-Enfdpar[1,2]/((ka)/((v_F)*((ka)-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Enfdpar[2,2])))*exp(-(cl_F)/(v_F)*Enfdpar[2,2])-(1/(1-exp(-(ka)*Enfdpar[2,2])))*exp(-(ka)*Enfdpar[2,2])))
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Enfdpar[1,2],Enfdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose (mg) -> Css_trough (mg/L)",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c()
     cat("\n")
     note_for_close_window()
     Enfcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Enfdpar[2,2]))
     Enfcpar<-edit(Enfcpar)
     ### show(Enfcpar);cat("\n\n")
##     Enfcpar<-check(Enfcpar)
     C<-(ka)*Enfcpar[1,2]/((v_F)*((ka)-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Enfcpar[2,2])))*exp(-(cl_F)/(v_F)*Enfcpar[2,2])-(1/(1-exp(-(ka)*Enfcpar[2,2])))*exp(-(ka)*Enfcpar[2,2]))
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css_trough (mg/L)"),
                             Values=c(Enfcpar[1,2],Enfcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Enf.more()
       } else {
       	 if (pick == 2){	
       	  cal.again()
        }
  }} else {
        if (pick == 2){
           cal.again()   
    }  
  }
}


Ind.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
ka   <- X[2,2] 
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d()
     cat("\n")
     note_for_close_window()
     Inddpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(5.0,8))
     Inddpar<-edit(Inddpar)
     ### show(Inddpar);cat("\n\n")
##     Inddpar<-check(Inddpar)
     d<-Inddpar[1,2]/((ka)/(65.7*((ka)-(cl_F)/65.7))*((1/(1-exp(-(cl_F)/65.7*Inddpar[2,2])))*exp(-(cl_F)/65.7*Inddpar[2,2])-(1/(1-exp(-(ka)*Inddpar[2,2])))*exp(-(ka)*Inddpar[2,2])))
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Inddpar[1,2],Inddpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c()
     cat("\n")
     note_for_close_window()
     Indcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Inddpar[2,2]))
     Indcpar<-edit(Indcpar)
     ### show(Indcpar);cat("\n\n")
##     Indcpar<-check(Indcpar)
     C<-(ka)*Indcpar[1,2]/(65.7*((ka)-(cl_F)/65.7))*((1/(1-exp(-(cl_F)/65.7*Indcpar[2,2])))*exp(-(cl_F)/65.7*Indcpar[2,2])-(1/(1-exp(-(ka)*Indcpar[2,2])))*exp(-(ka)*Indcpar[2,2]))
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css_trough (mg/L)"),
                             Values=c(Indcpar[1,2],Indcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Ind.more()
       } else {	
       	 if (pick == 2){
       	  cal.again()
        }
  }} else {
        if (pick == 2){
          cal.again()   
    }  
  }
}


Rit.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
ka   <- X[2,2]
v_F  <- X[3,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d()
     cat("\n")
     note_for_close_window()
     Ritdpar<-data.frame(input=c("Css_trough (mcg/L)","tau (hr)"),value=c(100,24))
     Ritdpar<-edit(Ritdpar)
     ### show(Ritdpar);cat("\n\n")
##     Ritdpar<-check(Ritdpar)
     d<-Ritdpar[1,2]/((ka)/((v_F)*((ka)-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Ritdpar[2,2])))*exp(-(cl_F)/(v_F)*(Ritdpar[2,2]-0.778))-(1/(1-exp(-(ka)*Ritdpar[2,2])))*exp(-(ka)*(Ritdpar[2,2]-0.778))))
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mcg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Ritdpar[1,2],Ritdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c()
     cat("\n")
     note_for_close_window()
     Ritcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Ritdpar[2,2]))
     Ritcpar<-edit(Ritcpar)
     ### show(Ritcpar);cat("\n\n")
##     Ritcpar<-check(Ritcpar)
     C<-(ka)*Ritcpar[1,2]/((v_F)*((ka)-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Ritcpar[2,2])))*exp(-(cl_F)/(v_F)*(Ritcpar[2,2]-0.778))-(1/(1-exp(-(ka)*Ritcpar[2,2])))*exp(-(ka)*(Ritcpar[2,2]-0.778)))
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css_trough (mg/L)"),
                             Values=c(Ritcpar[1,2],Ritcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Rit.more()
       } else {
       	 if (pick == 2){
       	 cal.again()
        }
  }} else {
        if (pick == 2){
         cal.again()   
    }  
  }
}


Eve.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]

  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d_mcg();cat("\n")
     cat(" Suggested everolimus target plasma conc.\n\n")
     cat(" Renal/heart transplant: 3-8 mcg/L         \n")
     cat("                   SEGA: 5-10 mcg/L        \n\n")
     note_for_close_window()
     Evedpar<-data.frame(input=c("Css_trough (mcg/L)","tau (hr)"),value=c(6,24))
     Evedpar<-edit(Evedpar)
     ### show(Evedpar);cat("\n\n")
##     Evedpar<-check(Evedpar)
     d<-Evedpar[1,2]/(6.07/((v_F)*(6.07-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Evedpar[2,2])))*exp(-(cl_F)/(v_F)*Evedpar[2,2])-(1/(1-exp(-6.07*Evedpar[2,2])))*exp(-6.07*Evedpar[2,2]))*1000)
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Evedpar[1,2],Evedpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c_mcg();cat("\n")
     cat(" Suggested everolimus target plasma conc.\n\n")
     cat(" Renal/heart transplant: 3-8 mcg/L         \n")
     cat("                   SEGA: 5-10 mcg/L        \n\n")
     note_for_close_window()
     Evecpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Evedpar[2,2]))
     Evecpar<-edit(Evecpar)
     ### show(Evecpar);cat("\n\n")
##     Evecpar<-check(Evecpar)
     C<-6.07*Evecpar[1,2]/((v_F)*(6.07-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Evecpar[2,2])))*exp(-(cl_F)/(v_F)*Evecpar[2,2])-(1/(1-exp(-6.07*Evecpar[2,2])))*exp(-6.07*Evecpar[2,2]))*1000
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css_trough (mcg/L)"),
                             Values=c(Evecpar[1,2],Evecpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Eve.more()
       } else {	
       	 if (pick == 2){
       	  cal.again()
        }
  }} else {
        if (pick == 2){
         cal.again()   
    }  
  }
}


Tac.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d_mcg();cat("\n")
     cat(" --- Suggested tacrolimus target blood conc. ---\n\n")
     cat("          plasma: 0.5-1.5 mcg/mL                \n")
     cat("     whole blood: 5-15 mcg/mL                   \n")
     cat("\n")
     note_for_close_window()
     Tacdpar<-data.frame(input=c("Css_trough (mcg/mL)","tau (hr)"),value=c(10,12))
     Tacdpar<-edit(Tacdpar)
     ### show(Tacdpar);cat("\n\n")
##     Tacdpar<-check(Tacdpar)
     d<-Tacdpar[1,2]/(4.5/(314*(4.5-(cl_F)/314))*((1/(1-exp(-(cl_F)/314*Tacdpar[2,2])))*exp(-(cl_F)/314*Tacdpar[2,2])-(1/(1-exp(-4.5*Tacdpar[2,2])))*exp(-4.5*Tacdpar[2,2]))*1000)
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mcg/mL)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Tacdpar[1,2],Tacdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c_mcg();cat("\n")
     cat(" --- Suggested tacrolimus target blood conc. ---\n\n")
     cat("          plasma: 0.5-1.5 mcg/mL                \n")
     cat("     whole blood: 5-15 mcg/mL                   \n")
     cat("\n")
     note_for_close_window()
     Taccpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Tacdpar[2,2]))
     Taccpar<-edit(Taccpar)
     ### show(Taccpar);cat("\n\n")
##     Taccpar<-check(Taccpar)
     C<-4.5*Taccpar[1,2]/(314*(4.5-(cl_F)/314))*((1/(1-exp(-(cl_F)/314*Taccpar[2,2])))*exp(-(cl_F)/314*Taccpar[2,2])-(1/(1-exp(-4.5*Taccpar[2,2])))*exp(-4.5*Taccpar[2,2]))*1000
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css_trough (mg/L)"),
                             Values=c(Taccpar[1,2],Taccpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Tac.more()
       } else {
       	 if (pick == 2){
            cal.again()
        }
  }} else {
        if (pick == 2){
           cal.again()   
    }  
  }
}


Eno.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d_eno();cat("\n")
     cat(" -- Suggested therapeutic ANTI-Xa Target ---\n\n")
     cat("      Q12 hour dosing regimen: 0.6-1 IU/mL    \n")
     cat("    Once daily dosing regimen: 1-2   IU/mL    \n")
     cat("\n")
     note_for_close_window()
     Enodpar<-data.frame(input=c("Amax (IU/mL)","tau (hr)"),value=c(0.8,12))
     Enodpar<-edit(Enodpar)
     ### cat("\n");show(Enodpar);cat("\n\n")
##     Enodpar<-check(Enodpar)
     d<-Enodpar[1,2]/(exp(-(cl_F)/(v_F)*(log(0.34*(1-exp(-(cl_F)/(v_F)*Enodpar[2,2]))/((cl_F)/(v_F)*(1-exp(-0.34*Enodpar[2,2])))))/(0.34-(cl_F)/(v_F)))/(v_F)*(1-exp(-(cl_F)/(v_F)*Enodpar[2,2]))/1000)
     ### sim<-matrix(d[1 ,1])
     ### here d is 'IU'; now convert d(IU) to d(mg); 1 mg = 100 IU
     d<-d/100
     doutput<-data.frame(Parameters=c("Amax (IU/mL)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Enodpar[1,2],Enodpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> Amax Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c_eno();cat("\n")
     cat(" -- Suggested therapeutic ANTI-Xa Target ---\n\n")
     cat("      Q12 hour dosing regimen: 0.6-1 IU/mL    \n")
     cat("    Once daily dosing regimen: 1-2   IU/mL    \n")
     cat("\n")
     note_for_close_window()
     Enocpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Enodpar[2,2]))
     Enocpar<-edit(Enocpar)
     ### cat("\n");show(Enocpar);cat("\n\n")
     ### Enocpar<-check(Enocpar)
     ### again; convert input D(mg) into D(IU) to calc. 'C' now. -YJ
     Enocpar[1,2] <- Enocpar[1,2]*100
     C<-Enocpar[1,2]*exp(-(cl_F)/(v_F)*(log(0.34*(1-exp(-(cl_F)/(v_F)*Enocpar[2,2]))/((cl_F)/(v_F)*(1-exp(-0.34*Enocpar[2,2])))))/(0.34-(cl_F)/(v_F)))/(v_F)*(1-exp(-(cl_F)/(v_F)*Enocpar[2,2]))/1000
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> calc. Amax (IU/mL)"),
                             Values=c(Enocpar[1,2],Enocpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Eno.more()
       } else {
             if (pick == 2){
             	cal.again()
        }
  }} else {
        if (pick == 2){
         cal.again()   
    }  
  }
}


Amianhir.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amianhirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(10,8))
     Amianhirdpar<-edit(Amianhirdpar)
     ### cat("\n");show(Amianhirdpar);cat("\n\n")
##     Amianhirdpar<-check(Amianhirdpar)
     d<-Amianhirdpar[1,2]/(0.85*1.85/((v_F)*(1.85-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Amianhirdpar[2,2])))*
        exp(-(cl_F)/(v_F)*Amianhirdpar[2,2])-(1/(1-exp(-1.85*Amianhirdpar[2,2])))*exp(-1.85*Amianhirdpar[2,2])))
     Tmax<-log(1.85-(cl_F/v_F))/(1.85-(cl_F/v_F))    # estimate Tmax first, and
     Cmax<-TheIRsscpr(0.85,d,Amianhirdpar[2,2],Tmax) # then calc. estimated Cpeak
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("**calc Css_Peak (mg/L)","Target Css_trough (mg/L)","tau (hr)"," -> Dose (mg)"),
                              value=c(Cmax,Amianhirdpar[1,2],Amianhirdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amianhircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Amianhirdpar[2,2]))
     Amianhircpar<-edit(Amianhircpar)
     ### cat("\n");show(Amianhircpar);cat("\n\n")
##     Amianhircpar<-check(Amianhircpar)
     C<-0.85*1.85*Amianhircpar[1,2]/((v_F)*(1.85-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Amianhircpar[2,2])))*
        exp(-(cl_F)/(v_F)*Amianhircpar[2,2])-(1/(1-exp(-1.85*Amianhircpar[2,2])))*exp(-1.85*Amianhircpar[2,2]))
     Tmax<-log(1.85-(cl_F/v_F))/(1.85-(cl_F/v_F))    # estimate Tmax first, and
     Cmax<-TheIRsscpr(0.85,Amianhircpar[1,2],Amianhircpar[2,2],Tmax) # then calc. estimated Cpeak
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("D (mg)","tau (hr)","calc Css_trough (mg/L)","calc Css_Peak (mg/L)"),
                              value=c(Amianhircpar[1,2],Amianhircpar[2,2],C,Cmax))
     cat("\n");show(coutput);cat("\n")
          Amianhir.more()
       } else {
             if (pick == 2){
             	cal.again()
        }
  }} else {
        if (pick == 2){
           cal.again()   
    }  
  }
}


Amianhcr.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
  cat("\n")
  file.menu <- c("Css -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amianhcrdpar<-data.frame(input=c("Css (mg/L)","tau (hr)"),value=c(10,12))
     Amianhcrdpar<-edit(Amianhcrdpar)
     ### cat("\n");show(Amianhcrdpar);cat("\n\n")
##     Amianhcrdpar<-check(Amianhcrdpar)
     d<-Amianhcrdpar[1,2]*cl_F*Amianhcrdpar[2,2]/0.85
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Amianhcrdpar[1,2],Amianhcrdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amianhcrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Amianhcrdpar[2,2]))
     Amianhcrcpar<-edit(Amianhcrcpar)
     ### cat("\n");show(Amianhcrcpar);cat("\n\n")
##     Amianhcrcpar<-check(Amianhcrcpar)
     C<-0.85*Amianhcrcpar[1,2]/(cl_F*Amianhcrcpar[2,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css (mg/L)"),
                             Values=c(Amianhcrcpar[1,2],Amianhcrcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Amianhcr.more()
       } else {
             if (pick == 2){
             	cal.again()
        }
  }} else {
        if (pick == 2){
         cal.again()   
    }  
  }
}


Amianhinfusion.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
  cat("\n")
  file.menu <- c("Css -> Dosing Rate",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
    cat("\n")
     note_for_infusionR_c_to_d()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amianhinfusiondpar<-data.frame(input=c("Css (mg/L)"),value=c(10))
     Amianhinfusiondpar<-edit(Amianhinfusiondpar)
     ### cat("\n");show(Amianhinfusiondpar);cat("\n\n")
##     Amianhinfusiondpar<-check(Amianhinfusiondpar)
     d<-Amianhinfusiondpar[1,2]*cl/0.85
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css (mg/L)"," -> Infusion/dosing rate (mg/hr)"),
                             Values=c(Amianhinfusiondpar[1,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dosing Rate -> Css",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_infusionR_d_to_c()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amianhinfusioncpar<-data.frame(input=c("Dosing Rate (mg/hr)"),value=c(d))
     Amianhinfusioncpar<-edit(Amianhinfusioncpar)
     ### cat("\n");show(Amianhinfusioncpar);cat("\n\n")
##     Amianhinfusioncpar<-check(Amianhinfusioncpar)
     C<-0.85*Amianhinfusioncpar[1,2]/(cl)
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Infusion/dosing Rate (mg/hr)"," -> Css (mg/L)"),
                             Values=c(Amianhinfusioncpar[1,2],C))
     cat("\n");show(coutput);cat("\n")
          Amianhinfusion.more()
       } else {
             if (pick == 2){	
             	cal.again()        
        }
  }} else {
        if (pick == 2){
           cal.again()   
    }  
  }
}



Amidihir.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amidihirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(10,8))
     Amidihirdpar<-edit(Amidihirdpar)
     ### cat("\n");show(Amidihirdpar);cat("\n\n")
##     Amidihirdpar<-check(Amidihirdpar)
     d<-Amidihirdpar[1,2]/(0.8*1.85/((v_F)*(1.85-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Amidihirdpar[2,2])))*
        exp(-(cl_F)/(v_F)*Amidihirdpar[2,2])-(1/(1-exp(-1.85*Amidihirdpar[2,2])))*exp(-1.85*Amidihirdpar[2,2])))
     Tmax<-log(1.85-(cl_F/v_F))/(1.85-(cl_F/v_F))    # estimate Tmax first, and
     Cmax<-TheIRsscpr(0.8,d,Amidihirdpar[2,2],Tmax) # then calc. estimated Cpeak
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Target Css_trough (mg/L)","**calc Css_peak (mg/L)","tau (hr)"," -> Dose (mg)"),
                              value=c(Amidihirdpar[1,2],Cmax,Amidihirdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amidihircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Amidihirdpar[2,2]))
     Amidihircpar<-edit(Amidihircpar)
     ### cat("\n");show(Amidihircpar);cat("\n\n")
##     Amidihircpar<-check(Amidihircpar)
     C<-1.85*0.8*Amidihircpar[1,2]/((v_F)*(1.85-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Amidihircpar[2,2])))*
        exp(-(cl_F)/(v_F)*Amidihircpar[2,2])-(1/(1-exp(-1.85*Amidihircpar[2,2])))*exp(-1.85*Amidihircpar[2,2]))
     Tmax<-log(1.85-(cl_F/v_F))/(1.85-(cl_F/v_F))    # estimate Tmax first, and
     Cmax<-TheIRsscpr(0.8,Amidihircpar[1,2],Amidihircpar[2,2],Tmax) # then calc. estimated Cpeak
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("D (mg)","tau (hr)","calc Css_peak (mg/L)"," -> Css_trough (mg/L)"),
                              value=c(Amidihircpar[1,2],Amidihircpar[2,2],Cmax,C))
     cat("\n");show(coutput);cat("\n")
          Amidihir.more()
       } else {
             if (pick == 2){	
             	cal.again()
        }
  }} else {
        if (pick == 2){
            cal.again()   
    }  
  }
}


Amidihcr.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
  cat("\n")
  file.menu <- c("Css -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amidihcrdpar<-data.frame(input=c("Css (mg/L)","tau (hr)"),value=c(10,12))
     Amidihcrdpar<-edit(Amidihcrdpar)
     ### cat("\n");show(Amidihcrdpar);cat("\n\n")
##     Amidihcrdpar<-check(Amidihcrdpar)
     d<-Amidihcrdpar[1,2]*cl_F*Amidihcrdpar[2,2]/0.8
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Amidihcrdpar[1,2],Amidihcrdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amidihcrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Amidihcrdpar[2,2]))
     Amidihcrcpar<-edit(Amidihcrcpar)
     ### cat("\n");show(Amidihcrcpar);cat("\n\n")
##     Amidihcrcpar<-check(Amidihcrcpar)
     C<-0.8*Amidihcrcpar[1,2]/(cl_F*Amidihcrcpar[2,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css (mg/L)"),
                             Values=c(Amidihcrcpar[1,2],Amidihcrcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Amidihcr.more()
       } else {
             if (pick == 2){
             	cal.again()
        }
  }} else {
        if (pick == 2){
          cal.again()   
    }  
  }
}


Amidihinfusion.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
  cat("\n")
  file.menu <- c("Css -> Infusion/dosing Rate",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_infusionR_c_to_d()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amidihinfusiondpar<-data.frame(input=c("Css (mg/L)"),value=c(15))
     Amidihinfusiondpar<-edit(Amidihinfusiondpar)
     ### cat("\n");show(Amidihinfusiondpar);cat("\n\n")
##     Amidihinfusiondpar<-check(Amidihinfusiondpar)
     d<-Amidihinfusiondpar[1,2]*cl/0.8
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css (mg/L)"," -> Infusion/dosing rate (mg/hr)"),
                             Values=c(Amidihinfusiondpar[1,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dosing Rate -> Css",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_infusionR_d_to_c()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Amidihinfusioncpar<-data.frame(input=c("Infusion/dosing Rate (mg/hr)"),value=c(d))
     Amidihinfusioncpar<-edit(Amidihinfusioncpar)
     ### cat("\n");show(Amidihinfusioncpar);cat("\n\n")
##     Amidihinfusioncpar<-check(Amidihinfusioncpar)
     C<-0.8*Amidihinfusioncpar[1,2]/(cl)
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Infusion/dosing Rate (mg/hr)"," -> Css (mg/L)"),
                             Values=c(Amidihinfusioncpar[1,2],C))
     cat("\n");show(coutput);cat("\n")
          Amidihinfusion.more()
       } else {
             if (pick == 2){
               	cal.again()
         }
  }} else {
        if (pick == 2){
          cal.again()   
    }  
  }
}


Oxtir.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Oxtirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(10,8))
     Oxtirdpar<-edit(Oxtirdpar)
     ### cat("\n");show(Oxtirdpar);cat("\n\n")
##     Oxtirdpar<-check(Oxtirdpar)
     d<-Oxtirdpar[1,2]/(0.65*1.85/((v_F)*(1.85-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Oxtirdpar[2,2])))*
        exp(-(cl_F)/(v_F)*Oxtirdpar[2,2])-(1/(1-exp(-1.85*Oxtirdpar[2,2])))*exp(-1.85*Oxtirdpar[2,2])))
     Tmax<-log(1.85-(cl_F/v_F))/(1.85-(cl_F/v_F))    # estimate Tmax first, and
     Cmax<-TheIRsscpr(0.65,d,Oxtirdpar[2,2],Tmax) # then calc. estimated Cpeak
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Target Css_trough (mg/L)","**calc Css_peak (mg/L)","tau (hr)"," -> Dose (mg)"),
                              value=c(Oxtirdpar[1,2],Cmax,Oxtirdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c()
     cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Oxtircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Oxtirdpar[2,2]))
     Oxtircpar<-edit(Oxtircpar)
     ### cat("\n");show(Oxtircpar);cat("\n\n")
##     Oxtircpar<-check(Oxtircpar)
     C<-1.85*0.65*Oxtircpar[1,2]/((v_F)*(1.85-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Oxtircpar[2,2])))*
        exp(-(cl_F)/(v_F)*Oxtircpar[2,2])-(1/(1-exp(-1.85*Oxtircpar[2,2])))*exp(-1.85*Oxtircpar[2,2]))
     Tmax<-log(1.85-(cl_F/v_F))/(1.85-(cl_F/v_F))    # estimate Tmax first, and
     Cmax<-TheIRsscpr(0.65,Oxtircpar[1,2],Oxtircpar[2,2],Tmax) # then calc. estimated Cpeak
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("D (mg)","tau (hr)","calc Css_Peak (mg/L)","calc Css_trough (mg/L)"),
                              value=c(Oxtircpar[1,2],Oxtircpar[2,2],Cmax,C))
     cat("\n");show(coutput);cat("\n")
          Oxtir.more()
       } else {
             if (pick == 2){
             cal.again()
        }
  }} else {
  	if (pick == 2){
        cal.again()   
    }  
  }
}



Oxtcr.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
  cat("\n")
  file.menu <- c("Css -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d();cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Oxtcrdpar<-data.frame(input=c("Css (mg/L)","tau (hr)"),value=c(10,12))
     Oxtcrdpar<-edit(Oxtcrdpar)
     ### cat("\n");show(Oxtcrdpar);cat("\n\n")
##     Oxtcrdpar<-check(Oxtcrdpar)
     d<-Oxtcrdpar[1,2]*cl_F*Oxtcrdpar[2,2]/0.65
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Oxtcrdpar[1,2],Oxtcrdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c();cat("\n")
     cat("\n *** Suggested targeted theophylline plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     cat("\n")
     note_for_close_window()
     Oxtcrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Oxtcrdpar[2,2]))
     Oxtcrcpar<-edit(Oxtcrcpar)
     ### cat("\n");show(Oxtcrcpar);cat("\n\n")
##     Oxtcrcpar<-check(Oxtcrcpar)
     C<-0.65*Oxtcrcpar[1,2]/(cl_F*Oxtcrcpar[2,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css (mg/L)"),
                             Values=c(Oxtcrcpar[1,2],Oxtcrcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Oxtcr.more()
       } else {
             if (pick == 2){
             	cal.again()
        }
  }} else {
        if (pick == 2){
        cal.again()   
    }  
  }
}



Their.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d();cat("\n")
     cat("\n *** Suggested theophylline targeted plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     note_for_close_window()
     Theirdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(10,8))
     Theirdpar<-edit(Theirdpar)
     ### cat("\n");show(Theirdpar);cat("\n\n")
##     Theirdpar<-check(Theirdpar)
     d<-Theirdpar[1,2]/(1.85/((v_F)*(1.85-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Theirdpar[2,2])))*
        exp(-(cl_F)/(v_F)*Theirdpar[2,2])-(1/(1-exp(-1.85*Theirdpar[2,2])))*exp(-1.85*Theirdpar[2,2])))
     Tmax<-log(1.85-(cl_F/v_F))/(1.85-(cl_F/v_F))    # estimate Tmax first, and
     Cmax<-TheIRsscpr(1.0,d,Theirdpar[2,2],Tmax) # then calc. estimated Cpeak
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Target Css_trough (mg/L)","**calc Css_peak (mg/L)","tau (hr)"," -> Dose (mg)"),
                              value=c(Theirdpar[1,2],Cmax,Theirdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c();cat("\n")
     cat("\n *** Suggested theophylline targeted plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     note_for_close_window()
     Theircpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Theirdpar[2,2]))
     Theircpar<-edit(Theircpar)
     ### cat("\n");show(Theircpar);cat("\n\n")
##     Theircpar<-check(Theircpar)
     C<-1.85*Theircpar[1,2]/((v_F)*(1.85-(cl_F)/(v_F)))*((1/(1-exp(-(cl_F)/(v_F)*Theircpar[2,2])))*
        exp(-(cl_F)/(v_F)*Theircpar[2,2])-(1/(1-exp(-1.85*Theircpar[2,2])))*exp(-1.85*Theircpar[2,2]))
     Tmax<-log(1.85-(cl_F/v_F))/(1.85-(cl_F/v_F))             # estimate Tmax first, and
     Cmax<-TheIRsscpr(1.0,Theircpar[1,2],Theircpar[2,2],Tmax) # then calc. estimated Cpeak
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("D (mg)","tau (hr)","calc Css_peak (mg/L)"," -> Css_trough (mg/L)"),
                              value=c(Theircpar[1,2],Theircpar[2,2],Cmax,C))
     cat("\n");show(coutput);cat("\n")
          Their.more()
       } else {
             if (pick == 2){
             	cal.again()
        }
  }} else {
        if (pick == 2){
           cal.again()   
    }  
  }
}


Thecr.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
  cat("\n")
  file.menu <- c("Css -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d();cat("\n")
     cat("\n *** Suggested theophylline targeted plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     note_for_close_window()
     Thecrdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(10,12))
     Thecrdpar<-edit(Thecrdpar)
     ### cat("\n");show(Thecrdpar);cat("\n\n")
##     Thecrdpar<-check(Thecrdpar)
     d<-Thecrdpar[1,2]*cl_F*Thecrdpar[2,2]
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Thecrdpar[1,2],Thecrdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c();cat("\n")
     cat("\n *** Suggested theophylline targeted plasma conc. range ***\n\n")
     cat("       Adult: 5-15 mg/L; child: 5-10 mg/L\n\n")
     note_for_close_window()
     Thecrcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Thecrdpar[2,2]))
     Thecrcpar<-edit(Thecrcpar)
     ### cat("\n");show(Thecrcpar);cat("\n\n")
##     Thecrcpar<-check(Thecrcpar)
     C<-Thecrcpar[1,2]/(cl_F*Thecrcpar[2,2])
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css (mg/L)"),
                             Values=c(Thecrcpar[1,2],Thecrcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Thecr.more()
       } else {
             if (pick == 2){
             	cal.again()
        }
  }} else {
        if (pick == 2){
           cal.again()   
    }  
  }
}
  
Val.more<-function()
{
  X <- read.table("params.csv",header=FALSE)
  cl_F <- X[1,2]
  vd_F <- X[3,2] 
  ka   <- X[2,2] 

  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d();cat("\n")
     cat("\n *** Suggested valproate target plasma conc. range:\n\n")
     cat("\n     epilepsy: 50-100 mg/L; mania: 85-125 mg/L       \n")
     cat("\n")
     note_for_close_window()
     Valdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(80,12))
     Valdpar<-edit(Valdpar)
     ### show(Valdpar);cat("\n\n")     
##     Valdpar<-check(Valdpar)
     d<-Valdpar[1,2]/(ka/(vd_F*(ka-(cl_F/vd_F)))*((1/(1-exp(-(cl_F/vd_F)*Valdpar[2,2])))*exp(-(cl_F/vd_F)*Valdpar[2,2])-
        (1/(1-exp(-ka*Valdpar[2,2])))*exp(-ka*Valdpar[2,2])))
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Valdpar[1,2],Valdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c();cat("\n")
     cat("\n *** Suggested valproate target plasma conc. range:\n\n")
     cat("\n     epilepsy: 50-100 mg/L; mania: 85-125 mg/L       \n")
     cat("\n")
     note_for_close_window()
     Valcpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Valdpar[2,2]))
     Valcpar<-edit(Valcpar)
     ### show(Valcpar);cat("\n\n")  
##     Valcpar<-check(Valcpar)
     C<-ka*Valcpar[1,2]/(vd_F*(ka-(cl_F/vd_F)))*((1/(1-exp(-(cl_F/vd_F)*Valcpar[2,2])))*exp(-(cl_F/vd_F)*Valcpar[2,2])-
        (1/(1-exp(-ka*Valcpar[2,2])))*exp(-ka*Valcpar[2,2]))
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css_trough (mg/L)"),
                             Values=c(Valcpar[1,2],Valcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Val.more()
       } else {
             if (pick == 2){
             	 cal.again()
        }
  }} else {
        if (pick == 2){
           cal.again()   
    }  
  }
}
 
  
Cyc.more<-function(A,B)
{
X <- read.table("params.csv",header=FALSE)
cl <- X[1,2]
## A = PTD (post-transplant day)
## B = bw (kg)

 cat("\n")
  file.menu <- c("C(2)ss <-> Dose",            ### added by YJ
                 "Css_trough <-> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d_mcg();cat("\n")
     cat(" C(2)ss: target C(2) conc. or 'C2';\n Cys conc. at 2 hr post-dose at steady-state.\n\n")
     cat("-----------------------------------------\n")
     cat(" Transpiant      Time          Target C2 \n")
     cat("             post-transplant     conc.   \n")
     cat("               (months)         (mcg/L)  \n")
     cat("-----------------------------------------\n")
     cat(" Renal            1             1,700    \n")
     cat("                  2             1,500    \n")
     cat("                  3             1,300    \n")
     cat("                 4-6            1,100    \n")
     cat("                 7-12             900    \n")
     cat("                 > 12             800    \n")
     cat("-----------------------------------------\n")
     cat(" Liver           0-3            1,000    \n")
     cat("                 4-6              800    \n")
     cat("                 > 6              600    \n")
     cat("-----------------------------------------\n\n")
     note_for_close_window()
     ### new function: use C(2) to adjust dose... -YJ
     Cycdpar<-data.frame(input=c("C(2)ss (mcg/L)","tau (hr)"),value=c(600,12))
     Cycdpar<-edit(Cycdpar)
     ### show(Cycdpar);cat("\n\n") 
##     Cycdpar<-check(Cycdpar)
     A<-30  ### arbitary value; just to make sure it is at SS. -YJ
     d<-Cycdpar[1,2]*exp(-cl/(4*B)*(Cycdpar[2,2]-2))/((0.2+10*abs(A-7)/((A+10)*60))*1000*0.3/((4*B)*(0.3-(cl/(4*B))))*
        ((1/(1-exp(-(cl/(4*B))*Cycdpar[2,2])))*exp(-(cl)/(4*B)*Cycdpar[2,2])-(1/(1-exp(-0.3*Cycdpar[2,2])))*
        exp(-0.3*Cycdpar[2,2])))  ### C(12) = C(2)*exp(-kel*(tau-2)); "Cycdpar[1,2]*exp(-cl/(4*B)*(Cysdpar[2,2]-2))"
                                  ### assumed it is one-compartment model. -YJ
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("C(2)ss (mcg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Cycdpar[1,2],Cycdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> C(2)ss",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c_mcg();cat("\n")
     cat(" C(2)ss: target C(2) conc. or 'C2';\n Cys conc. at 2 hr post-dose at steady-state.\n\n")
     cat("-----------------------------------------\n")
     cat(" Transpiant      Time          Target C2 \n")
     cat("             post-transplant     conc.   \n")
     cat("               (months)         (mcg/L)  \n")
     cat("-----------------------------------------\n")
     cat(" Renal            1             1,700    \n")
     cat("                  2             1,500    \n")
     cat("                  3             1,300    \n")
     cat("                 4-6            1,100    \n")
     cat("                 7-12             900    \n")
     cat("                 > 12             800    \n")
     cat("-----------------------------------------\n")
     cat(" Liver           0-3            1,000    \n")
     cat("                 4-6              800    \n")
     cat("                 > 6              600    \n")
     cat("-----------------------------------------\n\n")
     cat("\n")
     note_for_close_window()
     Cyccpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Cycdpar[2,2]))
     Cyccpar<-edit(Cyccpar)
     ### show(Cyccpar);cat("\n\n") 
##     Cyccpar<-check(Cyccpar)
     C<-((0.2+10*abs(A-7)/((A+10)*60))*Cyccpar[1,2]*1000*0.3/((4*B)*(0.3-(cl/(4*B))))*((1/(1-exp(-(cl/(4*B))*Cyccpar[2,2])))*
         exp(-(cl)/(4*B)*Cyccpar[2,2])-(1/(1-exp(-0.3*Cyccpar[2,2])))*exp(-0.3*Cyccpar[2,2])))/exp(-cl/(4*B)*(Cyccpar[2,2]-2))
         ### original C is Ctrough_ss; now C(2)ss = Ctrough_ss/exp(-kel*(tau-2)); assumed it is one-compartment model. -YJ
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> C(2)ss (mcg/L)"),
                             Values=c(Cyccpar[1,2],Cyccpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Cyc.more(A,B)
       } else {
             if (pick == 2){	
             	cal.again()
         }
  }}
  else{
  if (pick == 2){
     cat("\n")
     note_for_c_to_d_mcg();cat("\n")
     cat(" Css_trough or C(0)ss: target trough conc.;\n Cys conc. right before next dose at steady-state.\n\n")
     cat("-------------------------------------------\n")
     cat(" Transpiant      Time       Target C(0)ss \n")
     cat("            post-transplant      conc.    \n")
     cat("                (days)         (mcg/L)    \n")
     cat("-------------------------------------------\n")
     cat(" General        > 7           100-500    \n")
     cat(" Renal          > 7           100-350    \n")
     cat(" Liver          > 7           200-500    \n")
     cat(" Bone marrow    > 7           250-500    \n")
     cat("-----------------------------------------\n\n")
     cat("\n")
     note_for_close_window()
     ### if want to use C(2) to adjust dose... is it possible here? Yes, I think so. -YJ
     Cycdpar<-data.frame(input=c("Css_trough (mcg/L)","tau (hr)"),value=c(300,12))
     Cycdpar<-edit(Cycdpar)
     ### show(Cycdpar);cat("\n\n") 
##     Cycdpar<-check(Cycdpar)
     d<-Cycdpar[1,2]/((0.2+10*abs(A-7)/((A+10)*60))*1000*0.3/((4*B)*(0.3-(cl/(4*B))))*((1/(1-exp(-(cl/(4*B))*Cycdpar[2,2])))*
        exp(-(cl)/(4*B)*Cycdpar[2,2])-(1/(1-exp(-0.3*Cycdpar[2,2])))*exp(-0.3*Cycdpar[2,2])))
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mcg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Cycdpar[1,2],Cycdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c_mcg();cat("\n")
     cat(" Css_trough or C(0)ss: target trough conc.;\n Cys conc. right before next dose at steady-state.\n\n")
     cat("-------------------------------------------\n")
     cat(" Transpiant      Time       Target C(0)ss \n")
     cat("            post-transplant      conc.    \n")
     cat("                (days)         (mcg/L)    \n")
     cat("-------------------------------------------\n")
     cat(" General        > 7           100-500    \n")
     cat(" Renal          > 7           100-350    \n")
     cat(" Liver          > 7           200-500    \n")
     cat(" Bone marrow    > 7           250-500    \n")
     cat("-----------------------------------------\n\n")
     note_for_close_window()
     Cyccpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Cycdpar[2,2]))
     Cyccpar<-edit(Cyccpar)
     ### show(Cyccpar);cat("\n\n") 
##     Cyccpar<-check(Cyccpar)
     C<-(0.2+10*abs(A-7)/((A+10)*60))*Cyccpar[1,2]*1000*0.3/((4*B)*(0.3-(cl/(4*B))))*((1/(1-exp(-(cl/(4*B))*Cyccpar[2,2])))*exp(-(cl)/(4*B)*Cyccpar[2,2])-(1/(1-exp(-0.3*Cyccpar[2,2])))*exp(-0.3*Cyccpar[2,2]))
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css_trough (mcg/L)"),
                             Values=c(Cyccpar[1,2],Cyccpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Cyc.more(A,B)
       } else {
             if (pick == 2){	
             	  cal.again()
         }
  }}
     else {
        if (pick == 3){
           cal.again()   
    }  
  }
 }
}


Ima.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
v_F  <- X[2,2]
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d();cat("\n")
     cat(" --- Suggested imatinib target plasmac conc.: unknown (around 1.0 mg/L?)\n\n")
     note_for_close_window()
     Imadpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(1.0,24))
     Imadpar<-edit(Imadpar)
     ### show(Imadpar);cat("\n\n") 
##     Imadpar<-check(Imadpar)
     d<-Imadpar[1,2]/(((1/(1.5*(cl_F)))*(1-exp(-(cl_F)/(v_F)*1.5))*(exp(-(cl_F)/(v_F)*(Imadpar[2,2]-1.5))))/(1-(exp(-(cl_F)/(v_F)*Imadpar[2,2]))))
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Imadpar[1,2],Imadpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c();cat("\n")
     cat(" --- Suggested imatinib target plasmac conc.: unknown (around 1.0 mg/L?)\n\n")
     note_for_close_window()
     Imacpar<-data.frame(input=c("D (mg)","tau (hr)"),value=c(d,Imadpar[2,2]))
     Imacpar<-edit(Imacpar)
     ### show(Imacpar);cat("\n\n") 
##     Imacpar<-check(Imacpar)
     C<-((Imacpar[1,2]/(1.5*(cl_F)))*(1-exp(-(cl_F)/(v_F)*1.5))*(exp(-(cl_F)/(v_F)*(Imacpar[2,2]-1.5))))/(1-(exp(-(cl_F)/(v_F)*Imacpar[2,2])))
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css_trough (mg/L)"),
                             Values=c(Imacpar[1,2],Imacpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          Ima.more()
       } else {
             if (pick == 2){
             	  cal.again()
        }
  }} else {
        if (pick == 2){
           cal.again()   
    }  
  }
}



ChiVal.more<-function()
{
X <- read.table("params.csv",header=FALSE)
cl_F <- X[1,2]
vd_F <- X[3,2] 
ka   <- X[2,2] 
  cat("\n")
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d();cat("\n")
     cat("\n *** Suggested valproate target plasma conc. range:\n\n")
     cat("\n     epilepsy: 50-100 mg/L; mania: 85-125 mg/L       \n")
     cat("\n")
     note_for_close_window()
     ChiValdpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(80,12))
     ChiValdpar<-edit(ChiValdpar)
     ### show(ChiValdpar);cat("\n\n")
##     ChiValdpar<-check(ChiValdpar)
     d<-ChiValdpar[1,2]/(ka/((vd_F)*(ka-(cl_F)/(vd_F)))*((1/(1-exp(-(cl_F)/(vd_F)*ChiValdpar[2,2])))*
        exp(-(cl_F)/(vd_F)*ChiValdpar[2,2])-(1/(1-exp(-ka*ChiValdpar[2,2])))*exp(-ka*ChiValdpar[2,2])))
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(ChiValdpar[1,2],ChiValdpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n");cat("\n")
     cat("\n *** Suggested valproate target plasma conc. range:\n\n")
     cat("\n     epilepsy: 50-100 mg/L; mania: 85-125 mg/L       \n")
     cat("\n")
     note_for_close_window()
     ChiValcpar<-data.frame(input=c("D (mg)","tau (hr)"),Value=c(d,ChiValdpar[2,2]))
     ChiValcpar<-edit(ChiValcpar)
##     ChiValcpar<-check(ChiValcpar)
     ### show(ChiValcpar);cat("\n\n")
     C<-ka*ChiValcpar[1,2]/((vd_F)*(ka-(cl_F)/(vd_F)))*((1/(1-exp(-(cl_F)/(vd_F)*ChiValcpar[2,2])))*
        exp(-(cl_F)/(vd_F)*ChiValcpar[2,2])-(1/(1-exp(-ka*ChiValcpar[2,2])))*exp(-ka*ChiValcpar[2,2]))
     ### sim<-matrix(C[1 ,1])
     coutput<-data.frame(Parameters=c("Dose (mg)","tau (hr)"," -> Css (mg/L)"),
                             Values=c(ChiValcpar[1,2],ChiValcpar[2,2],C))
     cat("\n");show(coutput);cat("\n")
          ChiVal.more()
       } else {
             if (pick == 2){
             	cal.again()
        }
  }} else {
        if (pick == 2){
           cal.again()   
    }  
  }
}
  

Phe.more<-function()
{
 cat("\n")
     ### read saved PK parameters from file. -YJ
     X <- read.table("params.csv",header=FALSE)
     km   <- X[1,2]
     vmax <- X[2,2]
 
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){
     cat("\n")
     note_for_c_to_d();cat("\n")
     cat(" --- Suggested target Phenytoin therapeutic ranges ---\n")
     cat(" ----------------------------------------------\n")
     cat("   Adult:   Total plasma conc.  10-20 (mg/L) \n")
     cat("   Neonate: Total plasma conc.   8-15 (mg/L) \n")
     cat(" ----------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     Phedpar<-data.frame(input=c("Css_trough (mg/L)","tau (hr)"),value=c(15,24))
     Phedpar<-edit(Phedpar)     ### un-remarked this for flexibility; use default values here to save time. -YJ
     ## Phedpar<-check(Phedpar)
     d<-(vmax*Phedpar[1,2])/(km+Phedpar[1,2])/(24/Phedpar[2,2])
     ### sim<-matrix(d[1 ,1])
     doutput<-data.frame(Parameters=c("Css_trough (mg/L)","tau (hr)"," -> Dose (mg)"),
                             Values=c(Phedpar[1,2],Phedpar[2,2],d))
     cat("\n");show(doutput);cat("\n")
   file.menu <- c("Dose -> Css_trough",
                  "exit")
   pick <- menu(file.menu, title = "<< D -> C Adjustment >>") 
    if (pick == 1){
     cat("\n")
     note_for_d_to_c();cat("\n")
     cat(" --- Suggested target Phenytoin therapeutic ranges ---\n")
     cat(" ----------------------------------------------\n")
     cat("   Adult:   Total plasma conc.  10-20 (mg/L) \n")
     cat("   Neonate: Total plasma conc.   8-15 (mg/L) \n")
     cat(" ----------------------------------------------\n")
     cat("\n")
     note_for_close_window()
     Phecpar<-data.frame(input=c("Dose (mg)","tau (hr)"),value=c(d,24))
     Phecpar<-edit(Phecpar)
##     Phecpar<-check(Phecpar)
     Phelimit(Phecpar[1,2],Phecpar[2,2])    
         } else {
             if (pick == 2){
              cat("\n\n")
              unlink("params.csv")
             	cal.again()
        }
  }} else {
        if (pick == 2){
          unlink("params.csv")
          cal.again()   
    }  
  }
}


### War.more<-function()
### {
### 
###   cat("\n")
###   file.menu <- c("INR -> Dose",
###                  "exit")
###   pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
###   if (pick == 1){
###      cat("\n")
###      note_for_c_to_d_war()
###      cat("\n")
###      note_for_close_window()
###      Wardpar<-data.frame(input=c("INR","tau (day)"),value=c(2,1.))
###      Wardpar<-edit(Wardpar)        ### keep as edit-able for user to make decision here -YJ
### ##     Wardpar<-check(Wardpar)
###      show(Wardpar);cat("\n\n")
###      d<-Wardpr(Wardpar[1,2],Wardpar[2,2])
###      ### sim<-matrix(d[1 ,1])
###      doutput<-data.frame(d)
###      colnames(doutput)<-list("*** calculated Dose (mg)")
###      cat("\n")
###      show(doutput)
###      cat("\n")
###    file.menu <- c("Dose -> INR",
###                   "exit")
###    pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
###     if (pick == 1){
###      cat("\n")
###      note_for_d_to_c_war()
###      cat("\n")
###      note_for_close_window()
###      Warcpar<-data.frame(input=c("D (mg)","tau (day)"),value=c(2.5,1.))
###      Warcpar<-edit(Warcpar)
###      show(Warcpar);cat("\n\n")
### ##     Warcpar<-check(Warcpar)
###      C<-Warcpr(Warcpar[1,2],Warcpar[2,2])
###      ### sim<-matrix(C[1 ,1])
###      coutput<-data.frame(C)
###      colnames(coutput)<-list("calculated INR")
###      cat("\n")
###      show(coutput)
###         cat("\n")
###           War.more()
###        } else {
###              if (pick == 2){
###              	cal.again()
###         }
###   }} else {
###         if (pick == 2){
###           cal.again()   
###     }  
###   }
### }
### 


