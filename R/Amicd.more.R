Ami.more<-function(B,E,F,G,Amicpar=NULL,coutput=NULL,Amidpar=NULL,doutput=NULL)
{
 cat("\n")                                     
  file.menu <- c("Css_trough -> Dose",
                 "exit")
  pick <- menu(file.menu, title = "<< Dose Adjustment >>") 
  if (pick == 1){                                                                              
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
     d<-Amidpar[1,2]/(((1-exp(-(samplesStats("cl"))/(samplesStats("v"))*Amidpar[3,2]))/(Amidpar[3,2]*(samplesStats("cl"))*(1-exp(-(samplesStats("cl"))/(samplesStats("v"))*Amidpar[2,2]))))*exp(-(samplesStats("cl"))/(samplesStats("v"))*(Amidpar[2,2]-Amidpar[3,2])))  
     sim<-matrix(d[1 ,1])
     doutput<-data.frame(sim)
     colnames(doutput)<-list("Dose (mg)")
     cat("\n")
     show(doutput)
       cat("\n")                          
       file.menu <- c("Dose -> Css_trough",
                      "exit")
       pick <- menu(file.menu, title = "<< Dose Adjustment again? >>")
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
          C<-(Amicpar[1,2]*(1-exp(-(samplesStats("cl"))/(samplesStats("v"))*Amicpar[3,2]))/(Amicpar[3,2]*(samplesStats("cl"))*(1-exp(-(samplesStats("cl"))/(samplesStats("v"))*Amicpar[2,2]))))*exp(-(samplesStats("cl"))/(samplesStats("v"))*(Amicpar[2,2]-Amicpar[3,2]))   
          sim<-matrix(C[1 ,1])                                                                       
          coutput<-data.frame(sim)                                                                  
          colnames(coutput)<-list("Css_trough (mg/L)")                                              
          cat("\n")
          show(coutput)
          cat("\n")
          Amicd.more(B,E,F,G,Amicpar,coutput,Amidpar,doutput)
       } else {
             if (pick == 2){                                           
             	if (is.null(coutput)||is.null(Amicpar)){
                  Amiss.output(B,E,F,G,Amidpar,doutput)	
                  cal.again()
           	}else{
      	        Amisscd.output(B,E,F,G,Amicpar,coutput,Amidpar,doutput)
                cal.again()
             }
         } 
  } }else {
      if (pick == 2){               
           if (is.null(doutput)||is.null(Amidpar)||is.null(coutput)||is.null(Amicpar)){
             Amiss.pkoutput(B,E,F,G)	
             cal.again()
           	}else{
      	Amisscd.output(B,E,F,G,Amidpar,doutput,Amicpar,coutput)
        cal.again()
        }           
      }
    }  
  }
