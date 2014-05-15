### Simulation
### One compartment PK model extravascualr single dose first-order absorption 
### for tdm simulation plot
###
plots_tdm <- function(Subject=NULL,  # N Subj's 
                      Dose=NULL,     # single dose
                      ka=NULL,     
                      Vd=NULL,
                      kel=NULL,      
                      nDose=NULL,    ## for multiple-dose
                      Tau=NULL,
                      MD=FALSE)
{
options(warn=-1)
is.Fentanyl<-is.Fentanyl

if(MD){

PKtime<-data.frame(time=seq(0.1,Tau*nDose,0.1))  ### set '0.01' to see more output details.

        defun <- function(time, y, parms) { 
          dy1 <- -parms["ka"] * y[1]
          dy2 <-  parms["ka"] * y[1]/parms["Vd"] -parms["kel"]*y[2]
          list(c(dy1,dy2)) 
        } 
}
else {    ### single-dose sim from here

PKtime<-data.frame(time=seq(0,24,1)) 

        defun <- function(time, y, parms) { 
           dy1dt <- -parms["ka"] * y[1]
           dy2dt <-  parms["ka"] * y[1]/parms["Vd"] - parms["kel"] * y[2]
           list(c(dy1dt,dy2dt)) 
        } 
}  
   
      pick <- 1    ### use no error sim here
     
      type<-switch(pick, 
                  "No Error",
                  "Error = Normal Error", 
                  "Error = Uniform Error",
                  "Error = Normal Error*True Value",
                  "Error = Uniform Error*True Value")
      
          Subject<- 1; i<- 1
          PKindex<-vector(Subject,mode="list")
           cat("\n\n")                                                               
           cat("*******************************************************\n")        
           cat(" Summary Table                                       \n\n")   
           if(MD)
           cat(" Model: 1-compartment, extravascular, multiple-dose,  \n")
           else    
           cat(" Model: 1-compartment, extravascular, single-dose,    \n")
           cat("      & 1st-ordered absorption/elim. without lag time\n")
           cat("            Subject #:", Subject,"                   \n")
           if(is.Fentanyl)
           cat("           Dose (mcg):", Dose,"                      \n") 
           else
           cat("            Dose (mg):", Dose/1000,"                 \n") 
           if(MD) {
           cat(" Dosing Interval (hr):", Tau,"                       \n")
           cat("            # of Dose:", nDose,"                     \n")}
           cat("*******************************************************\n\n")      
                
          ### PKindex[[i]]<-plot_tdm.out(PKtime,ka,kel,Vd,defun,Dose,i,MD)   
          time<-PKtime$time
          parms<-c(ka=ka,kel=kel,Vd=Vd) 
          if(MD){
             dosing.time<-seq(Tau,Tau*nDose,Tau)
             yini<-c(dy1=Dose,dy2=0)
             events <- data.frame(var="dy1",time=dosing.time,value=Dose,method="add")
             C1.lsoda<-data.frame(lsode(yini,c(0,time),defun,parms,rtol=1e-08,atol=1e-08,
                                  events=list(data=events)))
          }
          else{
          ### C1.lsoda<-data.frame(lsoda(c(Dose,0),c(0,time),defun,parms,rtol=c(1e-10,1e-10),atol=c(1e-10,1e-10)))
          C1.lsoda<-data.frame(lsode(c(Dose,0),c(0,time),defun,parms))}
          ### write.csv(C1.lsoda,file="C1.lsoda.csv",row.names=F,col.names=T)
          
          good<-ifelse(C1.lsoda[1:(length(time)+1),3]<=1e-8,  ### since PKtime is set not to include time = 0, so here
                       0,                                     ### we start from [1:lenght(...)] to include time = 0;
                       C1.lsoda[1:(length(time)+1),3])        ### just for plot conc.vs. time graph concern; -YJ
          PKindex<-data.frame(i,
                              C1.lsoda[1:(length(time)+1),1],
                              good)
          x<-C1.lsoda[1:(length(time)+1),1]
          y<-good
          plotting_tdm.sim(i,x,y,MD,Dose,Tau)          
}