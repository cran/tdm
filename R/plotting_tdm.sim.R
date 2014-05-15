### ----------------plot for simulation----------------
###
plotting_tdm.sim <- function(i,x,y,MD=FALSE,Dose,Tau) 
{

dev.new()
par(mfrow=c(1,1))

MEC<-MEC
MSC<-MSC
Cav<-Cav
y_label<-y_label
main_title<-main_title
main_title_SD<-""
is.Fentanyl<-is.Fentanyl
ocIRSMpar<-ocIRSMpar
main_title_SD<-paste(main_title,"- calc. conc. after the 1st-dose")
   
if(MD){
  ## linear plot
  plot(y~x,type='l',main=main_title,     ### plot lines only('l'ine only)
       xlab="Time after dosing (hr)",ylab=y_label,pch=15,col="black",bty="l",
       font.lab=2,cex.lab=1,cex.axis=1,cex.main=1,ylim=c(0,MSC*1.5)) 
  text("Linear",side=3,cex=0.88)
  abline(h=MEC, col="red", type="l", lwd=2)
  abline(h=MSC, col="red", type="l", lwd=2)
  abline(h=Cav, col="blue", type="l", lwd=2)
  mec_all<-paste("MEC =",MEC,"ng/mL")
  msc_all<-paste("MSC =",MSC,"ng/mL")
  cav_all<-paste("Cav_ss =",Cav,"ng/mL")
  text(max(x)/2,MEC/1.65,mec_all,col=2)
  text(max(x)/2,MSC*1.05,msc_all,col=2)
  text(max(x)/6,MSC*0.8,cav_all,col = "blue")
  if(is.Fentanyl)
  legend("top",legend= as.expression(c(paste("dosing regimen:",formatC(Dose,format="f",digits=1),
         "mcg, q",as.integer(Tau),"h (rounded), PO"))),
           xjust=0,yjust=0,box.col="white",box.lwd=0,cex=1.2)
  else
  legend("top",legend= as.expression(c(paste("dosing regimen:",formatC(Dose/1000,format="f",digits=1),
         "mg, q",as.integer(Tau),"h (rounded), PO"))),
           xjust=0,yjust=0,box.col="white",box.lwd=0,cex=1.2)
}
else{
  ## linear plot
  xx<-ocIRSMpar$ts
  yy<-ocIRSMpar$conc
  plot(y~x,type='b',main=main_title_SD,    ### plot lines and symbols ('b'oth)
       xlab="Time after dosing (hr)",ylab=y_label,col="black",bty="l",
       font.lab=2,cex.lab=1,cex.axis=1,cex.main=1) 
  text("Linear",side=3,cex=0.88)
  points(xx,yy,col="red",pch="X",lwd=2)
  text(max(x)/2,max(y)/2,"X - obs. conc.", col = "red")
  if(is.Fentanyl)
  legend("top",legend= as.expression(c(paste("dosing regimen:",formatC(Dose,format="f",digits=1),
         "mcg, single-dose"))),xjust=0,yjust=0,box.col="white",box.lwd=0,cex=1.2)
  else
  legend("top",legend= as.expression(c(paste("dosing regimen:",formatC(Dose/1000,format="f",digits=1),
         "mg, single-dose"))),xjust=0,yjust=0,box.col="white",box.lwd=0,cex=1.2)
}
}
