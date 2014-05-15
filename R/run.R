# List of Drugs Menu
run<-function()
{
options(warn=-1)
options(digits=5)

####
par(mar=c(0, 0, 0, 0)) 
plot(0, xlim=c(0, 210), ylim=c(0, 297), col="white")  ### will show ugly border line. too bad!  -YJ
logo<-readPNG(system.file("img","tdm_logo-2013.png",package="tdm"),TRUE)
grid.raster(logo,width=unit(1,"npc"),height=unit(1,"npc"))     ### this one works great without showing border. -YJ
text(100,100,"Welcome to using tdm v3.0.2",cex = 1.2)
text(100,70,"tdm is developed by Miao-ting Chen (MT) & Yung-jin Lee (YJ).",cex = 1.2)
### text(100,60,"Kaohsiung Veterans General Hospital (MT) &",cex = 1.2)
### text(100,50,"ptpc inc. (YJ),",cex = 1.2)
text(100,60,"Kaohsiung City, Taiwan",cex = 1.2)
readline(" Press Enetr to continue...");dev.off()
### Sys.sleep(3.6);dev.off()
### start from here
go2menu()
}