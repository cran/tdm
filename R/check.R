# check if the user correctly enters initial values for PK parameters ([1,2] should not be NA)
check<-function(a)
{
  repeat{
  if ( a[1,2] == 0){
  cat("\n")
  cat("*******************************************\n")
  cat("     Lose editing some parameters          \n")
  cat("     Press enter to continue...            \n")
  cat("*******************************************\n")
  readline()
  cat("\n")
  a<-edit(a)
  }
 else{
   break
   return(edit(a))
 }
}
  cat("\n")
  show(a)
}

# check if the user correctly enters initial values for PK parameters ([6,2] should not be NA)
ycheck<-function(b)
{
  repeat{
  if ( b[6,2] == 0){
  cat("\n")
  cat("*******************************************\n")
  cat("     Lose editing some parameters         \n")
  cat("     Press enter to continue...            \n")
  cat("*******************************************\n")
  readline()
  cat("\n")
  b<-edit(b)
  }
 else{
   break
   return(edit(b))
 }
}
  cat("\n")
  show(b)
}

# check if the user correctly enters initial values for PK parameters ([4,2] should not be NA)
zcheck<-function(c)
{
  repeat{
  if ( c[4,2] == 0){
  cat("\n")
  cat("*******************************************\n")
  cat("     Lose editing some parameters.         \n")
  cat("     Press enter to continue...            \n")
  cat("*******************************************\n")
  readline()
  cat("\n")
  c<-edit(c)
  }
 else{
   break
   return(edit(c))
 }
}
  cat("\n")
  show(c)
}

# check if the user correctly enters initial values for PK parameters ([3,2] should not be NA)
wcheck<-function(c)
{
  repeat{
  if ( c[3,2] == 0){
  cat("\n")
  cat("*******************************************\n")
  cat("     Lose editing some parameters.         \n")
  cat("     Press enter to continue...            \n")
  cat("*******************************************\n")
  readline()
  cat("\n")
  c<-edit(c)
  }
 else{
   break
   return(edit(c))
 }
}
  cat("\n")
  show(c)
}