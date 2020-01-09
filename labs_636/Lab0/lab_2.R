## lab 2 
baby.max = function(a,b){
  if(a>b){return(a)}
  else{return(b)}
}

baby.max(3,4)
#--------------------------------------------
#--------------------------------------------
square.a = function(a = 1, b = 2){
  cat(seq = "","square.a(a = ",a,",b=",b,")\n")
  b = 100
  c=a*a
  return(c)
}
square.a (a = 3, b= 4 )

#--------------------------------------------
#--------------------------------------------
##while loop

x <- 1
while (x<10){
  cat("x= ", x, "\n")
  x = 2*x
}



#--------------------------------------------
#--------------------------------------------

devtools::install_github("yihui/xaringan")
install.packages("devtools")
if(!requireNamespace("xaringan"))
devtools::install_github("yihui/xaringan")

#--------------------------------------------
#--------------------------------------------







