adaline <- function(xin, yd, eta, tol, maxepocas, par)
{
  dimxin <- dim(xin)
  N <- dimxin[1]
  n <- dimxin[2]
  
  if(par == 1){
    wt  <- as.matrix(runif(n+1) - 0.5)
    xin <- cbind(1,xin)
  }else wt <- as.matrix(runif(n) - 0.5)
  
  nepocas <- 0
  eepoca <- tol + 1
  
  evec<- matrix(nrow=1, ncol=maxepocas)
  while((nepocas < maxepocas) && (eepoca > tol)){
    ei2 <- 0
    
    xseq <- sample(N)
    for(i in 1: N){
      irand <- xseq[i]
      yhati <- 1.0 *((xin[irand,] %*% wt))
      ei <- yd[irand] - yhati
      dw <- eta* ei*xin[irand,]
      wt <- wt+ dw
      ei2 <- ei2+ ei*ei
      
    }
    nepocas <- nepocas + 1
    evec[nepocas] <- ei2/2
    
    eepoca <- evec[nepocas]
  }
  retlist <- list(wt, evec[1: nepocas])
  
  return( retlist)
  
}
data('wine')

x <- matrix(wine[,2:14])
y <- matrix(wine[,1])

y = ifelse(y == 1, 1, 2)

N <- dim(y)[2]
Ntrain <- 0.7*Ntrain 
Nteste <- N - 0.7*Ntrain

yTrain <- matrix(y[1:Ntrain,ncol=Ntrain])
xTrain <- matrix(x[1:Ntrain,ncol=Ntrain])

yTeste <- matrix(y[Ntrain:N,ncol=Nteste])
xTeste <- matrix(x[Ntrain:N,ncol=Nteste])

vetorAcuraciaTeste <- matrix(0,ncol=10)
vetorAcuraciaTrain <- matrix(0,ncol=10)
for (i in 1:10) {

retlist<- adaline(xTrain,yTrain,0.01,0.01,50,1)

w <- unlist(retlist[1])
erro <- retlist[2]

yhatteste <- xTeste %*% w
yhattrain <- xTrain %*% w

vetorAcuraciaTeste[i] <- 1 - (t(yhatteste -YTeste) %*% (yhatteste -YTeste)/Nteste)
vetorAcuraciaTrain[i] <- 1 - (t(yhattrain -yTrain) %*% (yhattrain -yTrain)/Ntrain)

}

print(mean(vetorAcuraciaTeste))
print(sd(vetorAcuraciaTeste))

print(mean(vetorAcuraciaTrain))
print(sd(vetorAcuraciaTrain))

