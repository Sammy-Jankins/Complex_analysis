
N <- 200
M <- matrix(0, nrow = N, ncol=N)
# inverse temperatur beta = 1/(Boltzman constant * temperature), 
# critical value beta_C = 1/2*log(sqrt(2)+1)
beta <- 100
flips <- 0
HMagnet <- 1

set.seed(2327)

for(k in 1:N) {
    for(l in 1:N) {
        p = runif(1, 0, 1);
        if(p<0.5)
            M[[k,l]] <- 1
        else
            M[[k,l]] <- -1 
    }
   
}

# Metropolis
for(i in 1:1000000) {
# choose random spin
k0 <- floor(runif(1, 1, N+1))
l0 <- floor(runif(1, 1, N+1))

#  flip it           
MNew <-  M[[k0,l0]] * -1        
# energy difference  H = new - old
H <- 0
if(k0>1) {
    H <- H + -1 * M[[k0-1,l0]] * MNew  - (-1 * M[[k0-1,l0]] * M[[k0,l0]])  
}
if(k0<N) {
    H <- H + -1 * M[[k0+1,l0]] * MNew  - (-1 * M[[k0+1,l0]] * M[[k0,l0]])  
}
if(l0>1) {
    H <- H + -1 * M[[k0,l0-1]] * MNew  - (-1 * M[[k0,l0-1]] * M[[k0,l0]])  
}
if(l0<N) {
    H <- H + -1 * M[[k0,l0+1]] * MNew  - (-1 * M[[k0,l0+1]] * M[[k0,l0]])  
}
H <- H + (-HMagnet * (MNew - M[[k0,l0]]))


if(H <= 0) {
    M[[k0, l0]] <- MNew
    flips <- flips +1
} else {
    p <- runif(1,0,1)
    if(p  < exp(-beta * H)) {
        M[[k0, l0]] <- MNew
        flips <- flips +1
    }
}

}

# Plot 

blueX <-vector()
blueY <-vector()
redX  <-vector()
redY  <-vector()

for(k in 1:N) {
    for(l in 1:N) {
        
        if(M[[k,l]]==1) {
            blueX <- c(blueX,k)
            blueY <- c(blueY,l)
        }
        else {
            redX <- c(redX,k)
            redY <- c(redY,l)
        } 
    }
}

plot(blueX,blueY,asp=1, cex=0.25,pty="s", pch=15,xlab="x",ylab="y",col="blue", xlim=c(-1,N+1),ylim=c(-1,N+1))
points(redX,redY,cex=0.25,pty="s", pch=15,xlab="x",ylab="y",col="red")

flips
