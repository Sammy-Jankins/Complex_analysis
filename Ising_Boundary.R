
N <- 200
M <- matrix(0, nrow = N, ncol=N)
# inverse temperatur beta = 1/(Boltzman constant * temperature), 
# critical value beta_C = 1/2*log(sqrt(2)+1)
beta <- 1/2*log(sqrt(2)+1)
flips <- 0


set.seed(2359)

for(k in 1:N) {
    for(l in 1:N) {
        p = runif(1, 0, 1);
        if(p<0.5)
            M[[k,l]] <- 1
        else
            M[[k,l]] <- -1 
    }
   
}

for(l in 1:N) {
    M[[1,l]] <- 1
    M[[N,l]] <- -1
}
for(k in 1:N) {
    if(k < N/2+1) {
         M[[k,1]] <- 1
         M[[k,N]] <- 1
    } else {
        M[[k,1]] <- -1
        M[[k,N]] <- -1 
    }
}

# for(k in 1:10) {
#         M[[N/2+k-50,1]] <- -1
#         M[[N/2+k-40,1]] <- 1
#         M[[N/2+k-30,1]] <- -1
#         M[[N/2+k-20,1]] <- 1
#         M[[N/2+k-10,1]] <- -1
#         M[[N/2+k,1]] <- 1
#         M[[N/2+k+10,1]] <- -1
#         M[[N/2+k+20,1]] <- 1
#         M[[N/2+k+30,1]] <- -1
#         M[[N/2+k+40,1]] <- 1
# }
# for(k in 1:20) {
#     M[[N/2+k-50,1]] <- -1
#     M[[N/2+k-30,1]] <- 1
#     M[[N/2+k-10,1]] <- -1
#     M[[N/2+k+10,1]] <- 1
#     M[[N/2+k+30,1]] <- -1
#     M[[N/2+k+50,1]] <- 1
# }
# 
# for(k in 1:2) {
#     M[[N/2+k-5,N]] <- -1
#     M[[N/2+k-3,N]] <- 1
#     M[[N/2+k-1,N]] <- -1
#     M[[N/2+k+1,N]] <- 1
#     M[[N/2+k+3,N]] <- -1
#     M[[N/2+k+5,N]] <- 1
# }

# Metropolis
for(i in 1:5000000) {
# choose random spin
k0 <- floor(runif(1, 2, N))
l0 <- floor(runif(1, 2, N))

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
greenX  <-vector()
greenY  <-vector()


xGreen <- N/2
yGreen <- 1
M[[xGreen,yGreen]] <- 2
z=1
GreensX <- vector()
GreensY <- vector()
GreensX <- c(GreensX, xGreen)
GreensY <- c(GreensY, yGreen)
indexGreen <- 1

while(yGreen < N || xGreen < N/2) {

    z <- z+1
    xNew <- 0
    xNew2 <- 0
    yNew <- 0
    yNew2 <- 0
    did <- 0
    
    if (xGreen < N && yGreen>1 && 
        M[[xGreen+1, yGreen]]==1 && M[[xGreen+1, yGreen-1]]==-1) {
        xNew <- xGreen + 1
        yNew <- yGreen
        did <- did+1
    }
    if (xGreen < N && yGreen>1 && M[[xGreen+1, yGreen]]==1 &&
        M[[xGreen+1, yGreen-1]]==1 && M[[xGreen, yGreen-1]]==-1) {
        did <- did+1
        M[[xGreen+1, yGreen-1]] <- 2
        M[[xGreen+1, yGreen]] <- 2
        GreensX <- c(GreensX, xGreen+1)
        GreensY <- c(GreensY, yGreen)
        xGreen <- xGreen +1
        yGreen <- yGreen -1
        GreensX <- c(GreensX, xGreen)
        GreensY <- c(GreensY, yGreen)
        indexGreen <- indexGreen+2
        next
    }
    if (xGreen > 1 && yGreen>1 && 
        M[[xGreen, yGreen-1]]==1 && M[[xGreen-1, yGreen-1]]==-1) {
        did <- did+1
        yNew <- yGreen - 1
        xNew <- xGreen
    }
    if (xGreen > 1 && yGreen>1 && M[[xGreen, yGreen-1]]==1 &&
        M[[xGreen-1, yGreen-1]]==1 && M[[xGreen-1, yGreen]]==-1) {
        did <- did+1
        M[[xGreen-1, yGreen-1]] <- 2
        M[[xGreen, yGreen-1]] <- 2
        GreensX <- c(GreensX, xGreen)
        GreensY <- c(GreensY, yGreen-1)
        xGreen <- xGreen -1
        yGreen <- yGreen -1
        GreensX <- c(GreensX, xGreen)
        GreensY <- c(GreensY, yGreen)
        indexGreen <- indexGreen+2
        next
    }
    if (xGreen > 1 && yGreen<N && 
        M[[xGreen-1, yGreen]]==1 && M[[xGreen-1, yGreen+1]]==-1) {
        did <- did+1
        xNew <- xGreen -1
        yNew <- yGreen
    }
    if (xGreen > 1 && yGreen<N &&  M[[xGreen-1, yGreen]]==1  &&
        M[[xGreen-1, yGreen+1]]==1 && M[[xGreen, yGreen+1]]==-1) {
        did <- did+1
        M[[xGreen-1, yGreen+1]] <- 2
        M[[xGreen-1, yGreen]] <- 2
        GreensX <- c(GreensX, xGreen-1)
        GreensY <- c(GreensY, yGreen)
        xGreen <- xGreen -1
        yGreen <- yGreen +1
        GreensX <- c(GreensX, xGreen)
        GreensY <- c(GreensY, yGreen)
        indexGreen <- indexGreen+2
        next
    }
    if (xGreen < N && yGreen<N && 
        M[[xGreen, yGreen+1]]==1 && M[[xGreen+1, yGreen+1]]==-1) {
        did <- did+1
        xNew <- xGreen
        yNew <- yGreen +1
    }
    if (xGreen < N && yGreen<N && M[[xGreen, yGreen+1]]==1 &&
        M[[xGreen+1, yGreen+1]]==1 && M[[xGreen+1, yGreen]]==-1) {
        did <- did+1
        M[[xGreen+1, yGreen+1]] <- 2
        M[[xGreen, yGreen+1]] <- 2 
        GreensX <- c(GreensX, xGreen)
        GreensY <- c(GreensY, yGreen+1)
        xGreen <- xGreen +1
        yGreen <- yGreen +1
        GreensX <- c(GreensX, xGreen)
        GreensY <- c(GreensY, yGreen)
        indexGreen <- indexGreen+2
        next
    }
    if (did==1) { 
        M[[xNew, yNew]] <-2
        xGreen <- xNew
        yGreen <- yNew
        GreensX <- c(GreensX, xGreen)
        GreensY <- c(GreensY, yGreen)
        indexGreen <- indexGreen+1
        next 
    }
    if (did == 0) {
        indexGreen <- indexGreen-1
        xGreen <- GreensX[indexGreen]
        yGreen <- GreensY[indexGreen]
        GreensX <- GreensX[1:indexGreen]
        GreensY <- GreensY[1:indexGreen]
        next
    }
    if (did == 3) {
        befX <- GreensX[indexGreen - 1]
        befY <- GreensY[indexGreen - 1]
        if (befX == xGreen) {
            xGreen <- xGreen - (befY - yGreen)
        } else {
            yGreen <- yGreen + (befX - xGreen)
        }
        M[[xGreen, yGreen]] <-2
        GreensX <- c(GreensX, xGreen)
        GreensY <- c(GreensY, yGreen)
        indexGreen <- indexGreen+1
        next
    }
    if (did == 2) {
        if (indexGreen == 1) {
            befX <- xGreen
            befY <- yGreen - 1
        } else {
            befX <- GreensX[indexGreen - 1]
            befY <- GreensY[indexGreen - 1]
        }
        if (befX == xGreen) {
            xGreenTemp <- xGreen - (befY - yGreen)
            yGreenTemp <- yGreen
        } else {
            yGreenTemp <- yGreen + (befX - xGreen)
            xGreenTemp <- xGreen
        }
        if (M[[xGreenTemp, yGreenTemp]]==1) {
            xGreen <- xGreenTemp
            yGreen <- yGreenTemp
            M[[xGreen, yGreen]] <-2
            GreensX <- c(GreensX, xGreen)
            GreensY <- c(GreensY, yGreen)
            indexGreen <- indexGreen+1
            next
        } else {
            xGreen <- xGreen + (xGreen - befX)
            yGreen <- yGreen + (yGreen - befY)
            M[[xGreen, yGreen]] <-2
            GreensX <- c(GreensX, xGreen)
            GreensY <- c(GreensY, yGreen)
            indexGreen <- indexGreen+1
        }
    }

}

print(indexGreen)

for(k in 1:N) {
    for(l in 1:N) {
        
        if(M[[k,l]]==1) {
            blueX <- c(blueX,k)
            blueY <- c(blueY,l)
        }
        else if (M[[k,l]]==-1) {
            redX <- c(redX,k)
            redY <- c(redY,l)
        } 
        else {
            greenX <- c(greenX,k)
            greenY <- c(greenY,l) 
        }
    }
}





plot(blueX,blueY,asp=1, cex=0.25,pty="s", pch=15,xlab="x",ylab="y",col="blue", xlim=c(-1,N+1),ylim=c(-1,N+1))
points(redX,redY,cex=0.25,pty="s", pch=15,xlab="x",ylab="y",col="red")
points(greenX,greenY,cex=0.25,pty="s", pch=15,xlab="x",ylab="y",col="green")

flips
