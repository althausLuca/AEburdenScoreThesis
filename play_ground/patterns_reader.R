

n_sim <- 100000

x <- rexp(100000, 1/10)


for(i in 1:100000){
    x[i] <- rexp(1, 1/10)
    x[i] <- x[i]/2
}

control <- c(1,2,3,4,4)
treatment <- c(1,2,3,4,4)
df <- as.data.frame(rbind(control, treatment), row.names = c("control" , "treatment"))
