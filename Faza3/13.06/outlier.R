out<-clean.data

scaled.out <- scale(out[,-c(1,4,19)])
out2 <- out[,-c(1,4)]
str(out2)
out2<-data.frame(out2)
out2[is.nan(out2)] <- 0

out2 <- as.data.frame(out2)
dim <- dim(out2)
y <- unlist(out2)
y[ is.nan(y) ] <- 0
x <- matrix( y , dim )

out2<-x

mean.point <- sapply(1:ncol(scaled.out), function(x) mean(scaled.out[,x]))
mean.point <- sapply(1:ncol(out2), function(x) mean(out2[,x]))

dists <- sapply(1:nrow(out2), function(x) {
                                    sum(abs(out2[x,] - mean.point))})
plot(dists)
clean.data<-out
sorted.dists <- sort(dists, index.return=TRUE, decreasing=TRUE)
sorted.data <- clean.data[sorted.dists$ix,]

View(clean.data[sorted.dists$ix[1:30],])
