
# serial distribution
x=rweibull(1000,2.101547305,11.29064083)
plot(density(x))

# delay distribution
y=rweibull(1000,1.347738368,4.360796301)
plot(density(y))

z=rnbinom(5000,size=0.16,mu=2)
plot(density(z))
