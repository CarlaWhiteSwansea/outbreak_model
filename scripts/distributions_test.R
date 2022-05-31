
# serial distribution, dist_shape, dist_scale
x=rweibull(1000,2.101547305,11.29064083)
plot(density(x))

# delay distribution, delay_shape, delay_scale
y=rweibull(1000,1.347738368,4.360796301)
plot(density(y))

# distribution for selecting new cases from isolated infectors
# size=disp.iso, mu=r0isolated
z1=rnbinom(5000,size=0.9,mu=1.2)
plot(density(z1))

# distribution for selecting new cases from non-isolated infectors
# size=disp.com, mu=r0community
z2=rnbinom(5000,size=0.16,mu=0.2)
plot(density(z2))
