
a = 0
b = 2
n = 4

delta.x = ((b-a)/n)

f = function(x) {
	y = exp(x^2)
	return(y)
}

midpoint = function() {
	
	#defining subinterval vector
	#and filling vector with appropriate dx values
	subintervals = seq(a, b, by = delta.x) 

	#creating low and high endpoint vectors
	sub.low = subintervals[1:length(subintervals)-1]
	sub.high = subintervals[2:length(subintervals)]
	
	#combinging low and high endpoint vectors
	sub = cbind(sub.low, sub.high)

	#creates vector xi.star and fills it with n 0's
	xi.star = rep(0,n)
	
	#loops to fill xi.star with midpoints 
	for(i in 1:n) {
		xi.star[i] = (sub[i,1] + sub[i,2])/2
	}
	
	xi.star

	f.x = f(xi.star)

	Approx.int = sum(f.x)*delta.x

	return(Approx.int)
}

trapezoid = function() {
	
	#defining subinterval vector
	#and filling vector with appropriate dx values

	subintervals = seq(a, b, by = delta.x) 

	f.x = f(subintervals)

	midsum = sum(f.x[2:n])*2

	total.sum = f.x[1] + midsum + f.x[n+1]

	Approx.int = (delta.x/2)*total.sum

	return(Approx.int)
}

print("using midpoint method")
midpoint()
print("using trapezoid method")
trapezoid()
print("using the built in integrate function :)")
integrate(f, lower = 0, upper = 2)

