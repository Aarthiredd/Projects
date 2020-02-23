#given values for each attributes
Unit_cost<-80
carrying_cost_rate<-0.18
order_cost_perorder<-220

#simulating
r<-10^5

#random generation of triangular variable 
qtriangular<-function(p,a,b,c) { 
  QQ<-a+sqrt((b-a)*(c-a)*p) 
  qq<-b-sqrt((b-a)*(b-c)*(1-p)) 
  q<-ifelse(p<(c-a)/(b-a),QQ,qq) 
  return(q) 
}

set.seed(3333)
#generating results with given triangular probability condition
result_value<-round(qtriangular(runif(r),14000,17000,15000),2)
#histogram for the resulted observations
hist(result_value,freq=F,main="Relative Frequency Histogram of Simulated values")
lines(density(result_value),lwd=2,col="pink")

#optimizing the values
x1<-340
x2<-2*x1
optimising_result_value<-ceiling(result_value/x2)
order_cost<-optimising_result_value*order_cost_perorder
holding_costs<-Unit_cost*carrying_cost_rate*x1
Total_cost<-order_cost+holding_costs

inventory_level<-matrix(0,r,1)
order_quantity<-matrix(0,r,1)
total_min_cost<-matrix(0,r,1)
number_of_orders<-matrix(0,r,1)



for (i in 1:r)  {
  interval_select<-c(100,1000)
  total_cost_function = function(x1,result_value) result_value/(2*x1)*order_cost_perorder+Unit_cost*carrying_cost_rate*x1
  optimize_result_valuef<-optimise(f=total_cost_function,c(100,1000),result_value=result_value[i],
                 lower=min(interval_select),upper=max(interval_select),
                 maximum=FALSE,tol=.Machine$double.eps^0.5)
  
  
  inventory_level[i]<-round(optimize_result_valuef$minimum,0)
  order_quantity[i]<-2*inventory_level[i]
  total_min_cost[i]<-round(optimize_result_valuef$objective,2)
  number_of_orders[i]<-ceiling(result_value[i]/(2*optimize_result_valuef$minimum))
  
}

hist(inventory_level,freq=F)
lines(density(inventory_level),lwd=2,col="blue")
aa1<-round(mean(inventory_level),0)
sd(inventory_level)
summary(inventory_level)

#tabular frequency distribution of inventory levels
inventory_level_table<-as.data.frame(table(inventory_level))
inventory_level_table
#Find the index with the max frequency
inventory_level_maxindex<-which.max(inventory_level_table[,2])
inventory_level_maxindex
#Find the corresponding inventory level value
inventory_level_max<-inventory_level_table[inventory_level_maxindex,1]
inventory_level_max 

#Generating random triangular values between the min, max and with peak observed.
random_functiontest<-qtriangular(runif(N),min(inventory_level),max(inventory_level),Mode(inventory_level))
#Checing whether the simulated inventory levels fit a triangular distribution
qqplot(inventory_level,random_functiontest)
abline(0,1,lwd=2,col="yellow")


#Analysis of the minimized total costs that is to be observed
hist(total_min_cost,freq=F)
lines(density(total_min_cost),lwd=2,col="orange")
result_value_mincost<-mean(total_min_cost)
sd(total_min_cost)
summary(total_min_cost)


#tabular frequency distribution of inventory levels
total_min_cost_table<-as.data.frame(table(total_min_cost))
total_min_cost_table
#index with the max frequency
total_min_cost_maxindex<-which.max(total_min_cost_table[,2])
total_min_cost_maxindex
#corresponding inventory level value
total_min_cost_max<-total_min_cost_table[total_min_cost_maxindex,1]
total_min_cost_max



#Generating random triangular values between the min, max and with peak observed.
random_functiontest2<-qtriangular(runif(r),min(total_min_cost),max(total_min_cost),Mode(total_min_cost))
#checking if the simulated inventory levels fit a triangular distribution
qqplot(total_min_cost,random_functiontest2)
abline(0,1,lwd=2,col="pink")



#Analysis of the annual number of times order
hist(number_of_orders,freq=F)
lines(density(number_of_orders),lwd=2,col="darkred")
total_numberof_orders<-mean(number_of_orders)
sd(number_of_orders)
summary(number_of_orders)


#Summarizing all important variables in a table
final_table_representing<-data.frame("Avg Demand"=round(mean(result_value),0),"Avg Order Inventory "=aa1,
                "Avg Order Quantity"=round(mean(order_quantity),0),
                "Avg Minimum Cost"=round(result_value_mincost,2),"Avg Number of Orders"=round(total_numberof_orders,0))
#gives the output of the table
final_table_representing




