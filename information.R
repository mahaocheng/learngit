
#计算信息熵函数
H_calculate<-function(x)
{
    n<-length(x)
	H=0
	for(i in 1:n)
	{H<-H-x[i]*log2(x[i])}
	H	
}

x<-rep(1/8,8)
x1<-rep(0.2,5)
x2<-c(0.1,0.5,0.03,0.2,0.17)
H_calculate(x)
x<-c(0.5,0.5)
x<-c(0.5,0.25,0.25)
data<-c(14423,1769,22732,9918)
sum_data<-sum(data)
x1<-c((data[1]+data[3])/sum_data,(data[2]+data[4])/sum_data)
x2<-c((data[1]+data[2])/sum_data,(data[3]+data[4])/sum_data)
x3<-c(data[1]/(sum(data[1:2])),data[2]/sum(data[1:2]))
x4<-c(data[3]/(sum(data[3:4])),data[4]/sum(data[3:4]))
x2[1]*H_calculate(x3)+x2[2]*H_calculate(x4)

fx<-c(0,0.3,0.6,0,0)
expfx<-exp(fx)/sum(exp(fx))

