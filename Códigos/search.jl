using Distributions
using Plots
using Interpolations
using Statistics

d = Gamma(5,1)
w = range(0,stop=16,length=300)
plot(w,pdf.(d,w),label = "Gama(5,1)")
ww = range(0,stop=25,length=500)
dd = Gamma(5,2)
plot!(ww,pdf.(dd,ww),label = "Gama(5,2)")
png("content/post/search/densidade")

bet = 0.9
cbar = 1

choice = zeros(3000,length(w))
value = zeros(3000,length(w))

value[1,:] = w/(1-bet)

for j in 2:3000
    f = LinearInterpolation(w,value[j-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        mm = rand(d,2000)
        emp = w[i]/(1-bet)
        unp = cbar + bet*mean(f.(mm))
        choo = max(emp,unp)
        if choo == emp
            choice[j,i] = 1
            value[j,i] = emp
        else
            value[j,i] = unp
        end
    end
end

plot(value[3000,:]-value[2999,:])
png("content/post/search/conv_value1")
plot(choice[3000,:]-choice[2999,:])
png("content/post/search/conv_pol1")

plot(w,choice[3000,:], lab="Escolha")
png("content/post/search/escolha1")

cbar = 5

choice2 = zeros(3000,length(w))
value2 = zeros(3000,length(w))

value2[1,:] = w/(1-bet)

for j in 2:3000
    f = LinearInterpolation(w,value2[j-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        mm = rand(d,2000)
        emp = w[i]/(1-bet)
        unp = cbar + bet*mean(f.(mm))
        choo = max(emp,unp)
        if choo == emp
            choice2[j,i] = 1
            value2[j,i] = emp
        else
            value2[j,i] = unp
        end
    end
end

plot(value2[3000,:]-value2[2999,:])
plot(choice2[3000,:]-choice2[2999,:])

plot(w,choice[3000,:],label = "cbar=1")
plot!(w,choice2[3000,:],label = "cbar=5")
png("content/post/search/escolha2")

#### Different Distribution now

cbar = 1

choice3 = zeros(3000,length(ww))
value3 = zeros(3000,length(ww))

value3[1,:] = ww/(1-bet)

for j in 2:3000
    f = LinearInterpolation(ww,value3[j-1,:], extrapolation_bc = Line())
    for i in 1:length(ww)
        mm = rand(dd,2000)
        emp = ww[i]/(1-bet)
        unp = cbar + bet*mean(f.(mm))
        choo = max(emp,unp)
        if choo == emp
            choice3[j,i] = 1
            value3[j,i] = emp
        else
            value3[j,i] = unp
        end
    end
end

plot(value3[3000,:]-value3[2999,:])
plot(choice3[3000,:]-choice3[2999,:])

plot(ww,choice3[3000,:])

cbar = 5

choice4 = zeros(3000,length(ww))
value4 = zeros(3000,length(ww))

value4[1,:] = ww/(1-bet)

for j in 2:3000
    f = LinearInterpolation(ww,value4[j-1,:], extrapolation_bc = Line())
    for i in 1:length(ww)
        mm = rand(dd,2000)
        emp = ww[i]/(1-bet)
        unp = cbar + bet*mean(f.(mm))
        choo = max(emp,unp)
        if choo == emp
            choice4[j,i] = 1
            value4[j,i] = emp
        else
            value4[j,i] = unp
        end
    end
end

plot(ww,choice3[3000,:],label = "cbar=1")
plot!(ww,choice4[3000,:],label = "cbar=5")
png("content/post/search/escolha3")

plot(w,choice[3000,:],label = "Gama(5,1)")
plot!(ww,choice3[3000,:],label = "Gama(5,2)")
png("content/post/search/escolha4")
