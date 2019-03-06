#### s=1 but living 40 years in retirement

## Without borrowing

s = 7
ret_age = 40
work_age = 45

choice_ret5 = zeros(ret_age,length(w))
value_ret5 = zeros(ret_age,length(w))

value_ret5[1,:] = u.(w)
choice_ret5[1,:] = w

for j in 2:ret_age
    f = LinearInterpolation(w,value_ret5[j-1,:], extrapolation_bc = Line())
    for i in 1:length(ww)
        val(c) = -(u(c) + bet*f((1+r)*w[i] -c + s))
        otm = optimize(val,0,w[i])
        choice_ret5[j,i] = Optim.minimizer(otm)
        value_ret5[j,i] = -Optim.minimum(otm)
    end
    println("Iteration ", j)
end

value_trab5 = zeros(work_age,length(w))
choice_trab5 = zeros(work_age,length(w))

value_trab5[1,:] = value_ret5[ret_age,:]
choice_trab5[1,:] = choice_ret5[ret_age,:]

for k in 2:work_age
    f =  LinearInterpolation(w,value_trab5[k-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        mm = rand(d,2000)
        val(c) = -(u(c) + bet*mean(f.((1+r)*w[i] -c .+ mm)))
        otm = optimize(val,0,(w[i]))
        choice_trab5[k,i] = Optim.minimizer(otm)
        value_trab5[k,i] = -Optim.minimum(otm)
    end
    println("Iteration ",k)
end

start = 2

cons5 = zeros(85)
asset5 = zeros(85)

asset5[85] = start

for k in 45:-1:1
    cons_foo =  LinearInterpolation(w,choice_trab5[k,:], extrapolation_bc = Line())
    cons5[k+40] = cons_foo(asset5[k+40])
    asset5[k+39] = (1+r)*asset5[k+40] + wage[k] - cons5[k+40]
end

for k in 40:-1:2
    cons_foo =  LinearInterpolation(w,choice_ret5[k,:], extrapolation_bc = Line()) #also depends on the convergence of the previous program
    cons5[k] = cons_foo(asset5[k])
    asset5[k-1] = (1+r)*asset5[k] - cons5[k] + s
end

cons5[1] = asset5[1]

rett5 = zeros(40)
fill!(rett5,s)
income5 = [wage[45:-1:1]; rett5]

plot(asset5[85:-1:1], lab = "Assets", xlab = "Tempo",legend = :topleft)
plot!(cons5[85:-1:1], lab = "Consumption")
plot!(income5[1:85], lab = "Income")
vline!([46],lab = "Aposentadoria", lw = 2)

## With Borrowing

w = range(-25,stop=25,length=500)

s = 7
ret_age = 40
work_age = 45

choice_ret6 = zeros(ret_age,length(w))
value_ret6 = zeros(ret_age,length(w))

aux = zeros(length(w))
for i in 1:length(w)
    if w[i] < 0
        aux[i] = -Inf
    else
        aux[i] = u(w[i])
    end
end

value_ret6[1,:] = aux
choice_ret6[1,:] = w

for j in 2:ret_age
    f = LinearInterpolation(w,value_ret6[j-1,:], extrapolation_bc = Line())
    for i in 1:length(ww)
        val(c) = -(u(c) + bet*f((1+r)*w[i] -c + s))
        otm = optimize(val,0,(w[i]+26))
        choice_ret6[j,i] = Optim.minimizer(otm)
        value_ret6[j,i] = -Optim.minimum(otm)
    end
    println("Iteration ", j)
end

value_trab6 = zeros(work_age,length(w))
choice_trab6 = zeros(work_age,length(w))

value_trab6[1,:] = value_ret6[ret_age,:]
choice_trab6[1,:] = choice_ret6[ret_age,:]

for k in 2:work_age
    f =  LinearInterpolation(w,value_trab6[k-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        mm = rand(d,2000)
        val(c) = -(u(c) + bet*mean(f.((1+r)*w[i] -c .+ mm)))
        otm = optimize(val,0,(w[i]+26))
        choice_trab6[k,i] = Optim.minimizer(otm)
        value_trab6[k,i] = -Optim.minimum(otm)
    end
    println("Iteration ",k)
end

start = 2

cons6 = zeros(85)
asset6 = zeros(85)

asset6[85] = start

for k in 45:-1:1
    cons_foo =  LinearInterpolation(w,choice_trab6[k,:], extrapolation_bc = Line())
    cons6[k+40] = cons_foo(asset6[k+40])
    asset6[k+39] = (1+r)*asset6[k+40] + wage[k] - cons6[k+40]
end

for k in 40:-1:2
    cons_foo =  LinearInterpolation(w,choice_ret6[k,:], extrapolation_bc = Line()) #also depends on the convergence of the previous program
    cons6[k] = cons_foo(asset6[k])
    asset6[k-1] = (1+r)*asset6[k] - cons6[k] + s
end

cons6[1] = asset6[1]

rett6 = zeros(40)
fill!(rett6,s)
income6 = [wage[45:-1:1]; rett6]

plot(asset6[85:-1:1], lab = "Assets", xlab = "Tempo",legend = :topleft)
plot!(cons6[85:-1:1], lab = "Consumption")
plot!(income6[1:85], lab = "Income")
vline!([46],lab = "Aposentadoria", lw = 2)

#######################################
########### Infinite life #############
#######################################

choice_ret = zeros(1000,length(w))
value_ret = zeros(1000,length(w))

value_ret[1,:] = u.(w)

global j = 2
global err = 1

while j <= 1000 && err >= eps()*10^5
    f = LinearInterpolation(w,value_ret[j-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        val(c) = -(u(c) + bet*prob*mean(f.((1+r)*w[i] -c + s)))
        otm = optimize(val,0,w[i])
        choice_ret[j,i] = Optim.minimizer(otm)
        value_ret[j,i] = -Optim.minimum(otm)
    end
    global err = maximum(abs.(value_ret[j-1,:] - value_ret[j,:]))
    println("Iteration ",j, " w/ error ",err )
    global j = j + 1
end

value_trab = zeros(work_age,length(w))
choice_trab = zeros(work_age,length(w))

value_trab[1,:] = value_ret[44,:] #Depends on the convergence of the previous program

for k in 2:work_age
    f =  LinearInterpolation(w,value_trab[k-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        mm = rand(d,2000)
        val(c) = -(u(c) + bet*mean(f.((1+r)*w[i] -c .+ mm)))
        otm = optimize(val,0,w[i])
        choice_trab[k,i] = Optim.minimizer(otm)
        value_trab[k,i] = -Optim.minimum(otm)
    end
end

start = 2
wage = rand(d,65)

cons = zeros(85)
asset = zeros(85)

asset[85] = start

for k in 65:-1:2
    cons_foo =  LinearInterpolation(w,choice_trab[k,:], extrapolation_bc = Line())
    cons[k+20] = cons_foo(asset[k+20])
    asset[k+19] = (1+r)*asset[k+20] - cons[k+20] + wage[k]
end

for k in 21:-1:2
    cons_foo =  LinearInterpolation(w,choice_ret[44,:], extrapolation_bc = Line()) #also depends on the convergence of the previous program
    cons[k] = cons_foo(asset[k])
    asset[k-1] = (1+r)*asset[k] - cons[k] + s
end

rett = zeros(20)
fill!(rett,s)
income = [wage[65:-1:1]; rett]

plot(asset[85:-1:1], lab = "assets")
plot!(cons[85:-1:1], lab = "Consumption")
plot!(income[1:85], lab = "Income")
