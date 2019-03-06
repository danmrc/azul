using Distributions
using Plots
using Interpolations
using Statistics
using Optim

d = Gamma(5,1)
w = range(0.01,stop=300,length=1000)

u(c) = log(c)

#### Finite life
r = 0.06
bet = 1/(1+r)
work_age = 65
s = 1
ret_age = 20

choice_ret = zeros(ret_age,length(w))
value_ret = zeros(ret_age,length(w))

value_ret[1,:] = u.(w)
choice_ret[1,:] = w

for j in 2:ret_age
    f = LinearInterpolation(w,value_ret[j-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        val(c) = -(u(c) + bet*f((1+r)*w[i] -c + s))
        otm = optimize(val,0,w[i])
        choice_ret[j,i] = Optim.minimizer(otm)
        value_ret[j,i] = -Optim.minimum(otm)
    end
    println("Iteration ", j)
end

value_trab = zeros(work_age,length(w))
choice_trab = zeros(work_age,length(w))

value_trab[1,:] = value_ret[ret_age,:]
choice_trab[1,:] = choice_ret[ret_age,:]

for k in 2:work_age
    f =  LinearInterpolation(w,value_trab[k-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        mm = rand(d,2000)
        val(c) = -(u(c) + bet*mean(f.((1+r)*w[i] -c .+ mm)))
        otm = optimize(val,0,w[i])
        choice_trab[k,i] = Optim.minimizer(otm)
        value_trab[k,i] = -Optim.minimum(otm)
    end
    println("Iteration ",k)
end

start = 2
wage = rand(d,65)

cons = zeros(85)
asset = zeros(85)

asset[85] = start

for k in 65:-1:1
    cons_foo =  LinearInterpolation(w,choice_trab[k,:], extrapolation_bc = Line())
    cons[k+20] = cons_foo(asset[k+20])
    asset[k+19] = (1+r)*asset[k+20] - cons[k+20] + wage[k]
end

for k in 20:-1:2
    cons_foo =  LinearInterpolation(w,choice_ret[k,:], extrapolation_bc = Line()) #also depends on the convergence of the previous program
    cons[k] = cons_foo(asset[k])
    asset[k-1] = (1+r)*asset[k] - cons[k] + s
end

cons[1] = asset[1]

rett = zeros(20)
fill!(rett,s)
income = [wage[65:-1:1]; rett]

plot(asset[85:-1:1], lab = "Assets", xlab = "Tempo",legend = :topleft)
plot!(cons[85:-1:1], lab = "Consumption")
plot!(income[1:85], lab = "Income")
vline!([66],lab = "Aposentadoria", lw = 2)

png("./content/post/prev/exemplo.png")

plot(w,(choice_trab[30,:] ./w),xlab = "Riqueza")
plot!(w,choice_trab[28,:]./w)
plot!(w,choice_trab[15,:]./w)

png("./content/post/prev/otm_pol1.png")

#### Case 2: s = 5 (mean)

s = 5

choice_ret2 = zeros(ret_age,length(w))
value_ret2 = zeros(ret_age,length(w))

value_ret2[1,:] = u.(w)
choice_ret2[1,:] = w

for j in 2:ret_age
    f = LinearInterpolation(w,value_ret2[j-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        val(c) = -(u(c) + bet*f((1+r)*w[i] -c + s))
        otm = optimize(val,0,w[i])
        choice_ret2[j,i] = Optim.minimizer(otm)
        value_ret2[j,i] = -Optim.minimum(otm)
    end
    println("Iteration ", j)
end

value_trab2 = zeros(work_age,length(w))
choice_trab2 = zeros(work_age,length(w))

value_trab2[1,:] = value_ret2[ret_age,:]
choice_trab2[1,:] = choice_ret2[ret_age,:]

for k in 2:work_age
    f =  LinearInterpolation(w,value_trab2[k-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        mm = rand(d,2000)
        val(c) = -(u(c) + bet*mean(f.((1+r)*w[i] -c .+ mm)))
        otm = optimize(val,0,w[i])
        choice_trab2[k,i] = Optim.minimizer(otm)
        value_trab2[k,i] = -Optim.minimum(otm)
    end
    println("Iteration ",k)
end

start = 2

cons2 = zeros(85)
asset2 = zeros(85)

asset2[85] = start

for k in 65:-1:1
    cons_foo =  LinearInterpolation(w,choice_trab2[k,:], extrapolation_bc = Line())
    cons2[k+20] = cons_foo(asset2[k+20])
    asset2[k+19] = (1+r)*asset2[k+20] - cons2[k+20] + wage[k]
end

for k in 20:-1:2
    cons_foo =  LinearInterpolation(w,choice_ret2[k,:], extrapolation_bc = Line()) #also depends on the convergence of the previous program
    cons2[k] = cons_foo(asset2[k])
    asset2[k-1] = (1+r)*asset2[k] - cons2[k] + s
end

cons2[1] = asset2[1]

rett2 = zeros(20)
fill!(rett2,s)
income2 = [wage[65:-1:1]; rett2]

plot(asset2[85:-1:1], lab = "Assets", xlab = "Tempo",legend = :topleft)
plot!(cons2[85:-1:1], lab = "Consumption")
plot!(income2[1:85], lab = "Income")
vline!([66],lab = "Aposentadoria", lw = 2)

plot(w,(choice_trab2[30,:] ./w),xlab = "Riqueza")
plot!(w,choice_trab2[28,:]./w)
plot!(w,choice_trab2[15,:]./w)

png("./content/post/prev/otm_pol2.png")

##### Case 3 s = 7

s = 7

choice_ret3 = zeros(ret_age,length(w))
value_ret3 = zeros(ret_age,length(w))

value_ret3[1,:] = u.(w)
choice_ret3[1,:] = w

for j in 2:ret_age
    f = LinearInterpolation(w,value_ret3[j-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        val(c) = -(u(c) + bet*f((1+r)*w[i] -c + s))
        otm = optimize(val,0,w[i])
        choice_ret3[j,i] = Optim.minimizer(otm)
        value_ret3[j,i] = -Optim.minimum(otm)
    end
    println("Iteration ", j)
end

value_trab3 = zeros(work_age,length(w))
choice_trab3 = zeros(work_age,length(w))

value_trab3[1,:] = value_ret3[ret_age,:]
choice_trab3[1,:] = choice_ret3[ret_age,:]

for k in 2:work_age
    f =  LinearInterpolation(w,value_trab3[k-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        mm = rand(d,2000)
        val(c) = -(u(c) + bet*mean(f.((1+r)*w[i] -c .+ mm)))
        otm = optimize(val,0,w[i])
        choice_trab3[k,i] = Optim.minimizer(otm)
        value_trab3[k,i] = -Optim.minimum(otm)
    end
    println("Iteration ",k)
end

start = 2

cons3 = zeros(85)
asset3 = zeros(85)

asset3[85] = start

for k in 65:-1:1
    cons_foo =  LinearInterpolation(w,choice_trab3[k,:], extrapolation_bc = Line())
    cons3[k+20] = cons_foo(asset3[k+20])
    asset3[k+19] = (1+r)*asset3[k+20] - cons3[k+20] + wage[k]
end

for k in 20:-1:2
    cons_foo =  LinearInterpolation(w,choice_ret3[k,:], extrapolation_bc = Line()) #also depends on the convergence of the previous program
    cons3[k] = cons_foo(asset3[k])
    asset3[k-1] = (1+r)*asset3[k] - cons3[k] + s
end

cons3[1] = asset3[1]

rett3 = zeros(20)
fill!(rett3,s)
income3 = [wage[65:-1:1]; rett3]

plot(asset3[85:-1:1], lab = "Assets", xlab = "Tempo",legend = :topleft)
plot!(cons3[85:-1:1], lab = "Consumption")
plot!(income3[1:85], lab = "Income")
vline!([66],lab = "Aposentadoria", lw = 2)

plot(w,(choice_trab3[30,:] ./w),xlab = "Riqueza")
plot!(w,choice_trab3[28,:]./w)
plot!(w,choice_trab3[15,:]./w)
plot!(w,choice_trab3[10,:]./w)

png("./content/post/prev/otm_pol3.png")

plot(w,choice_trab[10,:],label = "s=1",xlab = "Riqueza", legend = :topleft)
plot!(w,(choice_trab2[10,:]), label = "s=5")
plot!(w,(choice_trab3[10,:]), label = "s=7")

png("./content/post/prev/otm_pol_comparada.png")

plot(cons[85:-1:1], lab = "s=1", legend = :bottomright)
plot!(cons2[85:-1:1], lab = "s=5")
plot!(cons3[85:-1:1], lab = "s=7")
vline!([66], lab = "Apoesentadoria", lw = 2)

png("./content/post/prev/cons.png")

plot(asset[85:-1:1], lab = "s=1", xlab = "Tempo",legend = :topleft)
plot!(asset2[85:-1:1], lab = "s=5")
plot!(asset3[85:-1:1], lab = "s=7")
vline!([66], lab = "Apoesentadoria", lw =2 )

png("./content/post/prev/assets.png")

### Case 3.1 s= 7 but loose consumption limit when working

s = 7

choice_ret4 = zeros(ret_age,length(w))
value_ret4 = zeros(ret_age,length(w))

value_ret4[1,:] = u.(w)
choice_ret4[1,:] = w

for j in 2:ret_age
    f = LinearInterpolation(w,value_ret4[j-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        val(c) = -(u(c) + bet*f((1+r)*w[i] -c + s))
        otm = optimize(val,0,w[i])
        choice_ret4[j,i] = Optim.minimizer(otm)
        value_ret4[j,i] = -Optim.minimum(otm)
    end
    println("Iteration ", j)
end

value_trab4 = zeros(work_age,length(w))
choice_trab4 = zeros(work_age,length(w))

value_trab4[1,:] = value_ret4[ret_age,:]
choice_trab4[1,:] = choice_ret4[ret_age,:]

for k in 2:work_age
    f =  LinearInterpolation(w,value_trab4[k-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        mm = rand(d,2000)
        val(c) = -(u(c) + bet*mean(f.((1+r)*w[i] -c .+ mm)))
        otm = optimize(val,0,(w[i]*1.5))
        choice_trab4[k,i] = Optim.minimizer(otm)
        value_trab4[k,i] = -Optim.minimum(otm)
    end
    println("Iteration ",k)
end

start = 2

cons4 = zeros(85)
asset4 = zeros(85)

asset4[85] = start

for k in 65:-1:1
    cons_foo =  LinearInterpolation(w,choice_trab4[k,:], extrapolation_bc = Line())
    cons4[k+20] = cons_foo(asset4[k+20])
    asset4[k+19] = (1+r)*asset4[k+20] - cons4[k+20] + wage[k]
end

for k in 20:-1:2
    cons_foo =  LinearInterpolation(w,choice_ret4[k,:], extrapolation_bc = Line()) #also depends on the convergence of the previous program
    cons4[k] = cons_foo(asset4[k])
    asset4[k-1] = (1+r)*asset4[k] - cons4[k] + s
end

cons4[1] = asset4[1]

rett4 = zeros(20)
fill!(rett4,s)
income4 = [wage[65:-1:1]; rett4]

plot(asset4[85:-1:1], lab = "Assets", xlab = "Tempo",legend = :topleft)
plot!(cons4[85:-1:1], lab = "Consumption")
plot!(income4[1:85], lab = "Income")
vline!([66],lab = "Aposentadoria", lw = 2)

plot(cons4[85:-1:1])
plot!(cons3[85:-1:1])

plot(asset4[85:-1:1])
plot!(asset3[85:-1:1])

## Wage distribution increaing with age

inc = range(0,stop=3,length=65)
xx = 0:0.05:15

anim = @animate for i=1:65
    y = inc[i]
    dd=Normal(5+y,1)
    plot(xx,pdf.(dd,xx), lab = string("Normal ",5+y,",1"))
    vline!([mean(dd)], lab = string("Média ",mean(dd)))
end

gif(anim,"./content/post/prev/teste.gif",fps=3)

## Wage distribution now increases with age, s=1

inc = range(0,stop=3,length=65)
inc = inc[65:-1:1]

s = 1

choice_ret5 = zeros(ret_age,length(w))
value_ret5 = zeros(ret_age,length(w))

value_ret5[1,:] = u.(w)
choice_ret5[1,:] = w

for j in 2:ret_age
    f = LinearInterpolation(w,value_ret5[j-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
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
    y = 5+inc[k]
    dd=Normal(y,1)
    for i in 1:length(w)
        mm = rand(dd,2000)
        val(c) = -(u(c) + bet*mean(f.((1+r)*w[i] -c .+ mm)))
        otm = optimize(val,0,w[i])
        choice_trab5[k,i] = Optim.minimizer(otm)
        value_trab5[k,i] = -Optim.minimum(otm)
    end
    println("Iteration ",k)
end

start = 2

cons5 = zeros(85)
asset5 = zeros(85)

asset5[85] = start

wage5 = zeros(65)

for i in 65:-1:1
    y = 5+inc[i]
    dd=Normal(y,1)
    wage5[i] = rand(dd,1)[1]
end

plot(wage5)

for k in 65:-1:1
    cons_foo =  LinearInterpolation(w,choice_trab5[k,:], extrapolation_bc = Line())
    cons5[k+20] = cons_foo(asset5[k+20])
    asset5[k+19] = (1+r)*asset5[k+20] - cons5[k+20] + wage5[k]
end

for k in 20:-1:2
    cons_foo =  LinearInterpolation(w,choice_ret5[k,:], extrapolation_bc = Line()) #also depends on the convergence of the previous program
    cons5[k] = cons_foo(asset5[k])
    asset5[k-1] = (1+r)*asset5[k] - cons5[k] + s
end

cons5[1] = asset5[1]

rett5 = zeros(20)
fill!(rett5,s)
income5 = [wage5[65:-1:1]; rett5]

plot(asset5[85:-1:1], lab = "Assets", xlab = "Tempo",legend = :topleft)
plot!(cons5[85:-1:1], lab = "Consumption")
plot!(income5[1:85], lab = "Income")
vline!([66],lab = "Aposentadoria", lw = 2)

png("./content/post/prev/sal_cresc.png")

plot(cons5[85:-1:1], lab = "Consumption", legend = :topleft)
plot!(income5[1:85], lab = "Income")
plot!((5 .+inc[65:-1:1]), label = "Trend Salário",lw=2)
vline!([66],lab = "Aposentadoria",lw=2)

png("./content/post/prev/sal_cresc2.png")

plot(w,choice_trab5[1,:], legend = :topleft,lab = "t = 65")
plot!(w,choice_trab5[10,:],lab = "t = 55")
plot!(w,choice_trab5[45,:],lab = "t = 20")
plot!(w,choice_trab5[55,:],lab = "t = 10")

png("./content/post/prev/escolh_cresc_sal.png")

plot(w,choice_trab[40,:])
plot!(w,choice_trab5[40,:])


### Increasing wage s = 5

s = 5

choice_ret6 = zeros(ret_age,length(w))
value_ret6 = zeros(ret_age,length(w))

value_ret6[1,:] = u.(w)
choice_ret6[1,:] = w

for j in 2:ret_age
    f = LinearInterpolation(w,value_ret6[j-1,:], extrapolation_bc = Line())
    for i in 1:length(w)
        val(c) = -(u(c) + bet*f((1+r)*w[i] -c + s))
        otm = optimize(val,0,w[i])
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
    y = 5+inc[k]
    dd=Normal(y,1)
    for i in 1:length(w)
        mm = rand(dd,2000)
        val(c) = -(u(c) + bet*mean(f.((1+r)*w[i] -c .+ mm)))
        otm = optimize(val,0,w[i])
        choice_trab6[k,i] = Optim.minimizer(otm)
        value_trab6[k,i] = -Optim.minimum(otm)
    end
    println("Iteration ",k)
end

start = 2

cons6 = zeros(85)
asset6 = zeros(85)

asset6[85] = start

for k in 65:-1:1
    cons_foo =  LinearInterpolation(w,choice_trab6[k,:], extrapolation_bc = Line())
    cons6[k+20] = cons_foo(asset6[k+20])
    asset6[k+19] = (1+r)*asset6[k+20] - cons6[k+20] + wage5[k]
end

for k in 20:-1:2
    cons_foo =  LinearInterpolation(w,choice_ret6[k,:], extrapolation_bc = Line()) #also depends on the convergence of the previous program
    cons6[k] = cons_foo(asset6[k])
    asset6[k-1] = (1+r)*asset6[k] - cons6[k] + s
end

cons6[1] = asset6[1]

rett6 = zeros(20)
fill!(rett6,s)
income6 = [wage5[65:-1:1]; rett6]

plot(asset6[85:-1:1], lab = "Assets", xlab = "Tempo",legend = :topleft)
plot!(cons6[85:-1:1], lab = "Consumption")
plot!(income6[1:85], lab = "Income")
vline!([66],lab = "Aposentadoria", lw = 2)

png("./content/post/prev/sal_cresc.png")

plot(cons6[85:-1:1], lab = "Consumption", legend = :topleft)
plot!(income6[1:85], lab = "Income")
plot!((5 .+inc[65:-1:1]), label = "Trend Salário",lw=2)
vline!([66],lab = "Aposentadoria",lw=2)

png("./content/post/prev/sal_cresc2.png")

plot(cons5[85:-1:1], lab = "s=1", legend = :topleft)
plot!(cons6[85:-1:1], lab = "s=5")

png("./content/post/prev/cons2.png")

plot(asset5[85:-1:1], lab = "s=1", legend = :topleft)
plot!(asset6[85:-1:1], lab = "s=5")

png("./content/post/prev/asset2.png")
