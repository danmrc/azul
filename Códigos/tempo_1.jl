######Profiling do Julia
##### Por Daniel Coutinho
######

### 1 - MQO

using Statistics

ols(x,y) = inv(x'*x)*x'*y

function func()
    X = randn(100,5)
    bet = [1.,2,3,4,5]
    y = X*bet + randn(100)
    ols(X,y)
end

tempo1 = Array{Float64}(undef,100)

for j=1:100
    tempo1[j] = @elapsed func()
end

temp = Statistics.median(tempo1)

### 2 - Otimização

using Optim
using Distributions

weib(par) = -1*sum(log(par[2])-log(par[1]) .+ (par[2]-1) .* (log.(x) .- log(par[1])) .- (x./par[1]) .^par[2])

g = Weibull(1,1)


lower = [0 0.]
upper = [Inf Inf]
x0 = [2 2.]

function func2()
    x = rand(g,500)
    optimize(weib,lower,upper,x0,Fminbox(LBFGS()))
end

tempo2 = Array{Float64}(undef,100)

for i=1:100
    tempo2[i] = @elapsed optimize(weib,lower,upper,x0,Fminbox(LBFGS()))
end

temp = Statistics.median(tempo2)

##3 - Bootstrap

function boot()

    z = randn(100)
    boot_mean = Array{Float64}(undef,10000)

    for i = 1:10000

        prob = repeat([1/length(z)],length(z))

        h = Categorical(prob)

        boot_mean[i] = Statistics.mean(z[rand(h,500)])
    end
end

tempo = Array{Float64}(undef,100)

for j=1:100
    tempo[j]=  @elapsed boot()
end

temp3 = Statistics.median(tempo)
