######Profiling do Julia
##### Por Daniel Coutinho
######

### 1 - MQO

ols(x,y) = inv(x'*x)*x'*y

function func()

    for i = 1:1000

        X = randn(100,5)
        bet = [1.,2,3,4,5]
        y = X*bet + randn(100)
        ols(X,y)
    end
end

tempo = Array{Float64}(undef,100)

for j=1:100

    val, t, bytes, gctime, memallocs =  @timed func()
    tempo[j] = t
end

sum(tempo)/length(tempo)

### 2 - Otimização

using Optim

vals = 1:0.01:10

valores = Array{Float64}(undef,length(vals),1)
x0=[0 0.]

for j = 1:length(vals)
        f(x) = (x[1]-vals[j]).^2 .+ x[2].^2

        val, t, bytes, gctime, memallocs = @timed optimize(f,x0,NelderMead())
        println(val)
        valores[j] = t
    end

sum(valores)/length(valores)

##3 - Bootstrap

using Distributions
using Statistics


function boot(amostra)

    boot_mean = Array{Float64}(undef,10000)

    for i = 1:10000

        prob = repeat([1/length(amostra)],length(amostra))

        h = Categorical(prob)

        boot_mean[i] = Statistics.mean(amostra[rand(h,500)])
    end
end

tempo = Array{Float64}(undef,100)

for j=1:100
    z = randn(100)
    val, t, bytes, gctime, memallocs =  @timed boot(z)
    tempo[j] = t
end

sum(tempo)/length(tempo)
