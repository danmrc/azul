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

function teste_optim()

    valores = Array{Float64}(undef,length(vals),2)

    for j = 1:length(vals)

        f(x) = x[1].^2 .+ x[2].^2

        x0 = [vals[j],vals[j]]

        otimo = optimize(f,x0,NelderMead())
        valores[j,:] = Optim.minimizer(otimo)
    end
    return(valores)
end

val, t, bytes, gctime, memallocs = @timed teste_optim()
