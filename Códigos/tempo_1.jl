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
