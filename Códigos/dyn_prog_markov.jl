using Plots
using Optim
using Interpolations
using Distributions

Transition_matrix = [[0.5 0.5];[0.4 0.6]]

w = [1 ;2]

u(c) = log(c)

T = 70

r = 0.05
beta = 1/(1+r)

grid = range(0,stop=10,step=0.05)

V = Array{Float64}(undef,T,length(grid),2)
P = Array{Float64}(undef,T,length(grid),2)

V[1,:,1] = u.(grid)
V[1,:,2] = u.(grid)

P[1,:,1] = grid
P[1,:,2] = grid

for i=1:(T-1)
    for k=1:2
        v_func = LinearInterpolation(grid,V[i,:,k], extrapolation_bc = Line())
        for j = 1:length(grid)
            func(c) = -u(c) - beta*Transition_matrix[k,:]'*v_func.(w .+(1+r)*(grid[j]-c))
            otimo = optimize(func,0,grid[j])
            V[i+1,j,k] = -Optim.minimum(otimo)
            P[i+1,j,k] = Optim.minimizer(otimo)
        end
    end
end

start_val = 2
initial_state = 1

cons = Array{Float64}(undef,T)
riqueza = Array{Float64}(undef,T)
riqueza[T] = start_val
state = Array{Int64}(undef,T)
state[1] = initial_state

dist = Uniform(0,1)

for j = 1:(T-1)
    aux = state[j]
    vv = rand(dist,1)
    if(vv <= Transition_matrix[aux,:])
        state[j+1] = 1
    else state[j+1] = 2 end
end

state = state[T:-1:1]

for j = T:-1:2
    cons_foo = LinearInterpolation(grid,P[j,:,state[j]],extrapolation_bc = Line())
    cons[j] = cons_foo(riqueza[j]+w[state[j]])
    riqueza[j-1] = (1+r)*(riqueza[j] + w[state[j]] - cons[j])
end

cons[1] = riqueza[1]

plot(riqueza[T:-1:1], lab = "Riqueza")
plot!(cons[T:-1:1], lab = "Consumo")
scatter!(state, lab = "Estado")
