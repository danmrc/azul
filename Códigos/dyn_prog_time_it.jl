using Interpolations
using Plots
using Roots
using Distributions

### Primeiro: caso com produção, utilidade log e função de produção Cobb Douglas
## Esse caso tem solução analítica, que é implementada abaixo

beta = 0.95
alpha = 0.65

grid_size = 300

iter = 15

k_grid = range(1e-5,8,length=grid_size)

uline(c) = 1/c
f(k) = k^alpha
fline(k) = alpha*k^(alpha-1)

c_val = zeros(grid_size,iter)
c_val[:,1] =  f.(k_grid)

for i in 2:iter
    for j in 1:grid_size
        foo_c = LinearInterpolation(k_grid,c_val[:,i-1],extrapolation_bc = Line())
        function g(c)
            y = f(k_grid[j])
            return uline(c) - beta*fline(y-c)*uline(foo_c(y-c))
        end
        c_val[j,i] = find_zeros(g,1e-10,f(k_grid[j]))[1]
    end
end

maximum(abs.(c_val[:,14]-c_val[:,15]))

true_pol(k) = (1-alpha*beta)*f(k)
plot(k_grid,true_pol.(k_grid), lab = "Sol Analítica")
plot!(k_grid,c_val[:,15], lab = "Sol Numérica" ,line =:dash)

##### Caso em que a economia só tem endowments e esses endowments seguem um processo de Markov

beta = 0.96
r = 0.04
sigma = 2

bond_grid_size = 300
y_grid_size = 4

iter = 50

(yt_grid,yt_prob) = discretize_ar(0.901,y_grid_size,1,0.00219)
yt_grid = yt_grid .+15
bond_grid = range(1,10,length=bond_grid_size)

uline(c) = c^(-sigma)

c_val = zeros(bond_grid_size,y_grid_size,iter)
c_val[:,:,1] = [0.5*(b+y) for b in bond_grid,y in yt_grid]

global i=2
global err = 1

while i <= iter&&err > 1e-10
    for j in 1:y_grid_size
        foo_c = LinearInterpolation((bond_grid,yt_grid),c_val[:,:,i-1],extrapolation_bc = Line())
        for l in 1:bond_grid_size
            function f(c)
                uline(c) - beta*(1+r)*yt_prob[j,:]'*uline.(foo_c((1+r)*bond_grid[l]+yt_grid[j]-c,yt_grid))
            end
            c_val[l,j,i] = find_zeros(f,1e-10,(bond_grid[l]+yt_grid[j]+2))[1]
        end
    end
    err = maximum(abs.(c_val[:,:,i]-c_val[:,:,i-1]))
    println("iteration ",i-1, ",error ", err)
    global i = i+1
end

maximum(abs.(c_val[:,:,30]-c_val[:,:,29]))

plot(bond_grid,c_val[:,1,30])
plot!(bond_grid,c_val[:,1,29])

plot(bond_grid,c_val[:,1,30], label = string("y=",round(yt_grid[1],digits=3)))
plot!(bond_grid,bond_grid .+ yt_grid[1], label = "RO")

plot(bond_grid,c_val[:,2,30], label = string("y=",round(yt_grid[2],digits=3)))
plot!(bond_grid,bond_grid .+ yt_grid[2], label = "RO")

plot(bond_grid,c_val[:,3,30], label = string("y=",round(yt_grid[3],digits=3)))
plot!(bond_grid,bond_grid .+ yt_grid[3], label = "RO")

plot(bond_grid,c_val[:,4,30], label = string("y=",round(yt_grid[3],digits=3)))
plot!(bond_grid,bond_grid .+ yt_grid[4], label = "RO")

plot(bond_grid,c_val[:,1,30], label = string("y=",round(yt_grid[1],digits=3)))
plot!(bond_grid,c_val[:,2,30], label = string("y=",round(yt_grid[2],digits=3)))
plot!(bond_grid,c_val[:,3,30], label = string("y=",round(yt_grid[3],digits=3)))
plot!(bond_grid,c_val[:,4,30], label = string("y=",round(yt_grid[4],digits=3)))
