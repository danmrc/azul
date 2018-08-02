using Optim
using Dierckx
using Plots

T = 100
alpha = 0.5
bet = 0.98
delt = 0.1
dens = 100

u(c) = log(c)
f(k) = k^alpha
K = linspace(0.1,5,dens)

C = Array{Float64}(T,length(K))
V = Array{Float64}(T,length(K))

V[1,1:length(K)] = u.(K)
C[1,1:length(K)] = K

for j = 2:T
    valor=Spline1D(K,V[(j-1),1:length(K)],k=1,bc="extrapolate")
    for i = 1:length(K)
        val(c)=-u(c)-bet*valor((1-delt)*K[i]+f(K[i])-c)
        otimo = optimize(val,0.1,K[i])
        V[j,i] = -Optim.minimum(otimo)
        C[j,i] = Optim.minimizer(otimo)
    end
end

start_val = 2

C_path = Array{Float64}(1,T)
K_path = Array{Float64}(1,T)
K_path[T] = start_val

for j = T:-1:2
    func_cons = Spline1D(K,C[j,1:length(K)],k=1,bc = "extrapolate")
    C_path[j] = func_cons(K_path[j])
    K_path[j-1] = f(K_path[j]) - func_cons(K_path[j]) + (1-delt)*K_path[j]
end

C_path[1] = K_path[1]

plot(C_path[T:-1:1])
plot!(K_path[T:-1:1])
