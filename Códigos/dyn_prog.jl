using Optim
using Interpolations
using Plots

T = 100
alpha = 0.5
bet = 0.98
delt = 1
dens = 100

u(c) = log(c)
f(k) = k^alpha
K = range(0.1,stop = 5,length = dens)

C = Array{Float64}(undef,T,length(K))
V = Array{Float64}(undef,T,length(K))

V[1,1:length(K)] = u.(K)
C[1,1:length(K)] = K

for j = 2:T
    valor=LinearInterpolation(K,V[(j-1),1:length(K)],extrapolation_bc= Interpolations.Linear())
    for i = 1:length(K)
        val(c)=-u(c)-bet*valor((1-delt)*K[i]+f(K[i])-c)
        otimo = optimize(val,0.1,K[i])
        V[j,i] = -Optim.minimum(otimo)
        C[j,i] = Optim.minimizer(otimo)
    end
end

start_val = 10

C_path = Array{Float64}(undef,T)
K_path = Array{Float64}(undef,T)
K_path[T] = start_val
K_true = Array{Float64}(undef,T)
K_true[T]=start_val

for j = T:-1:2
    func_cons = LinearInterpolation(K,C[j,1:length(K)], extrapolation_bc = Interpolations.Linear())
    C_path[j] = func_cons(K_path[j])
    K_path[j-1] = f(K_path[j]) - func_cons(K_path[j]) + (1-delt)*K_path[j]
    K_true[j-1] = alpha*bet*(1 - (alpha*bet)^(T-(T-j)))/(1-*(alpha*bet)^(T-(T-j-1)))*f(K_true[j])
end

C_path[1] = K_path[1]

plot(C_path[T:-1:1], lab = "Consumo")
plot!(K_path[T:-1:1], lab = "Trajetória estimada do Capital")
plot!(K_true[T:-1:1], lab = "Trajetória verdadeira do Capital")

png("imagem1")

err = K_true[T:-1:1] - K_path[T:-1:1]

plot(err, lab = "Erro na trajetória estimada")

png("imagem2")

T = 100
alpha = 0.5
bet = 0.98
delt = 0.3
dens = 100

u(c) = log(c)
f(k) = k^alpha
K = range(0.1,stop = 5,length = dens)

C = Array{Float64}(undef,T,length(K))
V = Array{Float64}(undef,T,length(K))

V[1,1:length(K)] = u.(K)
C[1,1:length(K)] = K

for j = 2:T
    valor=LinearInterpolation(K,V[(j-1),1:length(K)],extrapolation_bc= Interpolations.Linear())
    for i = 1:length(K)
        val(c)=-u(c)-bet*valor((1-delt)*K[i]+f(K[i])-c)
        otimo = optimize(val,0.1,K[i])
        V[j,i] = -Optim.minimum(otimo)
        C[j,i] = Optim.minimizer(otimo)
    end
end

start_val = 2

C_path = Array{Float64}(undef,T)
K_path = Array{Float64}(undef,T)
K_path[T] = start_val

for j = T:-1:2
    func_cons = LinearInterpolation(K,C[j,1:length(K)], extrapolation_bc = Interpolations.Linear())
    C_path[j] = func_cons(K_path[j])
    K_path[j-1] = f(K_path[j]) - func_cons(K_path[j]) + (1-delt)*K_path[j]
end

C_path[1] = K_path[1]

plot(C_path[T:-1:1], lab = "Consumo")
plot!(K_path[T:-1:1], lab = "Trajetória estimada do Capital")

png("imagem3")
