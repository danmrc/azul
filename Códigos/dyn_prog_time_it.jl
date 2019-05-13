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

k_grid = range(1e-5,8,length=grid_size) #grid para a variável de estado

uline(c) = 1/c
f(k) = k^alpha
fline(k) = alpha*k^(alpha-1)

c_val = zeros(grid_size,iter)
c_val[:,1] =  f.(k_grid)

for i in 2:iter
    for j in 1:grid_size
        foo_c = LinearInterpolation(k_grid,c_val[:,i-1],extrapolation_bc = Line()) #usando uma aproximação linear (por partes) para a função do consumo
        function g(c)
            y = f(k_grid[j])
            return uline(c) - beta*fline(y-c)*uline(foo_c(y-c))
        end
        c_val[j,i] = find_zeros(g,1e-10,f(k_grid[j]))[1] #resolvendo a equação de euler
    end
end

maximum(abs.(c_val[:,14]-c_val[:,15]))

true_pol(k) = (1-alpha*beta)*f(k)
plot(k_grid,true_pol.(k_grid), lab = "Sol Analítica",w=2)
plot!(k_grid,c_val[:,15], lab = "Sol Numérica" ,line =:dash,w=2)

#### Só para comparar, vamos ver o mesmo caso com iteração na função valor e só 15 iterações

using Optim

u(c)=log(c)

y = f.(k_grid)

bet = beta
alf = alpha

guess = y
vals = Array{Float64}(undef,150,length(y))
pol=Array{Float64}(undef,150,length(y))

vals[1,1:length(y)] = guess
pol[1,1:length(y)] = y

for i=1:15
  V=LinearInterpolation(y,vals[i,:],extrapolation_bc = Interpolations.Line())
    for j = 1:length(y)
        That(c)=-(u(c)+bet*V(f(y[j]-c)))
        op = optimize(That,0,y[j])
       vals[(i+1),j]=-Optim.minimum(op)
       pol[(i+1),j]=Optim.minimizer(op)
   end
end

plot(f.(k_grid),true_pol.(k_grid), lab = "Sol Analítica", legend = :topleft)
plot!(f.(k_grid),c_val[:,7], lab = "Sol Numérica Time Iteration \n" ,line =:dash)
plot!(f.(k_grid),pol[7,:], lab = "Sol Numérica Value Iteration" ,line =:dash)

plot(f.(k_grid),c_val[:,15] - true_pol.(k_grid), lab = "Erro Time Iteration")
plot!(f.(k_grid),pol[15,:] - true_pol.(k_grid), lab = "Erro Value Iteration")

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

# Caso em que temos uma restrição de endividamento
#Eu implemento o caso em que a dotação segue um processo i.i.d. discreto

#Parametros

beta = 0.96
r = 0.04
sigma = 2
kappa = 0.32
alpha = 0.5

bond_grid_size = 200
y_grid_size = length(2:5)
x_grid_size = length(2:5)

y_grid = collect(2:5)
x_grid = collect(2:5)
dist = DiscreteUniform(2,5)
prob_y = pdf.(dist,y_grid)
prob_x = pdf.(dist,x_grid)

endow_aux = reshape(collect(Iterators.product(x_grid,y_grid)),16,1)
endow = zeros(16,2)

for i in 1:16
    endow[i,1] = endow_aux[i][1]
    endow[i,2] = endow_aux[i][2]
end

prob = repeat([prob_x[1]*prob_y[1]],16)

uline(c) = alpha/c
p(c,y) = (1-alpha)/alpha*c/y

iter = 50
bond_grid = range(-1,2,length=bond_grid_size)

cons = zeros(iter,bond_grid_size,x_grid_size,y_grid_size)
cons[1,:,:,:] = [bonds + xs for bonds in bond_grid, xs in x_grid,ys in y_grid] #consome tudo o que tem hoje

py = zeros(iter,bond_grid_size,x_grid_size,y_grid_size)
py[1,:,:,:] .= 1

i=2
err = 1

while i <=iter&&err>1e-9 #enquanto a maior mudança na política formaior que 1e-9 o algoritmo. Ele para de qualquer forma se alcançar o número máximo de iterações
    foo_c = LinearInterpolation((bond_grid,y_grid,x_grid),cons[i-1,:,:,:],extrapolation_bc = Flat()) #interpolando a função consumo
    for k in 1:16
        index_x = findfirst(x_grid .== endow[k,1])
        index_y = findfirst(y_grid .== endow[k,2])
        for j in 1:bond_grid_size
            b_aux = -kappa*(x_grid[index_x] + py[i-1,j,index_x,index_y]*y_grid[index_y]) #o maximo de endividamento
            c_aux = (1+r)*bond_grid[j] + x_grid[index_x] - b_aux # o consumo no máximo de endividamento
            teste_aux = prob'*reshape(uline.(foo_c(b_aux,x_grid,y_grid)),16,1)
            teste = uline(c_aux) - beta*(1+r)*teste_aux[1] #aqui é o teste da etapa 3 do algoritmo
            if teste > 0 #eis a etapa 3
                cons[i,j,index_x,index_y] = max(c_aux,0)
                py[i,j,index_x,index_y] = p(cons[i,j,index_x,index_y],y_grid[index_y])
            else
                function f(c) #etapa 4: vamos primeiro estabelecer uma função para a rotina do Julia buscar por zeros
                    bb = (1+r)*bond_grid[j] + x_grid[index_x] -c
                    aux = prob'*reshape(uline.(foo_c(bb,x_grid,y_grid)),16,1)
                    uline(c) - beta*(1+r)*aux[1]
                end
                cons[i,j,index_x,index_y] = find_zeros(f,1e-8,(1+r)*bond_grid[j] + x_grid[index_x]+2)[1] #busca por um zero na equação de Euler e salva o valor que zera
                py[i,j,index_x,index_y] = p(cons[i,j,index_x,index_y],y_grid[index_y])
            end
        end
    end
    global err = maximum(abs.(cons[i,:,:,:]-cons[i-1,:,:,:])) #qual a mudança máxima em valor absoluto entre as duas iterações?
    println("iteration",i,"error",err) #só nos informa da evolução do algoritmo
    global i = i+1
end

plot(bond_grid,cons[50,:,1],lab = string("y = ",y_grid[1]), w=2)
plot!(bond_grid,cons[50,:,2],lab = string("y = ",y_grid[2]), w=2)
plot!(bond_grid,cons[50,:,3],lab = string("y = ",y_grid[3]), w=2)
plot!(bond_grid,cons[50,:,4],lab = string("y = ",y_grid[4]), w=2)

next_bond1 = (1+r)*bond_grid .+ y_grid[1] - cons[50,:,1]
debt_limit_val = -kappa*y_grid[1]
debt_limit = similar(next_bond1)
debt_limit .= debt_limit_val

plot(bond_grid,next_bond1,w=2,label = "Bonds next period", legend = :topleft)
plot!(bond_grid,debt_limit,w=2,line = :dash, label = "Debt Restriction")

next_bond2 = (1+r)*bond_grid .+ y_grid[2] - cons[50,:,2]
debt_limit_val2 = -kappa*y_grid[2]
debt_limit2 = similar(next_bond2)
debt_limit2 .= debt_limit_val2

plot(bond_grid,next_bond2,w=2,label = "Bonds next period", legend = :topleft)
plot!(bond_grid,debt_limit2,w=2,line = :dash, label = "Debt Restriction")
