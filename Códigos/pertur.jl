#order of par
#
#1. β
#2. γ
#3. α
# 4. δ
# 5. ρ_a

using ForwardDiff
using LinearAlgebra
using NLsolve
using Dates

function foc(yt,yf,x1t,x1f,x2t,x2f,par)
    eq1 =  yt[1]^(-par[2]) - par[1]*yf[1]^(-par[2])*(par[3]*x2t[1]*x1f[1]^(par[3]-1) + 1-par[4])
    eq2 = yt[1] + x1f[1] - (1-par[4])*x1t[1] - x2t[1]*x1t[1]^par[3]
    eq3 = (x2f[1] - 1) - par[5]*(x2t[1]-1)
    return [eq1;eq2;eq3]
end

# Steady state, numeric solution

function foc_steady(vars,par)
    eq1 =  vars[1]^(-par[2]) - par[1]*vars[1]^(-par[2])*(par[3]*vars[3]*vars[2]^(par[3]-1) + 1-par[4])
    eq2 = vars[1] + vars[2] - (1-par[4])*vars[2] - vars[3]*vars[2]^par[3]
    eq3 = (vars[3] - 1) - par[5]*(vars[3]-1)
    return [eq1;eq2;eq3]
end

# Steady State, Analytic solution

function steady(par)
    k = (1/par[3]*(1/par[1] + par[4]-1))^(1/(par[3]-1))
    c = k^par[3] - par[4]*k
    A = 1
    return [c,k,A]
end

start_t = now()

pp = [0.99,1,1/3,1,0]

nlsolve(x->foc_steady(x,pp),[0.3;1.0;1],autodiff = :forward)

start_t2 = now()

steady_an = steady(pp)

# checking that the steady state is the steady state - this should be 0 or close too
foc(steady_an[1],steady_an[1],steady_an[2],steady_an[2],steady_an[3],steady_an[3],pp)

# Perturbation part:

fyt = ForwardDiff.derivative(x->foc(x,steady_an[1],steady_an[2],steady_an[2],steady_an[3],steady_an[3],pp),steady_an[1])
fyf = ForwardDiff.derivative(x->foc(steady_an[1],x,steady_an[2],steady_an[2],steady_an[3],steady_an[3],pp),steady_an[1])

fxt = ForwardDiff.jacobian(x->foc(steady_an[1],steady_an[1],x[1],steady_an[2],x[2],steady_an[3],pp),[steady_an[2] steady_an[3]])
fxf = ForwardDiff.jacobian(x->foc(steady_an[1],steady_an[1],steady_an[2],x[1],steady_an[3],x[2],pp),[steady_an[2] steady_an[3]])

A = [fxf fyf]
B = [fxt fyt]

vals,vec = eigen(-B,A,sortby = x->abs(x))

nx = 2
ny = 1

indx = findall(abs.(vals) .< 1)

V1 = vec[:,indx]

V11 = V1[1:nx,:]
V12 = V1[(nx+1):size(V1,1),:]

if nx != length(indx)
    @warn "BK conditions not valid"
else
    @info "BK conditions verified"
end

D11 = diagm(vals[indx])

hx = V11*D11*inv(V11)
gx = V12*inv(V11)

end_t = now()

irfx = mapreduce(t->hx^t*[0;1],hcat,0:10)
irfx = irfx'

using Plots

plot(irfx[:,1],label = "k")
plot(irfx[:,2], label = "ϵ")

irfy = irfx*gx'

plot(irfy[:,1],label = "c")

grid = range(-0.1,0.1,length = 100);
grid2 = grid .+ steady_an[2];

sol_guess = (1-pp[1]*pp[3])*grid2 .^pp[3];

linear_approx_points = [grid zeros(length(grid))];

linear_approx =  steady_an[1] .+ linear_approx_points*gx';

plot(grid2,sol_guess, label = "Solução analítica", legend = :topleft)
plot!(grid2,linear_approx, label = "Solução aproximada")
vline!([steady_an[2]], label = "Steady state")

#order of par
#
#1. β
#2. γ
#3. α
# 4. δ
# 5. ρ_a

function foc2(y,x,par)
    
    eq1 = y[1]^(-par[2]) - par[1]*y[2]^(-par[2])*(par[3]*x[3]*x[2]^(par[3]-1) + 1 - par[4])
    eq2 = y[1] + x[2] - (1-par[4])*x[1] - x[3]*x[1]^par[3]
    eq3 = (x[4]-1) - par[5]*(x[3]-1)

    return [eq1;eq2;eq3]
end

ss2 = [repeat([ss[1]],2);mapreduce(j->repeat([ss[j]],2),vcat,2:3)]

foc2(ss2[1:2],ss2[3:6],pp)

fy = ForwardDiff.jacobian(x->foc2(x,ss2[3:6],pp),ss2[1:2])
fx = ForwardDiff.jacobian(x->foc2(ss2[1:2],x,pp),ss2[3:6])

A = [fx[:,[2,4]] fy[:,2]]
B = -[fx[:,[1,3]] fy[:,1]]

vals,vecs = eigen(B,A)

indxs = findall(abs.(vals) .< 1)

ny = Int(size(fy,2)/2)
nx = Int(size(fx,2)/2)

if nx == length(indxs)
    @info "Blanchard Khan conditions verified"
else
    @warn "Blanchard Khan conditions not verified"
end

V1 = vecs[:,indxs]

V11 = V1[1:nx,:]
V12 = V1[(nx+1):(nx+ny),:]

D11 = diagm(vals[indxs])

hx = V11*D11*inv(V11)
gx = V12*inv(V11)
