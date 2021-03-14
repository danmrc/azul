using Interpolations
using Plots
using Polynomials

grid = range(-2,2,length=10)
grid2 =  range(-2,2,length=50)

f(x)=(1+10*x^2)^(-1)

plot(grid2,f.(grid2))

f_hat = Polynomials.fit(grid2,f.(grid2))

plot(grid2,f.(grid2), lab = "Function")
plot!(grid2,f_hat.(grid2), lab = "Interpolation")

function cheby_nod(seq)
    m = length(seq)
     return map(k->-cos((2k-1)/(2m)*pi),seq)
 end

new_grid = 2*cheby_nod(1:20)

fit_cheb = Polynomials.fit(new_grid,f.(new_grid))

plot(grid2,f.(grid2), lab = "Function")
plot!(grid2,fit_cheb.(grid2), lab = "Interpolation")

poly_cheb = mapreduce(j->new_grid.^j,hcat,1:10)
ols_cheb = inv(poly_cheb'*poly_cheb)*poly_cheb'*f.(new_grid)

poly_grid2 = mapreduce(j->grid2 .^j,hcat,1:10)

fit_cheb = poly_grid2*ols_cheb

plot(grid2,f.(grid2), lab = "Function")
plot!(grid2,fit_cheb, lab = "Interpolation")

lin_interp = LinearInterpolation(grid,f.(grid))

plot(grid2,f.(grid2),lab = "Function")
plot!(grid2,lin_interp.(grid2), lab = "Interpolation")

grid3 = range(-2,2,length = 100)

lin_interp = LinearInterpolation(grid2,f.(grid2))

plot(grid3,f.(grid3),lab = "Function")
plot!(grid3,lin_interp.(grid3), lab = "Interpolation")
