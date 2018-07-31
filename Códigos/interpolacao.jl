using Polynomials
using Plots

x2 = linspace(0,6,10)
x_aux = linspace(0,6,100)

y = cos.(x2)

pol = polyfit(x2,y)

scatter(x2,y)
plot!(x_aux,polyval(pol,x_aux))

y2 = broadcast(*,x2,cos.(x2))

pol2 = polyfit(x2,y2)

scatter(x2,y2)
plot!(x_aux,polyval(pol2,x_aux))

x3 = linspace(-1,1,10)
y3 = 1./(1+25*x3.^2)

pol3 = polyfit(x3,y3)

x_aux = linspace(-1,1,100)

scatter(x3,y3)
plot!(x_aux,polyval(pol3,x_aux))
