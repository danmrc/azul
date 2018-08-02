using Polynomials
using Plots
using Dierckx

x2 = linspace(0,6,10)
x_aux = linspace(0,6,100)

y = cos.(x2)

pol = polyfit(x2,y)

f = Spline1D(x2,y,k=1)

scatter(x2,y,lab="Pontos para interpolação")
plot!(x_aux,polyval(pol,x_aux),lw = 2,lab="Polinômio interpolador")
plot!(x_aux,cos.(x_aux),linestyle = :dot,lw = 2, lab = "Função verdadeira")
plot!(x_aux,f.(x_aux), linestyle = :dash, lw = 2,lab =  "Aproximação linear")

y2 = broadcast(*,x2,cos.(x2))

pol2 = polyfit(x2,y2)

scatter(x2,y2)
plot!(x_aux,polyval(pol2,x_aux))

x3 = linspace(-3,3,15)
y3 = 1./(1+25*x3.^2)

pol3 = polyfit(x3,y3)

x_aux = linspace(-3,3,100)

g = Spline1D(x3,y3,k=1)

scatter(x3,y3,lab="Pontos para interpolação")
plot!(x_aux,polyval(pol3,x_aux),lw=2,lab="Polinômio interpolador")
plot!(x_aux,1./(1+25*x_aux.^2),lw=2,linestyle=:dot, lab = "Função verdadeira")
plot!(x_aux,g.(x_aux),linestyle = :dash, lab = "Aproximação Linear", lw = 2)

x4 = linspace(0.1,2,10)
au = 1./x4
y4 = x4.*sin.(au)

pol4 = polyfit(x4,y4)

h = Spline1D(x4,y4,k=1)

x_aux = linspace(0.1,2,50)

scatter(x4,y4)
plot!(x_aux,pol4.(x_aux),lw = 2)
plot!(x_aux,(x_aux.*sin.(1./x_aux)),lw= 2, linestyle = :dash)
plot!(x_aux,h.(x_aux), lw = 2, linestyle = :dot)
