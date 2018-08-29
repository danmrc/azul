using Polynomials
using Plots
using Interpolations

pyplot()

x2 = range(0,stop = 6,length = 10) #Cria 10 pontos equiespaçados entre 0 e 6
x_aux = range(0,stop = 6,length = 100) #Aonde vamos avaliar a função e o polinômio que aproxima

y = cos.(x2)

pol = polyfit(x2,y) #faz o fit do polinomio

scatter(x2,y,lab="Pontos para interpolação")
plot!(x_aux,polyval(pol,x_aux), lw=2,lab="Polinômio interpolador")
plot!(x_aux,cos.(x_aux),linestyle=:dash, lw=2, lab = "Função verdadeira")

x3 = range(-1/2,stop = 1/2,length = 10)
y3 = 1 ./(1 .+ 25 .* x3.^2)

pol3 = polyfit(x3,y3)

x_aux = range(-1/2,stop = 1/2,length = 100)

scatter(x3,y3,lab="Pontos para interpolação")
plot!(x_aux,polyval(pol3,x_aux),lab="Polinômio interpolador", lw = 2)
plot!(x_aux,1 ./(1+25*x_aux.^2),linestyle=:dash, lw=2, lab = "Função verdadeira")

x3 = range(-3,stop = 3,length = 15)
y3 = 1 ./(1 .+25 .* x3.^2)

pol3 = polyfit(x3,y3)

x_aux = range(-3,stop = 3,length = 100)

scatter(x3,y3,lab="Pontos para interpolação")
plot!(x_aux,polyval(pol3,x_aux),lw=2,lab="Polinômio interpolador")
plot!(x_aux,1./(1+25*x_aux.^2),lw=2,linestyle=:dot, lab = "Função verdadeira")


x2 = range(0,stop = 6,length = 10)
x_aux = range(0,stop = 6,length = 100)

y = cos.(x2)

aprox_lin = LinearInterpolation(x2,y)

scatter(x2,y,lab="Pontos para interpolação")
plot!(x_aux,polyval(pol,x_aux),lw = 2,lab="Polinômio interpolador")
plot!(x_aux,cos.(x_aux),linestyle = :dot,lw = 2, lab = "Função verdadeira")
plot!(x_aux,aprox_lin.(x_aux), linestyle = :dash, lw = 2,lab =  "Aproximação linear")

x3 = range(-1/2,stop = 1/2,length = 10)
y3 = 1 ./(1 .+25 .*x3.^2)

x_aux = range(-1/2,stop = 1/2,length = 100)

aprox_lin3 = LinearInterpolation(x3,y3)

scatter(x3,y3,lab="Pontos para interpolação")
plot!(x_aux,1./(1+25*x_aux.^2),lw=2,linestyle=:dot, lab = "Função verdadeira")
plot!(x_aux,aprox_lin3.(x_aux),linestyle = :dash, lab = "Aproximação Linear", lw = 2)

x3 = range(-3,stop = 3,length = 10)
3 = 1 ./(1 .+25 .*x3 .^2)

x_aux = range(-3,stop = 3,length = 100)

aprox_lin3 = LinearInterpolation(x3,y3)

scatter(x3,y3,lab="Pontos para interpolação")
plot!(x_aux,1./(1+25*x_aux.^2),lw=2,linestyle=:dot, lab = "Função verdadeira")
plot!(x_aux,aprox_lin3.(x_aux),linestyle = :dash, lab = "Aproximação Linear", lw = 2)

x5 = range(-5,stop = 5,length = 15)
y5 = x5.^2

inter_1 = LinearInterpolation(x5,y5, extrapolation_bc = Interpolations.Flat())
inter_2 = LinearInterpolation(x5,y5,extrapolation_bc = Interpolations.Linear())

x_aux5 = range(-7,stop = 7,length = 200)

scatter(x5,y5, lab = "Pontos para interpolação")
plot!(x_aux5,inter_1(x_aux5), lw = 2, lab = "Extrapolação constante")
plot!(x_aux5,inter_2(x_aux5), lw = 2, lab = "Extrapolação usando última reta")
