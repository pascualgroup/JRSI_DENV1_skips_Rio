### plot of s_critical vs number of skips (n) #
### skips with population growth ###
# dN/dt = (lam-mu)*N ==> N(t)=N0*exp((lam-mu)*t)
# dS/dt = lam*N-mu*S-b*I*S/N
# dI/dt = b*I*S/N - (mu+gamma)*I
# with b = b0*(1+delta*sen(wt+pi))

# under the approximation of the paper L. Stone et al we have --> dS/dt approx lam*N

# Calling Y=log(I)/(gamma+mu) and Z= R0*S (with R0=b0/(gamma+mu)) 
#--> dY/dt=(1+delta*sen(wt+pi))*Z/N-1 and dZ/dt=m*N with m=lamda*R0

# Z = Z0 + m*N0 (exp((lam-mu)*t)-1)/(lam-mu)
# ==> dY/dt = (1+delta*sen(wt+pi)) * [exp(-(lam-mu)*t)*Z0/N0 + (1-exp(-(lam-mu)*t))*m/(lam-mu)] -1

# when integral(dY/dt)_from{0}_to_{time_high_season} > 0 we have an outbreak. 
# Then I will look for the critical value which corresponds to:
#   z0_c * (lam-mu) = m - [ (m-lam+mu)*pi*(2*n+1)-2*delta]/(aux*w), with aux definded as follows in the script.


mu<-1/(365*75) # moratlity rate
w<-2*pi/365 # seasonal frecuency 
growth<-1.55#1.75 #(a growth of 1.74 gives an increment of 9% in 9 years)

delta<-0.2
r0<-1.4 # mean reproductive number


# instead of dS/dt approx lam*N, now we add the other term --> dS/dt approx (lam*N - mu*S)
# ==> now we have Z = R0*N0*exp((lam-mu)*t) - (R0*N0-Z0)*exp(-mu*t)
Boundary_2<-function(n,delta,growth,mu,r0){
  th<-pi*(2*n+1)/w
  lam<-growth*mu
  
  auxB1<-exp(-lam*th)
  auxB2<-w*delta/(w*w+lam*lam)
  auxB3<-auxB2*(1+auxB1) - (1-auxB1)/lam
  
  outputB<- 1+((pi*(2*n+1)*(1-1/r0) - 2*delta))/(auxB3*w)
  
  return(outputB)
}

n<-c(0:20)
sc<-Boundary_2(n,delta,growth,mu,r0)

par(cex=2)
#plot(n,sc,pch=19,col=1,xlab="n (number of skips)", ylab="sc")
plot_df = data.frame(n, sc)
source("rahul_theme.R")
Fig_1_Panel_A = ggplot(data = plot_df, aes(x = n, y = sc)) +
  geom_point(size = 3) + rahul_theme+
  theme_white_background + rahul_man_figure_theme +
  ylab(expression((S[c](n)))) +
  ylab(expression(paste("Value is ", sigma,",", R^{2},'=',r2.value)))
  geom_hline(yintercept = 0.70, color = 'red', linetype = 'dashed')
  ggdraw(p) + 
    draw_label(line_1, x = 0.55, y = 0.075) + # use relative coordinates for positioning
    draw_label(line_2, x = 0.55, y = 0.025)
  
  Fig_1_Panel_A = ggplot(data = plot_df, aes(x = n, y = sc)) +
    geom_point(size = 3) + rahul_theme+
    theme_white_background + rahul_man_figure_theme +
    ylab(expression(paste("Sucept. thres.  after 1st epidemic \n for n skips to occur ", S[c](n)))) +
    geom_hline(yintercept = 0.70, color = 'red', linetype = 'dashed') +
    theme(axis.title.y = element_text(size = 11,
                                      face = "bold",
                                      color = "black"))
  
  
  Fig_1_Panel_A = ggplot(data = plot_df, aes(x = n, y = sc)) +
    geom_point(size = 3) + rahul_theme+
    theme_white_background + rahul_man_figure_theme +
    ylab(expression(paste("Sucept. thres.  after 1st epidemic \n for n skips to occur ", S[c](n)))) +
    geom_hline(yintercept = 0.70, color = 'red', linetype = 'dashed') +
    theme(axis.title.y = element_text(size = 11,
                                      face = "bold",
                                      color = "black"))
  
  Fig_1_Panel_A = ggplot(data = plot_df, aes(x = n, y = sc)) +
    geom_point(size = 3) + rahul_theme+
    theme_white_background + rahul_man_figure_theme +
    ylab(expression(atop("Sucept. thres. ", paste(" for n skips to occur ", S[c](n))))) +
    geom_hline(yintercept = 0.70, color = 'red', linetype = 'dashed') +
    theme(axis.title.y = element_text(size = 11,
                                      face = "bold",
                                      color = "black"))
  p + xlab(expression(atop("A long string of text for the purpose", paste("of illustrating my point" [reported]))))
#Fig_1_Panel_A
# 
# tiff(
#   paste0(
#     "../Figures/Manuscript_Figures/TIFF_Files/Fig1.tiff"),
#   height = 5, width = 10, res = 300, units = "in")
# print(p)
# dev.off()

