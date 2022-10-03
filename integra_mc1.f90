program integra_mc1
implicit none
real::Vc,V,x,y,z,sigma,p
integer::aciertos,n,i
n=100000
aciertos=0
Vc=2
do i=1,n
   x=rand()
   y=rand()
   z=2*rand()
   if (y>=x/2 .and. y<=(1-x/2) .and. z<=2-x-2*y) then
    aciertos=aciertos+1
   end if
enddo
p=aciertos/float(n)
V=p*Vc
sigma=sqrt(V*(Vc-V)/float(n))
print*,'======================================================'
print*,'Probabilidad de acierto: ',p
print*,'El volumen es: ',V
print*,'Con un error de: ',sigma
print*,'Intervalo de confianza:',V-sigma,V+sigma
print*,'======================================================'
end program integra_mc1
