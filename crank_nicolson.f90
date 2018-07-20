program crank_nicolson
    ! este programa utiliza el metodo de Crank Nicolson
real::w(0:100),l(0:100),z(0:100),u(0:100)
real::alfa,Tm,k,lam,h,x,Lo,f
integer::i,j,n,m
print*,'================================'
print*,'Ingrese m, pero m>=3: '
read*,m
print*,'Ingrese n, n>=1: '
read*,n
print*,'Ingrese el tiempo maximo, Tm '
read*,Tm
print*,'Ingrese la longitud:'
read*,Lo
print*,'Ingrese alfa: '
read*,alfa
h=Lo/m
k=Tm/n
lam=(alfa*alfa*k)/h**2
w(m)=0
print*,'================================'
do i=1,m-1
    w(i)=f(i*h)
end do
l(1)=1.0+lam
u(1)=-lam/(2*l(1))
do i=2,m-2
     l(i)=1+lam+0.5*lam*u(i-1)
     u(i)=-lam/(2*l(i))
end do
l(m-1)=1+lam+0.5*lam*u(m-2)
do j=1,n
   t=j*k
   z(1)=((1-lam)*w(1)+0.5*lam*w(2))/l(1)
   do i=2,m-1
      z(i)=((1-lam)*w(i)+0.5*lam*(w(i+1)+w(i-1)+z(i-1)))/l(i)
   end do
   w(m-1)=z(m-1)
   do i=m-2,1,-1
      w(i)=z(i)-u(i)*w(i+1)
   end do
   if (j==n) then
      print*,0.000000, 0.000000000
       do i=1,m-1
           x=i*h
           print*,x,w(i)
        end do
        print*,1.000000000,0.00000000
   endif

 end do

print*,'================================'
end program crank_nicolson

function f(x)
real::x,f
f=sin(3.1416*x)
end function f




