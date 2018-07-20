program e_mejorado2d
implicit none
real::t,x,y,f,g,h,ti,tf,un,vn,tn,xf,yf,m1,s1,xver,yver
integer::i,n
print*,'Ingrese ti: '
read*,ti
print*,'Ingrese tf: '
read*,tf
print*,'Cuantas iteraciones desea realizar: '
read*,n
!==================================================================
!este programa resuelve el siguiente sistema de dos ecuaciones
!dx/dt=x-2y
!dy/dt=2x+y
!usando el metodo de euler mejorado
!la solucion exacta es x=-4exp(t)sen(t); y=4exp(t)cos(2t)
! como ya sabemos la solucion exacta calculamos el error relativo
!porcentual
!la xf, yf: es la solucion numerica del problema
!yver,xver: es la solucion exacta del problema
!==================================================================
x=0
y=4
t=0
h=(tf-ti)/n
print*,'       t               x          x-verdad            y             y-verdad '
do i=1,n
   m1=f(t,x,y)
   s1=g(t,x,y)
   un=x+h*m1
   vn=y+h*s1
   tn=t+h
   xf=x+0.5*h*(m1+f(tn,un,vn))
   yf=y+0.5*h*(s1+g(tn,un,vn))
   xver=-4*exp(tn)*sin(2*tn)
   yver=4*exp(tn)*cos(2*tn)
   print*,tn,xf,xver,yf,yver
   t=tn
   x=xf
   y=yf
  
end do
end program e_mejorado2d

real function f(t,x,y)
    implicit none
    real::t,x,y
    f=x-2*y
    end function f
   
real function g(t,x,y)
     implicit none
real::t,x,y
      g=2*x+y
      end function