function prod_triplo(r_l,a,u)
  implicit real*8(a-h,o-z)
  real*8,dimension(3),intent(in)::r_l,a,u
  real*8,dimension(2)::prod_triplo
  ra=r_l(1)*a(1)+r_l(2)*a(2)+r_l(3)*a(3)
  ru=r_l(1)*u(1)+r_l(2)*u(2)+r_l(3)*u(3)
  prod_triplo(1)=ru*a(1)-ra*u(1)
  prod_triplo(2)=ru*a(2)-ra*u(2)
end function
function bisseccao(x0,v0,x,y)
  implicit real*8(a-h,o-z)
  !sabemos que tempo que w(t0)=0 é t0=x0/v0
  !write(*,*)x
    t=-x0/v0+0.00000001
    f0=sqrt((x-x0-v0*t)**2+(y)**2)-(-x0/v0-t)
    t=t+x0/v0
    f1=sqrt((x-x0-v0*t)**2+(y)**2)-(-x0/v0-t)
    do while(f0*f1>0)!vamos encontrar a mudança de sinal
      f0=sqrt((x-x0-v0*t)**2+(y)**2)-(-x0/v0-t)
      t=t+x0/v0
      f1=sqrt((x-x0-v0*t)**2+(y)**2)-(-x0/v0-t)
    enddo
    !encontrando 
    a=t
    b=t-x0/v0
   ! write(*,*)a,b,f0,f1
    c=(a+b)/2.0!metodo da bessecção
    do while(abs(sqrt((x-x0-v0*c)**2+(y)**2)-(-x0/v0-c))>0.000001)
      t=a
      f0=sqrt((x-x0-v0*t)**2+(y)**2)-(-x0/v0-t)
      c=(a+b)/2.0
      t=c
      f1=sqrt((x-x0-v0*t)**2+(y)**2)-(-x0/v0-t)
      !write(*,*)a,b,c,f0,f1
      if(f0*f1<=0.0)then
        b=c
      else
        a=c
      endif
      !call sleep(1)
      c=(a+b)/2.0
    enddo
  bisseccao=c
end function
program tarefaA
  implicit real*8(a-h,o-z)
  real*8,dimension(3)::u,r_l
  write(*,*)'digite a posição'
  read(*,*)x0
  write(*,*)'digite a velocidade'
  read(*,*)v0
  write(*,*)'digite a espessura'
  read(*,*)h
  N=10
  open(1,file='E_p1')
  open(2,file='B_p1')
  !do i=-N,N
  !  do j=-N,N 
  !    write(3,*)i,j,bisseccao(x0,v0,i*1.d0),-(j+x0/v0)
  !  enddo
  !enddo
  !vamos supor que a posição da particula será (0,0,0) no instante que w(t0)=0.
  do i=-N,N
    do j=-N,N
      !write(*,*)i,j
      !vetor posição retarda
       r_l(1)=i*h-x0-v0*bisseccao(x0,v0,i*h,j*h)
       r_l(2)=j*h
       r_l(3)=0
       r=sqrt(r_l(1)*r_l(1)+r_l(2)*r_l(2)+r_l(3)*r_l(3))!modulo
       !u
       u(1)=r_l(1)/r-v0
       u(2)=r_l(2)/r
       u(3)=0
       ru=r_l(1)*u(1)+r_l(2)*u(2)+r_l(3)*u(3)
       !q/(4 pi epislon0)=1
       if(i==0 .and. j==0 )then
        !write(*,*)i,j,r*(1-v0**2)*u(1)/ru**3,u(1),r*(1-v0**2)*u(2)/ru**3
       else
        write(1,*)i*h,j*h,r*(1-v0**2)*u(1)/ru**3,r*(1-v0**2)*u(2)/ru**3
       endif
    enddo
  enddo
    do j=-2*N,2*N
      !vetor posição retarda
       r_l(1)=-x0-v0*bisseccao(x0,v0,1*0.0d0,j*h)
       r_l(2)=j*h/2
       r_l(3)=0
       r=sqrt(r_l(1)*r_l(1)+r_l(2)*r_l(2)+r_l(3)*r_l(3))!modulo
       !u
       u(1)=r_l(1)/sqrt(r_l(1)**2+r_l(2)**2+r_l(3)**2)-v0
       u(2)=r_l(2)/sqrt(r_l(1)**2+r_l(2)**2+r_l(3)**2)
       u(3)=0
       ru=r_l(1)*u(1)+r_l(2)*u(2)+r_l(3)*u(3)
       !q/(4 pi epislon0)=1
       
       Bz=r_l(1)*r*(1-v0**2)*u(2)/ru**3-r_l(2)*r*(1-v0**2)*u(1)/ru**3
       if(j==0 )then
        !write(*,*)i,j,r*(1-v0**2)*u(1)/ru**3,u(1),r*(1-v0**2)*u(2)/ru**3
       else
        write(2,*)j*h/2,Bz
       endif

    enddo
    do i=-N,N
      do j=-N,N
          r=sqrt((i*h)**2+(j*h)**2)
         if(i==0 .and. j==0 )then
          !write(*,*)i,j,r*(1-v0**2)*u(1)/ru**3,u(1),r*(1-v0**2)*u(2)/ru**3
         else
          write(3,*)i*h,j*h,(i*h)/r**3,(j*h)/r**3
         endif
      enddo
    enddo
  !configurando as condições iniciais.
end program
