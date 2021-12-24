function prod_triplo(r_l,a,u)
  implicit real*8(a-h,o-z)
  real*8,dimension(3),intent(in)::r_l,a,u
  real*8,dimension(2)::prod_triplo
  ra=r_l(1)*a(1)+r_l(2)*a(2)+r_l(3)*a(3)
  ru=r_l(1)*u(1)+r_l(2)*u(2)+r_l(3)*u(3)
  prod_triplo(1)=ru*a(1)-ra*u(1)
  prod_triplo(2)=ru*a(2)-ra*u(2)
end function
function bisseccao(x0,v0,a0,x,y)
  implicit real*8(a-h,o-z)
  !sabemos que tempo que w(t0)=0 é t0=(-v0+sqrt(v0**2-2*x0*a0))/a0
  !write(*,*)x
    t0=(-v0-sqrt(v0**2-2*x0*a0))/a0!instante que particula passa pelo x=0
    t=t0+0.001
    f0=sqrt((x-x0-v0*t-a0*t**2/2.0d0)**2+(y)**2)-(t0-t)
    t=t-t0
    f1=sqrt((x-x0-v0*t-a0*t**2/2.0d0)**2+(y)**2)-(t0-t)
    do while(f0*f1>0)!vamos encontrar a mudança de sinal
      f0=sqrt((x-x0-v0*t-a0*t**2/2.0d0)**2+(y)**2)-(t0-t)
      t=t-t0
      f1=sqrt((x-x0-v0*t-a0*t**2/2.0d0)**2+(y)**2)-(t0-t)
    enddo
    !encontrando 
    a=t
    b=t+t0
    c=(a+b)/2.0!metodo da bessecção
    do while(abs(sqrt((x-x0-v0*c-a0*c**2/2.0d0)**2+(y)**2)-(t0-c))>0.000001)
      t=a
      f0=sqrt((x-x0-v0*t-a0*t**2/2.0d0)**2+(y)**2)-(t0-t)
      c=(a+b)/2.0
      t=c
      f1=sqrt((x-x0-v0*t-a0*t**2/2.0d0)**2+(y)**2)-(t0-t)
      !write(*,*)a,b,c,f0,f1
      if(f0*f1<0.0)then
        b=c
      else
        a=c
      endif
      c=(a+b)/2.0
    enddo
  bisseccao=c
end function
program tarefaA
  implicit real*8(a-h,o-z)
  real*8,dimension(3)::u,r_l,E
  write(*,*)'digite a posição'
  read(*,*)x0
  write(*,*)'digite a velocidade'
  read(*,*)v0
  write(*,*)'digite a aceleração'
  read(*,*)a0
  write(*,*)'digite a espessura'
  read(*,*)h
  N=10
  t0=(-v0+sqrt(v0**2-2*x0*a0))/a0
  if (v0+a0*t0<1)then
  write(*,*)"faz sentido físico"
  open(1,file='E_p2')
  open(2,file='B_p2')
  !do i=-N,N
  !  do j=-N,N 
  !    write(3,*)i,j,bisseccao(x0,v0,i*1.d0),-(j+x0/v0)
  !  enddo
  !enddo
  !vamos supor que a posição da particula será (0,0,0) no instante que w(t0)=0.
  do i=-N,N
    do j=-N,N
      !write(*,*)i,j
      t=bisseccao(x0,v0,a0,i*h,j*h)!tempo retardado
      !vetor posição retarda
       r_l(1)=i*h-x0-v0*t-a0*t**2/2.0d0!x
       r_l(2)=j*h!y
       r_l(3)=0!z
       r=sqrt(r_l(1)*r_l(1)+r_l(2)*r_l(2)+r_l(3)*r_l(3))!modulo
       !u
       u(1)=r_l(1)/r-(v0+a0*t)!x
       u(2)=r_l(2)/r!y
       u(3)=0!z
       ru=r_l(1)*u(1)+r_l(2)*u(2)+r_l(3)*u(3)!produto escalar
       !Vamos usar a identidade do produto vetorial triplo Ax(BxC)=(A.C)B-(B.C)A
       !então no nosso caso rx(uxa)=(r.a)u-(r.u)a
       ra=r_l(1)*a0!produto escalar r com a
       E(1)=r/ru**3*((1-v0**2)*u(1)+(ra*u(1)-ru*a0))
       E(2)=r/ru**3*((1-v0**2)*u(2)+(ra*u(1)))
       !q/(4 pi epislon0)=1
       if(i==0 .and. j==0)then
       else
        write(1,*)i*h,j*h,E(1),E(2)
       endif
    enddo
  enddo
    do j=0,2*N
      !tempo retardado
      t=bisseccao(x0,v0,a0,0.0d0*1.0d0,j*1.0d0)
       !vetor posição retarda
       r_l(1)=-x0-v0*t-a0*t**2/2.0d0!x
       r_l(2)=j*h/2.0d0!y
       r_l(3)=0!z
       r=sqrt(r_l(1)*r_l(1)+r_l(2)*r_l(2)+r_l(3)*r_l(3))!modulo
       !u
       u(1)=r_l(1)/r-(v0+a0*t)!x
       u(2)=r_l(2)/r!y
       u(3)=0!z
       ru=r_l(1)*u(1)+r_l(2)*u(2)+r_l(3)*u(3)!produto escalar
       !Vamos usar a identidade do produto vetorial triplo Ax(BxC)=(A.C)B-(B.C)A
       !então no nosso caso rx(uxa)=(r.a)u-(u.a)r
       ra=r_l(1)*a0!produto escalar r com a      
       E(1)=r/ru**3*((1-v0**2)*u(1)+(ra*u(1)-ru*a0))
       E(2)=r/ru**3*((1-v0**2)*u(2)+(ra*u(1)))
       !q/(4 pi epislon0)=1
       Bz=r_l(1)*E(2)-r_l(2)*E(1)
       if(j==0)then
       else
        write(2,*)j*h/2.0d0,Bz
       endif
    enddo
  else
    write(*,*)"não faz sentido físico"
  endif
  !configurando as condições iniciais.
end program
