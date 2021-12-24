function prod_triplo(r_l,a,u)
  implicit real*8(a-h,o-z)
  real*8,dimension(3),intent(in)::r_l,a,u
  real*8,dimension(2)::prod_triplo
  ra=r_l(1)*a(1)+r_l(2)*a(2)+r_l(3)*a(3)
  ru=r_l(1)*u(1)+r_l(2)*u(2)+r_l(3)*u(3)
  prod_triplo(1)=ru*a(1)-ra*u(1)
  prod_triplo(2)=ru*a(2)-ra*u(2)
end function
function bisseccao(t0,w0,a0,x,y)
  implicit real*8(a-h,o-z)
  !sabemos que tempo que w(t0)=0 é t0=(-v0+sqrt(v0**2-2*x0*a0))/a0
  !write(*,*)x
    tl=acos(0.0d0)/w0
    t=tl
    f0=sqrt((x-a0*dcos(w0*t))**2+(y)**2)-(t0-t)
    t=t-tl
    f1=sqrt((x-a0*dcos(w0*t))**2+(y)**2)-(t0-t)
    do while(f0*f1>0)!vamos encontrar a mudança de sinal
      f0=sqrt((x-a0*dcos(w0*t))**2+(y)**2)-(t0-t)
      t=t-tl
      f1=sqrt((x-a0*dcos(w0*t))**2+(y)**2)-(t0-t)
    enddo
    !encontrando 
    a=t
    b=t+tl!metodo da bessecção
    do while(abs(sqrt((x-a0*dcos(w0*c))**2+(y)**2)-(t0-c))>0.05)
      t=a
      f0=sqrt((x-a0*dcos(w0*t))**2+(y)**2)-(t0-t)
      c=(a+b)/2.0
      t=c
      f1=sqrt((x-a0*dcos(w0*t))**2+(y)**2)-(t0-t)
      if(x==-50 .and. y==-37)then 
        write(*,*)a,b,f0,f1
        call sleep(1)
      endif
      if(f0*f1<0.0)then
        b=c
      else
        a=c
      endif
    enddo
  bisseccao=c
end function
program tarefaA
  implicit real*8(a-h,o-z)
  real*8,dimension(3)::u,r_l,E,S,ac
  write(*,*)'digite a variação de w'
  read(*,*)w
  write(*,*)'digite a amplitude'
  read(*,*)a
  h=1.0d0
  i=50
  j=50
  open(1,file='E_p3b')
  dt=0.001d0
  t0=-dt
  pi=acos(0.0d0)
  w0=0.0d0
 !vamos supor que a posição da particula será (0,0,0) no instante que w(t0)=0.0
    do while(w0<10*w)
        w0=w0+w
        !write(*,*)w0
        soma=0
        t0=0
        do while(t0<=2.0d0*pi/w0)
          t0=t0+dt
          !write(*,*)t0,2.0d0*pi/w0
          !tempo retardado
          t=bisseccao(t0,w0,a,i*1.0d0,j*1.0d0)
          !vetor posição retarda
          r_l(1)=i*h-a*cos(w0*t)!x
          r_l(2)=j*h!y
          r_l(3)=0!z
          r=sqrt(r_l(1)*r_l(1)+r_l(2)*r_l(2)+r_l(3)*r_l(3))!modulo
          !u
          u(1)=r_l(1)/r+a*w0*sin(w0*t)!x
          u(2)=r_l(2)/r!y
          u(3)=0!z
          ru=r_l(1)*u(1)+r_l(2)*u(2)+r_l(3)*u(3)!produto escalar
          !Vamos usar a identidade do produto vetorial triplo Ax(BxC)=(A.C)B-(B.C)A
          !então no nosso caso rx(uxa)=(r.a)u-(u.r)a
          ac(1)=-a*w0**2*cos(w0*t)
          ac(2)=0
          ac(3)=0
          ra=r_l(1)*(-a*w0**2*cos(w0*t))!produto escalar r com a
          !campo elétrico
          E(1)=r/ru**3*(ra*u(1)-ru*ac(1))
          E(2)=r/ru**3*(ra*u(2)-ru*ac(2))
          !vetor de poyting
        
          S(1)=E(1)*E(1)*r_l(1)
          S(2)=E(2)*E(2)*r_l(2)
          !fazendo a soma do periodo
          if(t0<=dt)then!começo da integral começo
            S_i=(sqrt(S(1)**2+S(2)**2))
          else if(t0>=2.0d0*pi/w0-dt)then!final da integral começo
            S_f=(sqrt(S(1)**2+S(2)**2))
          else
            soma=soma+(sqrt(S(1)**2+S(2)**2))
          endif
          !q/(4 pi epislon0)=1
        enddo
        trape=soma+(S_i+S_f)/2.0d0
        write(1,*)log(w0),log(trape/(2.0d0*pi/w0)*dt)
      enddo
end program
