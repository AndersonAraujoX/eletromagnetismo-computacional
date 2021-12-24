
program tarefaA
  implicit real*8(a-h,o-z)
  real*8,dimension(14)::valorh
  !vetor que 
  valorh = (/0.5,0.2,0.1,0.05,0.01,0.005,0.001,0.0005,0.0001,0.00005,0.00001,0.000001,0.0000001,0.00000001/)
  open(1,file="saida1-a-11371311.dat",status="replace")
  open(2,file="saida2-a-11371311.dat",status="replace")
  !os valores analiticos que usaremos para saber o desvio do valro real
  ordem1=0.25*dcos(0.25d0)*sinh(2.0d0)+2*dsin(0.25d0)*dcosh(2.0d0)
  ordem2=real(63)/16*dsin(0.25d0)*dsinh(2.0d0)+cos(0.25d0)*dcosh(2.0d0)
  ordem3=real(191)/64*dcos(0.25d0)*dsinh(2.0d0)+real(61)/8*dsin(0.25d0)*dcosh(2.0d0)
  write(1,13)"h","sim 3 pontos","p/ frente 2 p","p/ traz 2 p","sim 5 pontos","2ª sim 5 pontos","3ª anti-simétrica 5p"
  write(2,13)"h","sim 3 pontos","p/ frente 2 p","p/ traz 2 p","sim 5 pontos","2ª sim 5 pontos","3ª anti-simétrica 5p"
  !do que irá percorrer todo vetor que está salvo os valores de h 
  do i=1,14
    f_0=dsinh(2.0d0)*dsin(0.25d0)!f(0)
    f_1=dsinh(2.0d0*(1+valorh(i)))*dsin(0.25d0*(1+valorh(i)))!f(1+h)
    f_n1=dsinh(2.0d0*(1-valorh(i)))*dsin(0.25d0*(1-valorh(i)))!f(1-h)
    f_2=dsinh(2.0d0*(1+2*valorh(i)))*dsin(0.25d0*(1+2*valorh(i)))!f(1+2h)
    f_n2=dsinh(2.0d0*(1-2*valorh(i)))*dsin(0.25d0*(1-2*valorh(i)))!f(12h)
    !aproximações de ordem 1
    derivt2=(f_1-f_0)/valorh(i)
    derivf2=(f_0-f_n1)/valorh(i)
    derivs3=(f_1-f_n1)/(2*valorh(i))
    derivs5=(f_n2-8*f_n1+8*f_1-f_2)/(12*valorh(i))
    !aproximações de ordem 2
    deriv2s3=(f_n1-2*f_0+f_1)/valorh(i)**2
    deriv2s5=(-f_n2+16*f_n1-30*f_0+16*f_1-f_2)/(12*(valorh(i)**2))
    !aproximações de ordem 3
    deriv3an5=(-f_n2+2*f_n1-2*f_1+f_2)/(2*(valorh(i)**3))
   erro=abs(deriv3an5-ordem3)!coloquei essa variavel 'erro' por causa que o código tava dando erro que linha tava grande demais, então coloquei em uma variavel para caber
   write(1,14)valorh(i),derivs3,derivf2,derivt2,derivs5,deriv2s5,deriv3an5
   write(2,14)valorh(i),abs(derivs3-ordem1),abs(derivf2-ordem1),abs(derivt2-ordem1),abs(derivs5-ordem1),abs(deriv2s5-ordem2),erro
enddo
  14 format('|| ',f14.12,' | ',f14.12,' | ',f14.12,' | ',f14.12,' | ',f15.13,' | ',f15.12, ' | ',f19.12,'||')
  13 format('|| ',a14,' | ',a14,' | ',a14,' | ',a14,' | ',a15,' | ',a16, ' | ',a21,'||')
  write(1,*)"derivada de 1° ordem",ordem1,"derivada de 2° ordem",ordem2,"derivada de 3° ordem",ordem3
  write(2,*)"derivada de 1° ordem",ordem1,"derivada de 2° ordem",ordem2,"derivada de 3° ordem",ordem3
end program
