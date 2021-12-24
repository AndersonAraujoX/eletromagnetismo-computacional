program EC
implicit real*8(a-h,o-z)
real*8,dimension(1000)::x,y,z
integer N
write(*,*)'Quantidade de cargas'
read(*,*)N
write(*,*)'Definir as condicoes da elipse, sendo a, b e c respectivamente'
read(*,*)a,b,c
pi=2*acos(0.0)
do i=0,N!colocando as cargas dentro do circulo
   theta=2*pi*rand()
   phi=2*pi*rand()
   r=rand()
   x(i)=a*r*cos(theta)*sin(phi)
   y(i)=b*r*sin(theta)*sin(phi)
   z(i)=c*r*cos(phi)
enddo

do k=0,N
     write(1,*)x(k),y(k),z(k)
enddo
do i=0,N
   !write(*,*)i
   flag=0
   V_ant=0
   
   do l=0,N
      do j=0,N
         r=sqrt((x(l)-x(j))**2+(y(l)-y(j))**2+(z(i)-z(j))**2)
         if (r>0) then
            V_ant=V_ant+1/r
         endif
      enddo
   enddo !procurar melhor posição em todas as cargas


   do while(flag<1000)!caso saia do circulo parar o processo e seguir para o pr�ximo
      theta=2*pi*rand()
      phi=2*pi*rand()
      x(i)=x(i)+0.01*cos(theta)*sin(phi)
      y(i)=y(i)+0.01*sin(theta)*sin(phi)
      z(i)=z(i)+0.01*cos(phi)
      V_at=0!voltagem atual
      do l=0,N
         do j=0,N
            r=sqrt((x(l)-x(j))**2+(y(l)-y(j))**2+(z(i)-z(j))**2)
            if (r>0) then
               V_at=V_at+1/r
            endif
         enddo
      enddo


      if (sqrt(x(i)*x(i)/(a*a)+y(i)*y(i)/(b*b)+z(i)*z(i)/(c*c))>1) then
         x(i)=x(i)-0.01*cos(theta)*sin(phi)!retornando carga
         y(i)=y(i)-0.01*sin(theta)*sin(phi)
         z(i)=z(i)-0.01*cos(phi)
         flag=flag+1
      else if (V_at-V_ant<0) then!trocando a voltagem caso seja 
         !write(*,*)V_at-V_ant,V_ant,V_at
         flag=0
         V_ant=V_at!V atual se torna antigo
      else!caso ddp for o maior que o anterior 10 vezes paraa o 
         x(i)=x(i)-0.01*cos(theta)*sin(phi)
         y(i)=y(i)-0.01*sin(theta)*sin(phi)
         z(i)=z(i)-0.01*cos(phi)
         flag=flag+1
      end if
   enddo

enddo
do k=0,N
   write(2,*)x(k),y(k),z(k)
enddo
end program
