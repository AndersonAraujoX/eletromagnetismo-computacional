program EC
implicit real*8(a-h,o-z)
real*8,dimension(1000)::x,y
integer N
write(*,*)'Quantidade de cargas'
read(*,*)N
write(*,*)'tamanho as condições da elipse'
read(*,*)a,b
pi=2*acos(0.0)
do i=0,N!colocando as cargas dentro do circulo
   theta=2*pi*rand()
   r=rand()
   x(i)=a*r*cos(theta)
   y(i)=b*r*sin(theta)
enddo
!fazendo a movimenta��o
do k=0,N
     write(1,*)x(k),y(k)
enddo


do i=0,N
flag=0
V_ant=0
do l=0,N
   do j=0,N
      r=sqrt((x(l)-x(j))**2+(y(l)-y(j))**2)
      if (r>0) then
         V_ant=V_ant+1/r
      endif
   enddo
enddo !procurar melhor posi��o em todas as cargas


   do while(flag<100)!caso saia do circulo parar o processo e seguir para o pr�ximo
      theta=2*pi*rand() !dire��o aleat�ria
      x(i)=x(i)+0.01*cos(theta)
      y(i)=y(i)+0.01*sin(theta)
      V_at=0!voltagem atual
      do l=0,N
         do j=0,N
            r=sqrt((x(l)-x(j))**2+(y(l)-y(j))**2)
            if (r>0) then
               V_at=V_at+1/r
            endif
         enddo
      enddo


      if (sqrt((x(i)/a)**2+(y(i)/b)**2)>1) then
         x(i)=x(i)-0.01*cos(theta)!retorna a carga para posi��o anterior
         y(i)=y(i)-0.01*sin(theta)
         flag=flag+1
      else if (V_at-V_ant<0) then!trocando a voltagem caso seja 
         !write(*,*)V_at-V_ant,V_ant,V_at
         flag=0
         V_ant=V_at!V atual se torna antigo
      else!caso ddp for o maior que o anterior 100 vezes para o programa cancelar
         !write(*,*)flag
         x(i)=x(i)-0.01*cos(theta)!retorna a carga para posi��o anterior
         y(i)=y(i)-0.01*sin(theta)
         !write(*,*)flag
         flag=flag+1
      end if


   enddo
enddo
do k=0,N
   write(2,*)x(k),y(k)
enddo
end program
