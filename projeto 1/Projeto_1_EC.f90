program EC
implicit real*8(a-h,o-z)
real*8,dimension(1000)::x,y
integer N
write(*,*)'Quantidade de cargas'
read(*,*)N
pi=2*acos(0.0)
do i=0,N!colocando as cargas dentro do circulo
   theta=2*pi*rand()
   r=rand()
   x(i)=r*cos(theta)
   y(i)=r*sin(theta)

enddo
!fazendo a movimenta��o
do k=0,N
     write(1,*)x(k),y(k)
enddo
do i=0,N
!write(*,*)i
flag=0
V_ant=0
do l=0,N
   do j=0,N
      r=sqrt((x(l)-x(j))**2+(y(l)-y(j))**2)
      if (r>0) then
         V_ant=V_ant-log(r)
      endif
   enddo
enddo !procurar melhor posi��o em todas as cargas
   do while(flag<1000)!caso saia do circulo parar o processo e seguir para o pr�ximo
      theta=2*pi*rand() !dire��o aleat�ria
      x(i)=x(i)+0.01*cos(theta)
      y(i)=y(i)+0.01*sin(theta)
      V_at=0!voltagem atual
      do l=0,N
         do j=0,N
            r=sqrt((x(l)-x(j))**2+(y(l)-y(j))**2)
            if (r>0) then
               V_at=V_at-log(r)
            endif
         enddo
      enddo
      if (sqrt(x(i)**2+y(i)**2)>1) then
         x(i)=x(i)-0.01*cos(theta)!retorna a carga para posi��o anterior
         y(i)=y(i)-0.01*sin(theta)
         flag=flag+1
      else if (V_at-V_ant<0) then!trocando a voltagem caso seja 
         flag=0
         V_ant=V_at!V atual se torna antigo
      else!caso ddp for o maior que o anterior 10 vezes paraa o 
         x(i)=x(i)-0.01*cos(theta)!retorna a carga para posi��o anterior
         y(i)=y(i)-0.01*sin(theta)
         flag=flag+1
      end if
   enddo
enddo
do k=0,N
   write(2,*)x(k),y(k)
enddo
end program
