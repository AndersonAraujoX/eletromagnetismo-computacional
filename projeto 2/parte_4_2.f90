program tarefaA
  implicit real(a-h,o-z)
  real, dimension(:,:),allocatable::volt,Ex,Ey
  integer,dimension(5)::ipx,ipy
  write(*,*)'tamanho do array[-50:50]'
  read(*,*)N
  allocate(volt(-N:N,-N:N),Ex(-N:N,-N:N),Ey(-N:N,-N:N)) 
  open(1,file='saida_p4')
  open(2,file='E_p4')
  !configurando as condições iniciais.
  ipx(1)=1
  ipx(2)=-1
  ipx(3)=0
  ipx(4)=0
  ipy(1)=0
  ipy(2)=0
  ipy(3)=1
  ipy(4)=-1
  do i=-N,N!condicoes iniciais
    do j=-N,N
      if (abs(i)<=3*N/10 .and. abs(j)<=3*N/10 )then
        volt(i,j)=1
      else
        volt(i,j)=0
      endif
    enddo
  enddo
  do k=1,50000 
    j=0
    i=0
    do while(1<8)!parar quando chegar na parede
      a=rand()!caminho aleatorio
      i=i+ipx(int(1+4*a))
      j=j+ipy(int(1+4*a))
      if(abs(i)>=N .or. abs(j)>=N)then
        !write(*,*)i,j
        exit
      endif
      if (abs(i)>=3*N/10  .or. abs(j)>=3*N/10 )then!enquanto ficar fora do quadrado central continuar calculando
        volt(i,j)=(volt(i-1,j)+volt(i+1,j)+volt(i,j+1)+volt(i,j-1))*0.25
      endif
    enddo
  enddo
  do i=-N+1,N-1!campo eletrico na direção x
    do j=-N+1,N-1
      Ex(i,j)=(volt(i+1,j)-volt(i-1,j))/2
    enddo
  enddo
  !write(*,*)'a'
  do i=-N+1,N-1!campo eletrico na direção y
    do j=-N+1,N-1
      Ey(i,j)=(volt(i,j+1)-volt(i,j-1))/2
    enddo
  enddo
  do i=-N+1,N-1
    do j=-N+1,N-1!depositando os dados nos arquivos
      write(2,*)i,j,Ex(i,j),Ey(i,j)
    enddo
  enddo

  do i=-N+1,N-1
    do j=-N+1,N-1!depositando os dados nos arquivos
      write(1,*)i,j,volt(i,j)
    enddo
  enddo
end program
