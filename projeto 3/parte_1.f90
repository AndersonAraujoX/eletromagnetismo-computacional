program tarefaA
  implicit real*8(a-h,o-z)
  real*8, dimension(:,:,:),allocatable::volt,p
  real*8, dimension(:,:),allocatable::Ex,Ey
  write(*,*)'tamanho do array[-N:N,-N:N,-N:N]'
  read(*,*)N
  write(*,*)'digite o valor da carga'
  read(*,*)q
  allocate(volt(-N:N,-N:N,-N:N),p(-N:N,-N:N,-N:N),Ex(-N:N,-N:N),Ey(-N:N,-N:N)) 
  open(1,file='saida_p1')
  open(2,file='E_p1')
  !configurando as condições iniciais.
  dex=1.0/N
  do i=-N,N
    do j=-N,N
      do k=-N,N
        volt(i,j,k)=0
        p(i,j,k)=0
        if(i==0 .and. j==0 .and. k==0)then
          p(i,j,k)=q/(dex*dex*dex)
        endif
      enddo
    enddo
  enddo
  flag=0
  soma_f=1
  soma_p=0
  d3=(2*N)**3
  do while(abs(soma_f-soma_p)>0.000001*d3) !calculando a mudança de potêncial
    soma_p=soma_f
    soma_f=0.0d0
    flag=flag+1
    do i=-N+1,N-1
      do j=-N+1,N-1
        do k=-N+1,N-1
          volt(i,j,k)=(volt(i-1,j,k)+volt(i+1,j,k)+volt(i,j+1,k)+volt(i,j-1,k)+volt(i,j,k+1)+volt(i,j,k-1))/6+p(i,j,k)*dex*dex/6
          soma_f=soma_f+volt(i,j,k)
        enddo
      enddo
    enddo
  enddo
  write(*,*)"quantidade de interações",flag
  do i=-N+1,N-1!campo eletrico na direção x
    do j=-N+1,N-1
      Ex(i,j)=-(volt(i+1,j,0)-volt(i-1,j,0))/(2*dex)
    enddo
  enddo

  !write(*,*)'a'
  do i=-N+1,N-1!campo eletrico na direção y
    do j=-N+1,N-1
      Ey(i,j)=-(volt(i,j+1,0)-volt(i,j-1,0))/(2*dex)
    enddo
  enddo

  do i=-N+1,N-1
    do j=-N+1,N-1!depositando os dados no arquivo
      write(2,*)i*1.0/N,j*1.0/N,Ex(i,j),Ey(i,j)
    enddo
  enddo

  do i=-N+1,N-1
    do j=-N+1,N-1!depositando os dados no arquivo
      write(1,*)i*1.0/N,j*1.0/N,volt(i,j,0)
    enddo
  enddo

end program
