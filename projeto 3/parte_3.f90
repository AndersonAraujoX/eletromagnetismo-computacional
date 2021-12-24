program tarefaA
  implicit real(a-h,o-z)
  integer,dimension(7)::ipx,ipy,ipz
  real, dimension(:,:,:),allocatable::volt,p
  real, dimension(:,:),allocatable::Ex,Ey
  write(*,*)'tamanho do array[-N:N,-N:N,-N:N]'
  read(*,*)N
  write(*,*)'digite o valor da carga'
  read(*,*)q
  write(*,*)"escreva as novas coordenadas da carga como se fosse colocar em uma matriz[-N:N,-N:N]"
  read(*,*)x0,y0
  allocate(volt(-N:N,-N:N,-N:N),p(-N:N,-N:N,-N:N),Ex(-N:N,-N:N),Ey(-N:N,-N:N)) 
  open(1,file='saida_p3')
  open(2,file='E_p3')
  !caminho aleatório
  ipx(1)=1
  ipx(2)=-1
  ipx(3)=0
  ipx(4)=0
  ipx(5)=0
  ipx(6)=0

  ipy(1)=0
  ipy(2)=0
  ipy(3)=1
  ipy(4)=-1
  ipy(5)=0
  ipy(6)=0

  ipz(1)=0
  ipz(2)=0
  ipz(3)=0
  ipz(4)=0
  ipz(5)=1
  ipz(6)=-1
  !configurando as condições iniciais.
  dex=1.0/N
  do i=-N,N
    do j=-N,N
      do k=-N,N
        volt(i,j,k)=0
        p(i,j,k)=0
        if(i==x0 .and. j==y0 .and. k==0)then
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
    soma_f=0
    flag1=0
    
    do while(flag1<=100)
      flag=flag+1
      i=0
      j=0
      k=0
      do while(1<8)!parar quando chegar na parede
        a=rand()!caminho aleatorio
        i=i+ipx(int(1+6*a))
        j=j+ipy(int(1+6*a))
        k=k+ipz(int(1+6*a))
        if(abs(i)==N .or. abs(j)==N .or. abs(k)==N)then
          exit
        elseif(i==x0 .and. j==y0)then!caso o caminho aleatório chegue uma vez x0,y0 o loop.
          flag1=flag1+1
        endif
        volt(i,j,k)=(volt(i-1,j,k)+volt(i+1,j,k)+volt(i,j+1,k)+volt(i,j-1,k)+volt(i,j,k+1)+volt(i,j,k-1))/6+p(i,j,k)*dex*dex/6
      enddo
    enddo
    do i=-N+1,N-1
      do j=-N+1,N-1
        do k=-N+1,N-1
          soma_f=soma_f+volt(i,j,k)
        enddo
      enddo
    enddo
    !write(*,*)flag,soma_f-soma_p  
  enddo
  write(*,*)"quantidade de interações",flag
  do i=-N+1,N-1!campo eletrico na direção x
    do j=-N+1,N-1
      Ex(i,j)=-(volt(i+1,j,0)-volt(i-1,j,0))/2
    enddo
  enddo


  do i=-N+1,N-1!campo eletrico na direção y
    do j=-N+1,N-1
      Ey(i,j)=-(volt(i,j+1,0)-volt(i,j-1,0))/2
    enddo
  enddo

  do i=-N+1,N-1
    do j=-N+1,N-1!depositando os dados no arquivo
      write(2,*)i*1.0/N,j*1.0/N,Ex(i,j)*10,Ey(i,j)*10
    enddo
  enddo

  do i=-N+1,N-1
    do j=-N+1,N-1!depositando os dados no arquivo
      write(1,*)i*1.0/N,j*1.0/N,volt(i,j,0)
    enddo
  enddo

end program
