program tarefaA
  implicit real(a-h,o-z)
  real, dimension(:,:),allocatable::volt,Ex,Ey
  write(*,*)'tamanho do array[-N:N]'
  read(*,*)N
  allocate(volt(-N:N,-N:N),Ex(-N:N,-N:N),Ey(-N:N,-N:N)) 
  open(1,file='saida_p1')
  open(2,file='E_p1')
  !configurando as condições iniciais.
  do i=-N,N
    do j=-N,N
      if (abs(i)==N)then
        volt(i,j)=sign(1,i)
      else if (abs(j)==N)then
        volt(i,j)=i*1.0/N
      else
        volt(i,j)=0
      endif
    enddo
  enddo
  do j=-N,N!ultima configuracao, usando txt
    write(4,*)(volt(i,j),i=-N,N)
  enddo
  do k=1,100000!calculando a mudança de potêncial
    do i=-N+1,N-1
      do j=-N+1,N-1
        volt(i,j)=(volt(i-1,j)+volt(i+1,j)+volt(i,j+1)+volt(i,j-1))*0.25
      enddo
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
    do j=-N+1,N-1!depositando os dados no arquivo
      write(2,*)i,j,Ex(i,j)*10,Ey(i,j)*10
    enddo
  enddo

  do i=-N+1,N-1
    do j=-N+1,N-1!depositando os dados no arquivo
      write(1,*)i,j,volt(i,j)
    enddo
  enddo

end program
