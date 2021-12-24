program carlo
  integer::l
  !criar um vetor para exponencial
  real*8::magne,magnean
  real,dimension(-4:4)::pma,pme
  integer,dimension(1000)::plus,minus
  byte s(1000,1000)!gerando a matriz spin s(i,j)
  character*1 isimb(-1:1)
  integer,parameter :: seed = 15000!gerando uma semente
  call srand(seed)
  nao=rand(seed)!ignorar essa linha 

  
  isimb(1) = "+"
  isimb(-1) = "-"

  !inserindo dados
  write(*,*)'escreva o valor de beta'
  read(*,*)b
  write(*,*)'escreva o tamanho do sistema'
  read(*,*)l
  magne=0!zerando valor media magnética

  do j=-4,4,2!vetores dos expoentes
    pma(j)=exp(j*b)
    pme(j)=exp(-j*b)
  enddo

  do k=1,l!gerando o vetor minus e plus
    plus(k)=k+1
    minus(k)=k-1
  enddo
  plus(l)=1
  minus(1)=l
  
  
  !gerando o sistema de spin
  do i=1,l
    do j=1,l 
      s(i,j)=1
    enddo
  enddo

  do i=1,l!primeira configuracao
    write(10,*)(s(i,j),j=1,l)
  enddo

  do ia=1,100!interações

      do ic=1,(l)**2 !contador de monte carlos l**2

      !escolhendo uma coordenada aleatorio.
        i=(l)*rand()+1
        j=(l)*rand()+1

      !calculo da probabilidade
        M=s(i,j)*(s(plus(i),j)+s(minus(i),j)+s(i,plus(j))+s(i,minus(j)))
        prob=pma(M)/(pma(M)+pme(M))

        if(rand()>=Prob)then!probabilidade do spin mudar
          s(i,j)=s(i,j)*(-1)
        endif
      enddo

    do i=1,l!somando os spin para calcular o magnetismo
      do j=1,l 
        magne=magne+s(i,j)
      enddo
    enddo

    write(2,*)ia,magne/(l)**2!media magnetica
    magne=0
  enddo
  do i=1,l!ultima configuracao
    write(11,*)(s(i,j),j=1,l)
  enddo
  do i=1,l!ultima configuracao, usando txt
    write(12,*)(isimb(s(i,j)),j=1,l)
  enddo
end program