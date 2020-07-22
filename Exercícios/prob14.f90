!Programa calcula as m somas dos valores dos elementos de cada linha e n somas dos valores dos elementos de cada coluna
!imprimir dois conjuntos de elementos com os resultados dessas somas.

program prob14

!alocação de variáveis
implicit none
integer::i,m,j,v,n
real::s*8
real,allocatable::b(:,:)*8,c(:)*8,d(:)*8
character::resp


!Números de elementos do conjunto
write(6,11)
11 format(//t10,"Programa calcula as m somas dos valores dos elementos de cada linha"/t10,"e n somas dos valores dos elementos de cada coluna"/t10,&
"Ele imprime dois conjuntos de elementos com os resultados dessas somas."/)

98 v=1
1 do while (v==1)
write(6,10)
10 format(//t10,"Digite o numero de linhas M e colunas N da matriz, maior ou igual a 2."/)
read(5,*,err=1)m,n
if ((n<=1).or.(m<=1)) then
	write(6,12)
	12 format(//t10,"O valor de M e N deve ser maior ou igual a 2."/)
else
	v=0
end if
end do

allocate(b(m,n))
allocate(c(m))
allocate(d(n))

!leitura da matriz.

write(6,14)
14 format(//t12,"-Escreva os elementos da matriz- "/)

do i=1,m,1
write(*,*)
	do j=1,n,1
		2 write(*,19)i,j
		19 format(3x,"Elemento (",i2,",",i2,"):")
		read(5,*,err=2)b(i,j)
	end do
end do

!Calculos das somas.


do i=1,m,1
	s=0
	do j=1,n,1
		s=s+b(i,j)
	end do
	c(i)=s
end do

do j=1,n,1
	s=0
	do i=1,m,1
		s=s+b(i,j)
	end do
	d(j)=s
end do

!imprimir conjunto

write(*,41)
	41 format(//t10,"O conjunto das somas das linhas e :"/)
	write(*,42)(c(i),i=1,m,1)
	42 format(//t10,10(f6.2,1x))

write(*,43)
	43 format(//t10,"O conjunto das somas das colunas e :"/)
	write(*,44)(d(i),i=1,n,1)
	44 format(//t10,10(f6.2,1x))


!fim


deallocate(b)
deallocate(c)
deallocate(d)

write(*,32)
32 format (/t20,"Deseja calcular novamente?"/t20,"Tecle S se sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 98
  end if
write(6,46)
46 format(//t5,"Fim do programa de soma dos elementos das linha e colunas."/)
stop "Encerrado"
end program prob14


