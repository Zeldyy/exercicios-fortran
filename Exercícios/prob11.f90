	  !Programa verifica se a soma dos n-1 termos da linha de uma matriz quadrada é igual ao valor do ultimo termo.

program prob11

!alocação de variáveis
implicit none
integer::i,n,j,v,cor
real,allocatable::b(:,:)*8,a(:)*8
character::resp


!Números de elementos do conjunto
write(6,11)
11 format(//t10,"Programa verifica se a soma dos n-1 termos da linha"/t10," de uma matriz quadrada e igual ao valor do ultimo termo."/)

98 v=1
1 do while (v==1)
write(6,10)
10 format(//t10,"Digite a ordem da matriz quadrada, maior ou igual a 3."/)
read(5,*,err=1)n
if (n<=2) then
	write(6,12)
	12 format(//t10,"O valor deve ser maior ou igual a 3."/)
else
	v=0
end if
end do

allocate(b(n,n))

!leitura da matriz.

write(6,14)
14 format(//t12,"-Escreva os elementos da matriz- "/)

do i=1,n,1
	do j=1,n,1
		2 write(*,19)i,j
		19 format(/t10,"Elemento (",i2,",",i2,")"/)
		read(5,*,err=2)b(i,j)
	end do
end do

!criação de conjunto que representa as somas dos n-1 termos.
allocate(a(n))
do i=1,n,1
a(i)=0
	do j=1,n-1,1
		a(i)=a(i)+b(i,j)
	end do
end do

!verificar se todos os ultimos valores da matriz representam as somas dos n-1 termos
cor=1
do  i=1,n,1
if (a(i)==b(i,n)) then

else
	cor=0
	exit
end if
end do

!Imprimir resultado.

select case(cor)

case(1)
	write(*,9)
9	format(//t10,"O ultimo valor representa a soma dos n-1 termos da matriz."/)
case(0)
	write(*,23)
23	format(//t10,"O ultimo valor NAO representa a soma dos n-1 termos da matriz."/)

end select

!fim

deallocate(b)
deallocate(a)

write(*,32)
32 format (/t20,"Deseja calcular novamente?"/t20,"Tecle S se sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 98
  end if
write(6,46)
46 format(//t20,"Fim do programa de verificacao de matriz."/)
stop "Encerrado"
end program prob11


