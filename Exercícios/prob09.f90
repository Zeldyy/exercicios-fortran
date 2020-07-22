   !Programa faz o produto da diagona secundária de uma matriz quadrada, desconsiderando zeros.

program prob09

!alocação de variáveis
implicit none
integer::i,n,j,k
real,allocatable::b(:,:)*8
real::v*8
character::resp


!Números de elementos do conjunto
write(6,11)
11 format(//t10,"Programa faz o produto da diagona secundaria de uma matriz quadrada,"/t10,"desconsiderando zeros."/)

98 v=1
1 do while (v==1)
write(6,10)
10 format(//t10,"Digite a quantidade de elementos, numero inteiro positivo maior que um."/)
read(5,*,err=1)n
if (n<=1) then
	write(6,12)
	12 format(//t10,"O valor deve ser maior que 1."/)
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
		19 format(/t10,"Elemento (",i1,",",i1,")"/)
		read(5,*,err=2)b(i,j)
	end do
end do


!calculo da diagonal secundaria.

v=1

do k=0,n-1,1
	if (b(n-k,1+k)==0) then

	else
	v=b(n-k,1+k)*v
	end if
end do

!Imprimir produto.

!Imprimir o conjunto
write(6,15)v
15 format(//,t10,"O produto e: "/t15,f7.2/)
deallocate(b)

write(*,32)
32 format (/t20,"Deseja calcular novamente?"/t20,"Tecle S se sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 98
  end if
write(6,46)
46 format(//t20,"Fim do programa de calc. de produto da diagonal secundaria."/)
stop "Encerrado"
end program prob09