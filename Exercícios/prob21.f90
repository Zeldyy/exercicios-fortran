!programa que calcula fatorial de um número.
program prob21
implicit none
integer(kind=4)::n,i,v,ne,m
integer,allocatable::a(:)
character::resp


write(*,1)
	1 format(/t10,"Este programa mostra os n primeiros numeros primos."/)

!validação de n.
8 v=1
do while (v==1)

	write(*,19)
	19 format(/t10,"Digite o numero de primos."/)
	read(*,*,err=2)n


	if (n<=0) then
	else
			exit
	endif
	2 write(*,3)
	3 format(//t20,"Por favor digite um numero inteiro e maior que 0."/)

end do


 allocate(a(n))
 a(1)= 2


!calculo dos números primos.
m=1
ne = 2
do while (m<n)

88	ne=ne+1

	do i=2,ne-1,1
		if (mod(ne,i)==0) go to 88
	end do
	 
	m=m+1
	a(m) = ne

end do

!Imprimir o conjunto
write(6,15)
15 format(//,t10,"Os n primeiros numeros primos sao: "/)
write(6,16)(a(i),i=1,m,1)
16 format(//15(i3,1x))


deallocate(a)


!repetir programa e fim.		
7 write(*,199)
199 format(//t20,"Deseja calcular outros numeros primos?"/t20,"Se sim digite S, caso nao deseje digite qualquer outra tecla"/)
read(*,*)resp
if (resp=="S".or.resp=="s") then
go to 8
else
endif
		
stop "Fim do Programa de leitura de numeros."
end program prob21