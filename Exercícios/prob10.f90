	  !Programa verifica se na diagonal secundária de uma matriz quadrada existe a sequencia de fibonacci.

program prob10

!alocação de variáveis
implicit none
integer::i,n,j,k,v,fibo
real,allocatable::b(:,:)*8,a(:)
character::resp


!Números de elementos do conjunto
write(6,11)
11 format(//t10,"Programa verifica se na diagonal secundaria de uma matriz quadrada"/t10,"existe a sequencia de fibonacci."/)

98 v=1
1 do while (v==1)
write(6,10)
10 format(//t10,"Digite a ordem da matriz quadrada, maior ou igual a 8."/)
read(5,*,err=1)n
if (n<=7) then
	write(6,12)
	12 format(//t10,"O valor deve ser maior ou igual a 8."/)
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


!Gerar os elementos da sequência Fibonacci

allocate (a(n))
a(1)=0
a(2)=1
do i=3,n,1
a(i)=a(i-1)+a(i-2)
end do

!Verificar se diagonal segundária forma sequência de fibonacci.

fibo=1

do k=0,n-1,1
		if (b(1+k,n-k)==a(1+k)) then

		else
			fibo=0
			exit
		end if
end do


!Resposta da verificação

select case(fibo)

case(1)
	write(*,9)
9	format(//t10,"A diagonal secundaria forma sequencia de Fibonacci."/)
case(0)
	write(*,23)
23	format(//t10,"A diagonal secundaria nao forma sequencia de Fibonacci."/)

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
46 format(//t20,"Fim do programa de verificacao da diagonal secundaria."/)
stop "Encerrado"
end program prob10
