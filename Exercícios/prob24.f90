!Este programa le numeros inteiros e verifica se formam uma das permutações sem repetição
! dos numeros inteiros de 1 a n.
program prob24
implicit none
integer(kind=4)::n,v,p,perm,i
character::resp
integer(kind=4),allocatable::a(:)
logical::condic

!validação de n.

write(*,88)
88 format(/t10,"Este programa le numeros inteiros"/t10,"e verifica se formam uma das permutacoes"/t10,"sem repeticao dos numeros inteiros de 1 a n."/)

8 v=1
do while (v==1)
	write(*,1)
	1 format(/t10,"Digite quantos numeros quer digitar."/)
read(*,*,err=2)n
if (n<=0) then
else
exit
endif
2 write(*,3)
3 format(/t10,"Por favor digite um numero inteiro maior que 0."/)
end do
allocate(a(n))

!Ler conjunto A.
82 do while (v==1)
 write(*,80)
80 format(/t10,"Digite os numeros que quer verificar: "/)
read(*,*,err=81)(a(i),i=1,n,1)
exit
81 write(*,30)
30 format(/t10,"Por favor digite um numero inteiro."/)
go to 82
end do

!Verificar condições para calculo e calculo.
perm=1
do i=1,n,1
	do p=1,n,1
	if (a(i)<=0) then
		write(*,42)
		42 format(/t3,"Os numeros NAO sao uma das permutacoes dos numeros de 1 a n"/t3,"pois existe um numero menor ou igual a zero entre os numeros."/)
		go to 7
	end if
	
		if ((a(i)==a(p)).and.(p/=i)) then
			write(*,43)
			43 format(/t3,"Os numeros NAO sao uma das permutacoes dos numeros de 1 a n"/t3,"pois existe um numero repetido."/)
			go to 7
		end if
	end do
	 !calculo.
	perm=a(i)*perm
end do
 condic=(perm==fat(n))

!imprimir
select case(condic)

case (.true.)
	 write(*,40)
	40 format(/t3,"Os numeros sao uma das permutacoes dos numeros de 1 a n."/)
case (.false.)
	write(*,41)
	41 format(/t3,"Os numeros NAO sao uma das permutacoes dos numeros de 1 a n."/)

end select








7 deallocate(a)
	
!repetir programa e fim.		
write(*,19)
19 format(//t20,"Deseja calcular outra verificacao de permutacao?"/t20,"Se sim digite S, caso nao deseje digite qualquer outra tecla"/)
read(*,*)resp
if (resp=="S".or.resp=="s") then
go to 8
else
write(*,*)"Fim do programa de verificacao de permutacao."
endif




contains

!programa que calcula fatorial de um número.
real(kind=8) function fat(a)
implicit none
real(kind=8)::fatorial
integer::a
!calculo de n
if (n==0) then
fat=1
else
fatorial=1
do i=1,a,1
	fatorial=fatorial*i
end do
fat=fatorial
end if
end function fat
		
end program prob24
