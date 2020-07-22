!programa que calcula probabilidade de ganhar na megasena.
program prob22
implicit none
real(kind=8)::C,rel
integer::n,i,v
character::resp



write(*,88)
88 format(//t10,"Este programa calcula probabilidade de ganhar na megasena."/)
!validação de n.
8 v=1
do while (v==1)
 write(*,1)
1 format(/t10,"Digite a quantidade de numeros que deseja apostar:"/)
read(*,*,err=2)n

if ((n<6).or.(n>15)) then
else
exit
endif
2 write(*,3)
3 format(//t20,"A Megasena so pode ser apostada com 6 a 15 numeros."/)
end do
rel=fat(n)/(fat(6)*fat(n-6))
C=(fat(60)/(fat(6)*fat(60-6)))/rel

!imprimir


 write(*,6)n,C
 6 format(//t20,"A probabilidade apostando",i2," numeros e de: "/t10,"1:",f9.0/)
	
	
	
!repetir programa e fim.		
7 write(*,19)
19 format(//t20,"Deseja calcular outra probabilidade?"/t20,"Se sim digite S, caso nao deseje digite qualquer outra tecla"/)
read(*,*)resp
if (resp=="S".or.resp=="s") then
go to 8
else

endif

contains

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


end program prob22

		









