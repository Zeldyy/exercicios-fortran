!programa que calcula fatorial de um número.
program prob01
implicit none
integer(kind=4)::n,v
character::resp

!validação de n.

write(*,88)
88 format(/t10,"Este programa mostra o fatorial de um numero."/)
8  call validarn(n)

!imprimir

call fatorial(n)
	
!repetir programa e fim.		

call repeticao(resp,*8)

stop "Fim do programa de fatorial."



contains

!programa que calcula fatorial de um número.
subroutine fatorial(a)
implicit none
integer(kind=4)::a,i
real::fat*8

if (a==0) then 
fat=1
go to 200
end if

!calculo de n
fat=1
do i=1,a,1
fat=fat*i
end do

200 if (a<59) then
write(*,6)a,fat
	6 format(//t20,"O fatorial de ",i3," e: "/,f80.0/)
end if

if (a>=59) then
 write(*,15)a,fat
	15 format(//t20,"O fatorial de ",i3," e: "/t5,es30.20/)
end if

end subroutine fatorial


!subprograma le e valida n.
subroutine validarn(a)
integer::a
v=1
do while (v==1)
 write(*,1)
1 format(/t10,"Digite o numero que deseja calcular o fatorial:"/)
read(*,*,err=2)a
if (n<0) then
else
exit
endif
2 write(*,3)
3 format(/t10,"Por favor digite um numero inteiro e maior ou igual a 0"/)
end do
end subroutine validarn

!subprograma repete programa
subroutine repeticao(a,*)
character::a
7 write(*,19)
19 format(//t20,"Deseja calcular outro fatorial?"/t20,"Se sim digite S, caso nao deseje digite qualquer outra tecla"/)
read(*,*)a
if (a=="S".or.a=="s") then
return 1
else
endif
end subroutine repeticao

		
end program prob01







