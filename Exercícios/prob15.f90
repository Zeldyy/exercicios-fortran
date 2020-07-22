!Programa que identifica as raízes da equação ax²+bx+c=0 e diz se elas são reais, reais iguais ou imaginárias.

program prob15
implicit none

!declaração de variáveis

real(kind=8)::a,b,c
character::resp
complex(kind=8)::x1,x2,delta
integer::v,tipo


write(*,1)
1 format(//t10,"Programa que identifica as raizes da equacao Ax^2+Bx+C=0,"/t10,&
"e diz se elas sao reais, reais iguais ou imaginarias."/)

!ler e validar A,B e C

30 v=1
do while (v==1)
	write(*,2)
	2 format(//t10,"Digite os coeficientes da equacao A, B e C."/)
	read(*,*,err=3)a,b,c
	if (a==0) then
		write(*,40)
		40 format(//t10,"O coeficiente A deve ser diferente de zero."/)
		cycle
	end if
	exit
	3 write(*,4)
	4 format(//t10,"Os valores devem ser reais."/)
	cycle
end do


!Calculo das raízes.
delta=b*b-4*a*c
x1=(-b+sqrt(delta))/(2*a)
x2=(-b-sqrt(delta))/(2*a)

!Verificação do tipo da raiz.

if (delta==0) then
	write(*,5)
	5 format(//t10,"As raizes sao reais e iguais, de valor:"/)
	tipo=1
end if


if (int(delta)>0) then
	write(*,6)
	6 format(//t10,"As raizes sao reais e diferentes, de valores:"/)
	tipo=2
end if

if (int(delta)<0) then
	write(*,7)
	7 format(//t10,"As raizes sao imaginarias e diferentes, de valores:"/)
	tipo=3
end if

!imprimir Raízes e tipos

select case(tipo)

case(1)
	write(*,8)real(x1)
	8 format(/t5,f7.2/)

case(2)
	write(*,9)real(x1),real(x2)
	9 format(/t5,f7.2,"   e ",f7.2/)

case(3)
	write(*,17)x1
	17 format(/t5,"Primeira raiz:"1x,f7.2," Real e",1x,f7.2," Complexo."/)
	write(*,27)x2
	27 format(/t5,"Segunda raiz:"1x,f7.2," Real e",1x,f7.2," Complexo."/)

end select

!Fim

Write(*,10)
10 format(//t10," Deseja calcular novamente?"/,t10,"Digite S se sim e qualquer outra tecla se nao."/)
read(*,*)resp
if ((resp=="s").or.(resp=="S")) go to 30

Write(*,11)
11 format(//t10,"Fim do programa de calculo de raizes."/)

stop "Encerrado."
end program prob15

