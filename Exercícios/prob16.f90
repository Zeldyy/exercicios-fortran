!Programa que determina se um ponto está a direita, dentro ou à esquerda de uma reta.

program prob16
implicit none

!declaração de variáveis

real(kind=8)::D,p(3,2)
character::resp
integer::v


write(*,1)
1 format(//t10,"Programa que determina se um ponto esta a direita,"/t10,&
"dentro ou a esquerda de uma reta."/)

!determinar a Reta.
write(*,32)
32 format(//t5,"Para determinar a reta, determine as coordenadas de 2 pontos dessa reta."/)

30 v=1
do while (v==1)

	write(*,2)
	2 format(//t10,"Digite as coordenadas x e y do ponto p1"/)
	read(*,*,err=3)p(1,1),p(1,2)

	write(*,31)
	31 format(//t10,"Digite as coordenadas x e y do ponto p2"/)
	read(*,*,err=3)p(2,1),p(2,2)

	if ((p(1,1)==p(2,1)).and.(p(1,2)==p(2,2))) then
		write(*,40)
		40 format(//t10,"Os pontos nao formam uma reta. Digite novamente."/)
		cycle
	end if
	exit
	3 write(*,4)
	4 format(//t10,"Os valores devem ser numeros reais."/)
	cycle
end do

!Determinar o ponto.

do while (v==1)

	write(*,34)
	34 format(//t2,"Reta formada, digite agora as coordenadas x e y do ponto p3"/)
	read(*,*,err=35)p(3,1),p(3,2)
	exit
	35 write(*,36)
	36 format(//t10,"Os valores devem ser numeros reais."/)
end do


!calculo do determinante.
D=p(1,1)*p(2,2)*1+p(1,2)*1*p(3,1)+1*p(2,1)*p(3,2)&
 - (p(3,1)*p(2,2)*1+p(3,2)*1*p(1,1)+1*p(2,1)*p(1,2))

  
!Verificação do tipo da raiz.

if (D==0) then
	write(*,5)
	5 format(//t10,"O ponto p3 esta dentro da reta."/)
	end if
	


if (D>0) then
	write(*,6)
	6 format(//t10,"O ponto p3 esta a esquerda da reta."/)
end if

if (D<0) then
	write(*,7)
	7 format(//t10,"O ponto p3 esta a direita da reta."/)
end if


!Fim

Write(*,10)
10 format(//t10," Deseja calcular novamente?"/,t10,"Digite S se sim e qualquer outra tecla se nao."/)
read(*,*)resp
if ((resp=="s").or.(resp=="S")) go to 30

Write(*,11)
11 format(//t10,"Fim do programa de verificacao de posicao do ponto."/)

stop "Encerrado."
end program prob16