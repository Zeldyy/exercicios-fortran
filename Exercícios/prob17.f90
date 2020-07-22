!Programa que determina se um triângulo é equilátero, isosceles ou escaleno,
! e se é acutângulo, retângulo ou obtusângulo.

program prob17
implicit none

!declaração de variáveis

real(kind=8)::d1,d2,d3,p(3,2),a,b,c,d,f,aa,k,l,m
character::resp
integer::v


write(*,1)
1 format(//t10,"Programa que determina se um triangulo"/t10," e equilatero, isosceles ou escaleno"/t10,"e se e acutangulo, retangulo ou obtusangulo."&
/,t10,"Valores aproximados."/)

!determinar o Triângulo.
write(*,32)
32 format(//t5,"Para determinar o triangulo, determine as coordenadas dos 3 vertices."/)

30 v=1
do while (v==1)

	write(*,2)
	2 format(//t10,"Digite as coordenadas x e y do ponto p1"/)
	read(*,*,err=3)p(1,1),p(1,2)

	write(*,31)
	31 format(//t10,"Digite as coordenadas x e y do ponto p2"/)
	read(*,*,err=3)p(2,1),p(2,2)

	write(*,34)
	34 format(//t10,"Digite as coordenadas x e y do ponto p3"/)
	read(*,*,err=3)p(3,1),p(3,2)
	
	exit

	3 write(*,4)
	4 format(//t10,"Os valores devem ser numeros reais."/)

	cycle

end do

f=p(1,1)*p(2,2)*1+p(1,2)*1*p(3,1)+1*p(2,1)*p(3,2)&
 - (p(3,1)*p(2,2)*1+p(3,2)*1*p(1,1)+1*p(2,1)*p(1,2))
 if (f==0) then
 write(*,59)
	59 format(//t10," Os valores nao formam um triangulo. Por favor digite novamente."/)
	go to 30
end if




!calculo dos lados dos triangulos
d1=sqrt((p(2,1)-p(1,1))**2+(p(2,2)-p(1,2))**2)
d2=sqrt((p(3,1)-p(2,1))**2+(p(3,2)-p(2,2))**2)
d3=sqrt((p(3,1)-p(1,1))**2+(p(3,2)-p(1,2))**2)
k=anint(d1**2)
l=anint(d2**2)
m=anint(d3**2)

	   
!Determinar se o triângulo é equilatero, isosceles ou escaleno.

if ((k==l).and.(l==m)) then
	write(*,5)
	5 format(//t10,"O Triangulo e equilatero."//t10,"O Triangulo e acutangulo.")
	go to 99
end if

if ((k==l).or.(k==m).or.(l==m)) then
	write(*,6)
	6 format(//t10,"O triangulo e isosceles."/)
else
		write(*,7)
	7 format(//t10,"O triangulo e escaleno."/)
end if
	


!Determinar maior lado.



a=amax1(d1,d2,d3)
b=amin1(d1,d2,d3)
if ((b==d1).and.(a==d2)) c=d3
if ((b==d2).and.(a==d1)) c=d3
if ((b==d3).and.(a==d1)) c=d2
if ((b==d3).and.(a==d2)) c=d1
if ((b==d2).and.(a==d3)) c=d1
if ((b==d1).and.(a==d3)) c=d2







!Verificar se é retângulo ou obtusângulo.
aa=anint(a**2)
d=anint(b**2+c**2)
if (aa==d) then
	write(*,45)
			45 format(//t10,"O triangulo e retangulo."/)
end if
if (aa>d) then
	write(*,46)
			46 format(//t10,"O triangulo e obtusangulo."/)
end if
if (aa<d) then
	write(*,47)
			47 format(//t10,"O triangulo e acutangulo."/)
end if	

!Fim
 99 Write(*,10)
10 format(//t10," Deseja calcular novamente?"/,t10,"Digite S se sim e qualquer outra tecla se nao."/)
read(*,*)resp
if ((resp=="s").or.(resp=="S")) go to 30

Write(*,11)
11 format(//t10,"Fim do programa de verificacao de posicao do ponto."/)

stop "Encerrado."
end program prob17