				  !Programa que determina se os 4 pontos são um triângulo ou quadrilátero. Se for quadrilátero, verificar se é quadrado.

program prob18
implicit none

!declaração de variáveis

real(kind=8)::d1,d2,d3,d4,p(4,2),z,d,teste(4),area,area2
character::resp
integer::v


write(*,1)
1 format(//t10,"Programa que determina se os 4 pontos sao"/t10,"um triangulo ou quadrilatero."/t10,&
"Se for quadrilatero, verificar se e quadrado."/)

!determinar os pontos.
write(*,32)
32 format(//t5,"Determine as coordenadas dos 4 pontos."/)

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

	write(*,340)
	340 format(//t10,"Digite as coordenadas x e y do ponto p4"/)
	read(*,*,err=3)p(4,1),p(4,2)
	
	exit

	3 write(*,4)
	4 format(//t10,"Os valores devem ser numeros reais."/)

end do 

																  ! a a2	  b b2	 d d2
																   !b b2	  a a2	 c c2
!Calculo do Determinante.										    c c2	  c c2	 a a2
D=abs(p(2,1)*p(1,2)*1+p(2,2)*1*p(3,1)+1*p(1,1)*p(3,2)&			  ! d d2
- (p(3,1)*p(1,2)*1+p(3,2)*1*p(2,1)+1*p(1,1)*p(2,2)))+&			
abs(p(4,1)*p(3,2)*1+p(4,2)*1*p(1,1)+1*p(3,1)*p(1,2)&
 - (p(1,1)*p(3,2)*1+p(1,2)*1*p(4,1)+1*p(3,1)*p(4,2)))

 !caso não exista área

if (d==0) then

	write(*,59)
	59 format(//t10," Os valores nao formam nem um triangulo nem um quadrilatero."/)
	go to 99
end if

!Se entre 3 pontos, não se formar área, e a área dos 4 pontos existir, um triângulo é formado.
teste(1)=p(1,1)*p(2,2)*1+p(1,2)*1*p(3,1)+1*p(2,1)*p(3,2)&
 - (p(3,1)*p(2,2)*1+p(3,2)*1*p(1,1)+1*p(2,1)*p(1,2))		!123 234 341 124 
teste(2)=p(2,1)*p(3,2)*1+p(2,2)*1*p(4,1)+1*p(3,1)*p(4,2)&
 - (p(4,1)*p(3,2)*1+p(4,2)*1*p(2,1)+1*p(3,1)*p(2,2))
 teste(3)=p(3,1)*p(4,2)*1+p(3,2)*1*p(1,1)+1*p(4,1)*p(1,2)&
 - (p(1,1)*p(4,2)*1+p(1,2)*1*p(3,1)+1*p(4,1)*p(3,2))
 teste(4)=p(1,1)*p(2,2)*1+p(1,2)*1*p(4,1)+1*p(2,1)*p(4,2)&
 - (p(4,1)*p(2,2)*1+p(4,2)*1*p(1,1)+1*p(2,1)*p(1,2))

 if ((teste(1)==0).or.(teste(2)==0).or.(teste(3)==0).or.(teste(4)==0)) then
	write(*,590)
	590 format(//t10,"Os pontos formam um triangulo."/)
	go to 99
end if












  
d1=sqrt((p(2,1)-p(1,1))**2+(p(2,2)-p(1,2))**2)
d2=sqrt((p(3,1)-p(2,1))**2+(p(3,2)-p(2,2))**2)
d3=sqrt((p(4,1)-p(3,1))**2+(p(4,2)-p(3,2))**2)
d4=sqrt((p(4,1)-p(1,1))**2+(p(4,2)-p(1,2))**2)


!Verificar se o quadrilátero é um quadrado.
z=min(d1,d2,d3,d4)
area=anint(D/2)
area2=anint(z*z)

if (area==area2) then
	write(*,5900)
	5900 format(//t10,"Os pontos formam um quadrilatero e formam um quadrado."/)
else
	write(*,591)
	591 format(//t10,"Os pontos formam um quadrilatero e NAO formam um quadrado."/)	
end if
 

!Fim
 99 Write(*,10)
10 format(//t10," Deseja calcular novamente?"/,t10,"Digite S se sim e qualquer outra tecla se nao."/)
read(*,*)resp
if ((resp=="s").or.(resp=="S")) go to 30

Write(*,11)
11 format(//t10,"Fim do programa de verificacao de poligonal."/)

stop "Encerrado."
end program prob18