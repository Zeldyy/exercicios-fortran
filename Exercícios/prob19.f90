!Programa Identifica um triângulo a partir de 3 pontos e le um quarto, indicando
!se este está dentro, ao lado ou fora dele.
program prob19
implicit none

!Declaração de variáveis.

real(kind=8)::d1,d2,d3,p(4,2),d,teste(3),areaprinc,area2,area3,area4,areatot
character::resp
integer::v


write(*,1)
1 format(//t10,"Programa identifica um triangulo a partir de 3 pontos"/t10,"e le um quarto, indicando&
 se este esta "/t10,"dentro, ao lado, fora ou no vertice dele."/)


!Leitura e validação de pontos.

write(*,32)
32 format(//t5,"Determine as coordenadas dos 3 pontos que formam um triangulo."/)

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





	D=abs(p(1,1)*p(2,2)*1+p(1,2)*1*p(3,1)+1*p(2,1)*p(3,2)&
 - (p(3,1)*p(2,2)*1+p(3,2)*1*p(1,1)+1*p(2,1)*p(1,2)))

if (d==0) then
	write(*,79)
	79 format(/t10,"Os pontos nao formam um triangulo. Por favor digite-os novamente."/)
else
	exit
end if


3 write(*,4)
4 format(//t10,"Os valores devem ser numeros reais."/)




end do

	


!Leitura do 4 ponto.
do while (v==1)
write(*,340)
340 format(//t10,"Os pontos formam um triangulo."/t10,"Agora digite as coordenadas x e y do ponto p4"/)
read(*,*,err=999)p(4,1),p(4,2)
exit

999 write(*,40)
40 format(//t10,"Os valores devem ser numeros reais."/)
end do

!calculo de área dos 3 primeiros pontos.

areaprinc= d/2

!calculo da área de 3 triangulos formados pelo 4 ponto.


		! 234 341 124 
teste(2)=p(2,1)*p(3,2)*1+p(2,2)*1*p(4,1)+1*p(3,1)*p(4,2)&
 - (p(4,1)*p(3,2)*1+p(4,2)*1*p(2,1)+1*p(3,1)*p(2,2))
teste(3)=p(3,1)*p(4,2)*1+p(3,2)*1*p(1,1)+1*p(4,1)*p(1,2)&
 - (p(1,1)*p(4,2)*1+p(1,2)*1*p(3,1)+1*p(4,1)*p(3,2))
teste(4)=p(1,1)*p(2,2)*1+p(1,2)*1*p(4,1)+1*p(2,1)*p(4,2)&
 - (p(4,1)*p(2,2)*1+p(4,2)*1*p(1,1)+1*p(2,1)*p(1,2))

area2= abs(teste(2))/2
area3= abs(teste(3))/2
area4= abs(teste(4))/2

d1=sqrt((p(4,1)-p(3,1))**2+(p(4,2)-p(3,2))**2)
d2=sqrt((p(4,1)-p(2,1))**2+(p(4,2)-p(2,2))**2)
d3=sqrt((p(4,1)-p(1,1))**2+(p(4,2)-p(1,2))**2)

!Verificar se está no vértice ou nos lados.

if ((d1==0).or.(d2==0).or.(d3==0)) then
write(*,990)
990 format(/t10,"O ponto esta em um dos vertices."/)
go to 99
end if

if ((area2==0).or.(area3==0).or.(area4==0)) then
write(*,790)
790 format(/t10," O ponto esta nos lados."/)
go to 99
end if


!verificação de área.

 areatot = area2+area3+area4
if (areatot>areaprinc) then
	write(*,920)
	920 format(/t10,"O ponto esta fora do triangulo."/)
else
	write(*,921)
	921 format(/t10,"O ponto esta dentro do triangulo."/)
end if


!fim.

99 Write(*,10)
10 format(//t10," Deseja calcular novamente?"/,t10,"Digite S se sim e qualquer outra tecla se nao."/)
read(*,*)resp
if ((resp=="s").or.(resp=="S")) go to 30

Write(*,11)
11 format(//t10,"Fim do programa de verificacao de ponto em triangulo"/)

stop "Encerrado."
end program prob19