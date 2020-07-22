		 !Programa le dois centros de circunferencias e seus dois raios,
		 ! e diz quantos pontos estão em comum.
program prob20
implicit none

!Declaração de variáveis.

real(kind=8)::p(2,2),d,r1,r2,r3,r4,raios,comp
character::resp
integer::v


write(*,1)
1 format(//t10,"Programa le dois centros de circunferencias e seus dois raios"/t10,"e diz quantos pontos estao em comum."/)


!Leitura e validação de pontos.


30 v=1
do while (v==1)

101 write(*,32)
32 format(//t5,"Determine as coordenadas do centro e o raio do circulo 1."/)
	read(*,*,err=3)p(1,1),p(1,2),r1
	if (r1<=0) then
		write(*,100)
		100 format(/t10,"O raio deve ser maior que 0."/)
		go to 101
	end if
	
103	write(*,31)
	31 format(//t5,"Digite agora as coordenadas do centro e o raio do circulo 2."/)
	read(*,*,err=3)p(2,1),p(2,2),r2

	if (r2<=0) then
		write(*,102)
		102 format(/t10,"O raio deve ser maior que 0."/)
		go to 103
	end if
	
	exit


3 write(*,4)
4 format(//t10,"Os valores devem ser numeros reais."/)




end do
!calculo da distâncias entre os raios.

d=sqrt((p(2,1)-p(1,1))**2+(p(2,2)-p(1,2))**2)

raios=r1+r2
!Verificar o numero de pontos em comum.

if (d==0) then
	if (r1==r2) then
		write(*,995)
		995 format(/t10,"Todos os pontos estao em comum."/)	!casos concentricos.
		go to 99
	else
		write(*,996)
		996 format(/t10,"Nao existe ponto em comum."/)
		go to 99
	end if
end if

if (d==raios) then
	write(*,990)
	990 format(/t10,"Existe um ponto em comum."/)  !raios se tocando com distancias iguais.
	go to 99
end if


if (d>raios) then 
	write(*,991)
	991 format(/t10,"Nao existe um ponto em comum."/) !distancia supera intersecção dos raios.
end if

	 
if (d<raios) then !caso mais complexo onde engloba maiores possibilidades.
r3=min(r1,r2)
r4=max(r1,r2)
comp=d+r3

	if ((comp)==r4) then
		write(*,992)
		992 format(/t10,"Existe um ponto em comum."/)
		go to 99
	end if
	if ((comp)>r4) then
		write(*,993)
		993 format(/t10,"Existe dois pontos em comum."/)
	else 
		if	((comp)<r4) then
			write(*,994)
			994 format(/t10,"Nao exitem pontos em comum."/)
		end if
	end if

end if

!fim.

99 Write(*,10)
10 format(//t10," Deseja verificar outras circunferencias?"/,t10,"Digite S se sim e qualquer outra tecla se nao."/)
read(*,*)resp
if ((resp=="s").or.(resp=="S")) go to 30

Write(*,11)
11 format(//t10,"Fim do programa de verificacao de circunferencia."/)

stop "Encerrado."
end program prob20