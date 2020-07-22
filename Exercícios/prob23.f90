program prob23
implicit none


!Programa imprime tabela informando posição de um projétil lançado na atmosfera com velocidade constante em função do tempo.
!A é a inclinação em relação à terra.
!intervalos de 1 em 1 segundo.

integer::i,v
real,parameter::g=9.81
real,allocatable::T(:,:)
real(kind=8)::angulo,vel,tempo,tempoqueb
integer,allocatable::a(:)
character::resp


write(*,1)
1 format(/t10,"Programa imprime tabela informando posicao de um projetil"/t10,"lançado na atmosfera em funcao do tempo"/)

!Leitura de valores.
133 v=1
do while (v==1)
	write(*,2)
	2 format(/t3,"Digite a angulacao, em radianos, da direcao de lancamento em relacao a Terra:"/)
	read(*,*,err=4)angulo


	if ((angulo>=3.1415).or.(angulo<=0))	then
	write(*,30)
	30 format(/t10,"O angulo deve estar entre 0 e 3.1415."/)
	go to 133
	end if

	write(*,3)
	3 format(/t10,"Velocidade do projetil em metros por segundo:"/)
	read(*,*,err=4)vel

	if ((vel<=0))	then
	write(*,32)
	32 format(/t10,"A velocidade deve ser positiva"/)
	go to 133
	end if


    exit
	4 write(*,20)
	20 format(/t3,"Digite valores reais maiores que 0."/)
end do

 tempo=(2*vel*sin(angulo))/g

allocate(T(int(tempo)+2,2))
allocate(a(int(tempo)+2))
tempoqueb=int(tempo)

!Posições por tempo.
do i=1,tempoqueb+1,1
		a(i)=i-1
		T(i,1)=vel*cos(angulo)*(i-1)
		T(i,2)=vel*sin(angulo)*(i-1)-(g*(i-1)**2)/2
end do
	
  a(int(tempo)+2)=int(tempo)+1
  T(int(tempo)+2,1)=vel*cos(angulo)*tempo
  T(int(tempo)+2,2)=0


!impressão da tabela.

write(*,5)
5 format(//t10,":::::Tabela de posicao por tempo:::::"//t10,"--------------------------------------")
write(*,60)
60 format(t10,"Tempo"," (s)",6x,"X",6x,"Y"/)
do i=1,int(tempo)+2,1
write(*,6)a(i),T(i,1),T(i,2)
6 format(t10,i2," (s)",1x,f6.1," (m)",1x,f6.1," (m)",1x)
end do
write(*,7)
7 format(//t10,"---------------------------------------"/)


!fim

deallocate(T)
deallocate(a)

write(*,21)
21 format(//t10,"Deseja calcular novamente?"/t10,"Digite S se sim ou qualquer outra teclar se nao."/)
read(*,*)resp
if ((resp=="s").or.(resp=="S")) then
go to 133
end if


stop "Fim do programa de projetil."
end program prob23


