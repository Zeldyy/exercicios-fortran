!MAT045-Programação Científica
   
   ! Problema 7 - Ler um conjunto numérico p com k elementos ( k é um número maior que 7).
   !               Verificar se os valores dos elementos de índice ímpar formam uma progressão aritimética e os valores dos elementos de índice par uma progressão geométrica.
   !               Imprimir mensagem confirmando ou negando essas ocorrências.
   !  
	 program prob07
   !
   !  Declaração das variáveis
   !
     implicit none
	 integer::n,v,i,pa,pg
	 real, allocatable::p(:)
	 character::resp
	 real::r,u



write(6,35)
35 format(//t20,"Programa calcula se os valores impares de um"/t20,"conjunto formam&
  uma PA e os de indice par formam&
  uma PG."/)

!Números de elementos do conjunto
39 v=1
do while (v==1)
1 write(6,33)
33 format(//t20,"Digite a quantidade de elementos do conjunto"/)
read(5,*,err=1)n
if (n<=7) then
write(*,34)
34 format(//t20,"O valor deve ser maior que 7."/)
else
	v=0
end if
end do

allocate(p(n))

!leitura dos conjuntos.
2	write(6,36)
36 format(//t20,"Escreva os elementos do conjunto"/)
	read(*,*,err=2)(p(i),i=1,n,1)
	

!Verificação dos valores de indice impar
r=p(3)-p(1)
pa=1
do i=5,n,2
u=p(i-2)+r
if (p(i)==u) then

else
pa=0
 end if
 if (pa==0) exit
end do 


!Verificação dos valores de indice par
if (p(2)==0) then
	pg=0
	go to 1000
end if
r=p(4)/p(2)
pg=1
do i=6,n,2
if (p(i)==p(i-2)*r) then

else
pg=0
end if
if (pa==0) exit
end do 

!Imprimir resposta

1000 select case(pa)

case(1)
write(*,14)
14 format(//t20,"Os elementos de indice impar formam uma p.a."/)
case(0)
write(*,15)
15 format(//t20,"Os elementos de indice impar nao formam uma p.a."/)
end select

select case(pg)

case(1)
write(*,16)
16 format(//t20,"Os elementos de indice par formam uma p.g."/)
case(0)
write(*,17)
17 format(//t20,"Os elementos de indice par nao formam uma p.g."/)
end select
deallocate(p)




write(*,32)
32 format (/t20,"Deseja calcular novamente?&
 Tecle S se	sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 39
  end if
write(6,46)
46 format(//t20,"Fim do programa de descobrir pa e pg."/)
stop "Encerrado"
end program prob07




