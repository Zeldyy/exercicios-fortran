							 !MAT045-Programação Científica
   
   ! Problema 2 - Ler um conjunto numérico p com k elementos.
   !               Calcular média aritmetica desses elementos
   !  
	 program prob02
   !
   !  Declaração das variáveis
   !
     implicit none
	 integer::n,v,i
	 real, allocatable::p(:)*8
	 character::resp
	 real(kind=8)::m,s



write(6,35)
35 format(//t20,"Programa calcula media aritmetica de um&
 conjunto formado."/)

!Números de elementos do conjunto
39 v=1
do while (v==1)
write(6,33)
33 format(//t20,"Digite a quantidade de elementos do conjunto"/)
read(5,*,err=1)n
if (n<=0) then
write(*,34)
34 format(//t20,"O valor deve ser inteiro positivo. "/)
cycle
else
exit
end if

1 write(*,340)
340 format(//t20,"O valor deve ser inteiro positivo. "/)
end do

allocate(p(n))

!leitura dos conjuntos.
2	write(6,36)
36 format(//t20,"Escreva os elementos do conjunto."/)
	read(*,*,err=2)(p(i),i=1,n,1)

!calculo da media

m=0
do i=1,n,1
m=p(i)+m
end do
s=m/n

!imprimir resultado.

write(*,50)s
50 format (/t20,"O valor da media e:",f20.2,"."/)
deallocate(p)

write(*,32)
32 format (/t20,"Deseja calcular novamente?"/t20,"&
 Tecle S se	sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 39
  end if
write(6,46)
46 format(//t20,"Fim do programa de calcular media."/)
stop "Encerrado"
end program prob02

