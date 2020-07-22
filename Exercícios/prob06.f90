!Programa que le um conjunto e o ordena.

program prob06

!alocação de variáveis
implicit none
integer::n,v,i,ord,ne
character::resp
real::q
real,allocatable::b(:)


write(6,35)
35 format(//t20,"Programa que le um conjunto e o ordena."/)

!Números de elementos do conjunto
17 v=1
do while (v==1)
1 write(6,33)
33 format(//t20,"Digite a quantidade de elementos do conjunto"/)
read(5,*,err=1)n
if (n<=1) then
write(*,34)
34 format(//t20,"O valor deve ser maior que 1 "/)
else
	v=0
end if
end do

allocate(b(n))

!leitura dos conjuntos.
2	write(6,36)
36 format(//t20,"Escreva os elementos do conjunto"/)
	read(5,*,err=2)(b(i),i=1,n,1)

!ordenar o conjunto.
ord=0
do ne=n,2,-1
do i=1,ne-1,1
if (b(i)>=b(i+1)) then
	ord=1
	q=b(i)
	b(i)=b(i+1)
	b(i+1)=q
end if	
end do
if (ord==0) exit 
end do

!Imprimir novo conjunto.


write(*,66)
66 format (//t5,"O novo conjunto e :"/)
write(*,67)(b(i),i=1,n,1)
67 format(/t3,8(f6.2,1x)/)

!fim do programa e cicle.

deallocate (b)
write(*,32)
32 format (/t20,"Deseja ordenar novamente?"/t20,"Tecle S se	sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 17
  end if
write(6,46)
46 format(//t20,"Fim do programa de ordenar conjuntos."/)
stop "Encerrado"
end program prob06

