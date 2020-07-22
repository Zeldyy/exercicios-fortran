!Programa que faz o conjunto diferença entre dois conjuntos.

program prob08

!alocação de variáveis
implicit none
integer(kind=4)::i,a,b,k,l,u,v
real,allocatable::d(:),j(:),c(:)
character::resp


!Números de elementos do conjunto
write(6,11)
11 format(//t10,"Programa que faz o conjunto diferenca entre dois conjuntos."/t10,"A - B = C"/)

98 v=1
1 do while (v==1)
write(6,10)
10 format(//t10,"Digite a quantidade de elementos dos conjuntos A e B."/)
read(5,*,err=1)a,b
if ((a<0).or.(b<0)) then
	write(6,12)
	12 format(//t10,"O valor deve ser maior ou igual a 0."/)
else
	v=0
end if
end do

allocate(d(a))
allocate(j(b))
allocate(c(a))

if (a==0) then
write(*,77)
77 format(//t10,"O conjunto C e vazio."/)
go to 42
end if



!leitura do conjunto.

2 write(6,14)
14 format(//t10,"Escreva os elementos do conjunto A :"/)
	read(5,*,err=2)(d(i),i=1,a,1)

41 write(6,19)
19 format(//t10,"Escreva os elementos do conjunto B :"/)
	read(5,*,err=41)(j(i),i=1,b,1)


!determinar se um dos conjuntos são vazios.

if (b==0) then
write(*,72)
72 format(//t10,"O conjunto C e :"/)
write(*,79)(d(i),i=1,a,1)
79 format(//t10,10(f5.2,1x))
go to 42
end if



!Determinar valores de B que pertencem a A.
u=0
do k=1,a,1
l=1

do i=1,b,1
if (d(k)==j(i)) then
 l=0
 end if
end do

if (l==1) then
u=u+1
c(u)=d(k)
 
 end if

end do

!Caso B possua todos os valores de A.
if (u==0) then
write(*,87)
87 format(//t10,"O conjunto C e vazio."/)
go to 42
end if



!imprimir valor de C.

write(*,80)
80 format(//t10,"O conjunto C e :"/)
write(*,81)(c(i),i=1,u,1)
81 format(//t10,10(f6.2,1x))


42 deallocate(d)
deallocate(j)
deallocate(c)

 write(*,32)
32 format (/t20,"Deseja calcular novamente?"/t20,"Tecle S se sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 98
  end if
write(6,46)
46 format(//t20,"Fim do programa de Troca de Posicoes"/)
stop "Encerrado"
end program prob08

