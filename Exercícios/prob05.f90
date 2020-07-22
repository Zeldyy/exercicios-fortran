!Programa que le um conjunto e indica seu menor e maior valor, bem como sua posição no conjunto

program prob05

!alocação de variáveis
implicit none
integer::n,v,i,ord,ne,x,y
character::resp
integer,allocatable::posmenor(:),posmaior(:)
real,allocatable::b(:),j(:)
real::q


write(6,35)
35 format(/t10,"Programa calcula o maior e menor valor de um conjunto"/t10,"&
 bem como sua posicao."/)
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
allocate(j(n))
allocate(posmenor(n))
allocate(posmaior(n))

!leitura dos conjuntos.
2	write(6,36)
36 format(//t20,"Escreva os elementos do conjunto"/)
	read(5,*,err=2)(b(i),i=1,n,1)
	do i=1,n,1
	j(i)=b(i)
	end do

!ordenar o conjunto.
ord=0
do ne=n,2,-1
do i=1,ne-1,1
if (j(i)>=j(i+1)) then
	ord=1
	q=j(i)
	j(i)=j(i+1)
	j(i+1)=q
end if	
end do
if (ord==0) exit 
end do
!Descobrir a ordem
x=0
y=0
do i=1,n,1
if (j(1)==b(i)) then
x=x+1
posmenor(x)=i
endif
if (j(n)==b(i)) then
y=y+1
posmaior(y)=i
end if
end do
! Imprimir valores
!write(6,*)"O elemento de menor valor do conjunto e: ",j(1)," e sua(s) posicao(es) no conjunto e :",(posmenor(i),i=1,x,1)
!write(6,*)"O elemento de maior valor do conjunto e: ",j(n)," e sua(s) posicao(es) no conjunto e :",(posmaior(i),i=1,y,1)

write(6,37)j(1)
37 format(/t10,"O elemento de menor valor do conjunto e: ",f8.3/t10," e sua(s) posicao(es) no conjunto e :"/)
write(6,40) (posmenor(i),i=1,x,1)
 40 format(//t4,12(i2,1x))

write(6,38)j(n)
38 format(/t10,"O elemento de maior valor do conjunto e: ",f8.3/t10," e sua(s) posicao(es) no conjunto e :"/)
write(6,41) (posmaior(i),i=1,y,1)
 41 format(//t4,12(i2,1x))



deallocate (b)
deallocate (j)
deallocate (posmenor)
deallocate (posmaior)
write(*,32)
32 format (/t20,"Deseja calcular novamente?&
 Tecle S se	sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 17
  end if
write(6,46)
46 format(//t20,"Fim do programa de Descobrir maior e menor valor"/)
stop "Encerrado"
end program prob05
 



