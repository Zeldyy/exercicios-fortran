program prob25
implicit none
!programa le matriz retangular e multiplica pela sua transposta. >=2

!variaveis
character::resp
integer::v,i,n,m,j,k,l
real,allocatable::a(:,:),b(:,:),c(:,:)
real::s


write(*,1)
   1 format(/t10,"Programa le matriz retangular e multiplica pela sua transposta."/)

!tamanho da matriz.
77 v=1
do while (v==1)
   write(*,2)
   2 format(/t10,"Digite o numero de linhas e numero de colunas.(maior ou igual a 2)"/)
   read(*,*,err=3)m,n
   if ((m<2).or.(n<2)) then
		write(*,4)
		4 format(/t10,"Digite numeros maiores ou iguais a 2."/)
	else
		exit
	end if
	3 write(*,20)
    20 format(/t10,"Digite numeros inteiros."/)
end do

allocate(a(m,n))
allocate(b(n,m))
allocate(c(m,m))
!Ler matriz
write(*,5)
   5 format(/t10,"Digite agora os valores da matriz, de linha em linha"/)
do i=1,m,1
	do j=1,n,1
		7 write(*,6)i,j
		6 format(/t5,"Digite valor de A(",i2,",",i2,"):"/)
		  read(*,*,err=7)a(i,j)
	end do
end do

!Transposta de A.
  !a b 	   a d g   =a^2+b^2+c^2	a(1,1)
  !d e 	   b e h
 ! g h
 
do i=1,m,1
	do j=1,n,1
		b(j,i)=a(i,j)
	end do
end do


!Multiplicação.
 k=1
do i=1,m,1
	
	do l=1,m,1
		s=0
		do j=1,n,1
			s = s + a(k,j)*b(j,l)
 		end do
		c(i,l)=s
	end do
	k=k+1
end do 

!imprimir.

write(*,10)
10 format(/t10,"O produto das matrizes e:"//)
write(*,12)
12 format(/t13,"-------------------------------"/)
do i=1,m,1
	write(*,11) (c(i,j),j=1,m,1)
	11 format (/t10,5(3x,f5.1)//)
end do
write(*,122)
122 format(/t13,"-------------------------------"/)

!fim.

deallocate(a)
deallocate(b)
deallocate(c)

write(*,13)
13 format(/t10,"Deseja calcular novamente?"/t10,"Digite S se sim ou qualquer outra tecla se nao."/)
read(*,*)resp
if ((resp=="s").or.(resp=="S"))	go to 77

stop "Encerrado."
end program prob25
  


