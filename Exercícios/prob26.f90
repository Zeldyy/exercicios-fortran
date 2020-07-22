  !Este programa le um numero inteiro e gera todas as permutações de 1 a n,
  ! e informa o numero de inversões que ocorre.
program prob26
implicit none
integer(kind=4)::n,v,i,fato,j,inv,s,num,ne,ord,rep,q
real(8)::cont
character::resp

integer(kind=4),allocatable::a(:),b(:),c(:)

!validação de n.					

write(*,88)
88 format(/t10,"Este programa le numero inteiro,"/t10,"gera todas as permutacoes de 1 a n"/t10,"e informa o numero de inversoes que ocorre."/)

8 v=1
do while (v==1)
	write(*,1)
	1 format(/t10,"Digite o valor de n."/)
read(*,*,err=2)n
if ((n<=1).or.(n>9)) then
else
exit
end if
2 write(*,3)
3 format(/t10,"Por favor digite um numero inteiro maior que 1 e menor que 10."/)
end do

allocate(a(n))
allocate(b(n))
allocate(c(n))

fato=fat(n)
!definição de numero.
 num=0
 do i=1,n,1
	c(i)=i
	a(i)=i
	num= a(i)*10**(n-i)+num
 end do

 


!impressão de elementos.
inv=0
cont=1
write(*,80)n
80 format(/t10,"As permutacoes de 1 a ",i1," sao:"/)
write(*,81)
81 format(/t10,"--------------------------------------------------------------"/)
write(*,55)num,inv

do while (cont/=fato)
	a(n) = a(n)+9
	do i=n,2,-1
		if (a(i)>=10) then
			a(i-1)=a(i-1)+1
			a(i)= a(i)-10
		else
			exit
		end if
	end do

	s=1
	do i=1,n,1
		s=a(i)*s
	end do

	if (s==fato) then
	!ordenar conjunto.
		do i=1,n,1
			b(i)=a(i)
		end do
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

	!Verificar se existe numero repetido.
		rep=0
		do i=1,n,1
				if (b(i)/=c(i)) rep=1
		end do



	
		inv=0
		do i=1,n-1,1
			do j=2,n,1
				if (a(i)>a(j)) inv=inv+1
			end do
		end do
	else
		cycle
	end if
	if (rep==0) then
	   num=0
		do i=1,n,1
			num= a(i)*10**(n-i)+num
		end do

	   write(*,55)num,inv
	   55 format(t5,i9," com ",i2," inversoes.")
	   cont=cont+1
	end if

end do
write(*,81)


deallocate(a)
deallocate(b)
deallocate(c)

write(*,13)
13 format(/t10,"Deseja calcular novamente?"/t10,"Digite S se sim ou qualquer outra tecla se nao."/)
read(*,*)resp
if ((resp=="s").or.(resp=="S"))	go to 8

 contains

!função que calcula fatorial de um número.
real(kind=8) function fat(a)
implicit none
real(kind=8)::fatorial
integer::a
!calculo de n
if (n==0) then
fat=1
else
fatorial=1
do i=1,a,1
	fatorial=fatorial*i
end do
fat=fatorial
end if
end function fat
		
end program prob26