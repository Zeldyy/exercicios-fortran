	  !Programa le matriz tridimensional e mostra dois conjuntos, um representando os numeros positivos dessa matriz e 
	  !o outro os negativos. zeros não serão considerados.

program prob13

!alocação de variáveis
implicit none
integer::i,m,j,v,n,l,k,u,p,q
real,allocatable::b(:,:,:)*8,c(:)*8,d(:)*8
character::resp


!Números de elementos do conjunto
write(6,11)
11 format(//t10,"Programa le matriz tridimensional e a partir dele"/t10,"cria dois conjuntos, um representando os numeros positivos"/t10,"e o outro os numeros negativos da matriz."/t10,&
"Zeros nao serao considerados neste programa."/)

98 v=1
1 do while (v==1)
write(6,10)
10 format(//t10,"Digite o numero de linhas M, colunas N, e largura L da matriz, maior ou igual a 2."/)
read(5,*,err=1)m,n,l
if ((n<=1).or.(m<=1).or.(l<=1)) then
	write(6,12)
	12 format(//t10,"O valor de M, N e L devem ser maior ou igual a 2."/)
else
	v=0
end if
end do

u=m*n*l
allocate(b(m,n,l))
allocate(c(u))
allocate(d(u))

!leitura da matriz.

write(6,14)
14 format(//t10," -Escreva os elementos da matriz- "/)

do k=1,l,1
	do i=1,m,1
		do j=1,n,1
			2 write(*,19)i,j,k
			19 format(/t2,"Elemento (",i2,",",i2,",",i2,"):"1x)
			read(5,*,err=2)b(i,j,k)
		end do
	end do
end do

!Criação dos conjuntos

p=0
q=0

do k=1,l,1
	do i=1,m,1
		do j=1,n,1
			if (b(i,j,k)>0) then
				p=p+1
				c(p)=b(i,j,k)
			end if
			if (b(i,j,k)<0) then
				q=q+1
				d(q)=b(i,j,k)
			end if
		end do
	end do
end do




!Imprimir resultado.

if (p==0) then
	write(*,40)
	40 format(//t10,"O conjunto de elementos positivos e vazio."/)
else
	write(*,41)
	41 format(//t10,"O conjunto de elementos positivos e :"/)
	write(*,42)(c(i),i=1,p,1)
	42 format(//t10,10(f6.2,1x))
end if


if (q==0) then
	write(*,43)
	43 format(//t10,"O conjunto de elementos negativos e vazio."/)
else
	write(*,44)
	44 format(//t10,"O conjunto de elementos negativos e :"/)
	write(*,45)(d(i),i=1,q,1)
	45 format(//t10,10(f6.2,1x))
end if

!fim

8 deallocate(b)
deallocate(c)
deallocate(d)

 write(*,32)
32 format (/t20,"Deseja calcular novamente?"/t20,"Tecle S se sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 98
  end if
write(6,46)
46 format(//t20,"Fim do programa de separacao de elementos."/)
stop "Encerrado"
end program prob13


