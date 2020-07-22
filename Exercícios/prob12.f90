!Programa verifica se a soma dos 2 primeiros termos da linha de uma matriz é igual
! à diferença do ultimo menos o penultimo termo dessa linha. Verificar se isso ocorre ou nao em todas as linhas.

program prob12

!alocação de variáveis
implicit none
integer::i,m,j,v,cor,n
real,allocatable::b(:,:)*8
character::resp


!Números de elementos do conjunto
write(6,11)
11 format(//t10,"Programa verifica se a soma dos 2 primeiros termos da linha"/t10,"de uma matriz e igual a diferenca"/t10,"do ultimo menos o penultimo termo dessa linha."/t10,&
"Este programa verifica se isso ocorre ou nao em todas as linhas."/)

98 v=1
1 do while (v==1)
write(6,10)
10 format(//t10,"Digite o numero de linhas M e colunas N da matriz, maior ou igual a 6."/)
read(5,*,err=1)m,n
if ((n<=5).or.(m<=5)) then
	write(6,12)
	12 format(//t10,"O valor de M e N deve ser maior ou igual a 6."/)
else
	v=0
end if
end do

allocate(b(m,n))

!leitura da matriz.

write(6,14)
14 format(//t12,"-Escreva os elementos da matriz- "/)

do i=1,m,1
	do j=1,n,1
		2 write(*,19)i,j
		19 format(/t10,"Elemento (",i2,",",i2,")"/)
		read(5,*,err=2)b(i,j)
	end do
end do

!verificação.
cor=1
do  i=1,m,1
if ((b(i,1)+b(i,2))==(b(i,n)-b(i,n-1))) then

else
	cor=0
	exit
end if
end do

!Imprimir resultado.

select case(cor)

case(1)
	write(*,9)
9	format(//t10,"A soma dos 2 primeiros termos das linhas e igual"/t10,"a diferença do ultimo menos o penultimo termo da respectiva linha."/)
case(0)
	write(*,23)
23	format(//t10,"A soma dos 2 primeiros termos das linhas NAO e igual"/t10,"a diferença do ultimo menos o penultimo termo da respectiva linha."/)

end select

!fim

deallocate(b)


write(*,32)
32 format (/t20,"Deseja calcular novamente?"/t20,"Tecle S se sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 98
  end if
write(6,46)
46 format(//t20,"Fim do programa de verificacao de matriz."/)
stop "Encerrado"
end program prob12