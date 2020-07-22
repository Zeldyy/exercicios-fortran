!Programa que transforma os elementos pares de um conjunto em seu predecessor ímpar.

program prob04

!alocação de variáveis
implicit none
integer::i,n
real,allocatable::b(:)*8
real::v,c
character::resp


!Números de elementos do conjunto
write(6,11)
11 format(//t10,"Programa que transforma os elementos&
 pares de um conjunto em seu predecessor impar. "/)

98 v=1
1 do while (v==1)
write(6,10)
10 format(//t10,"Digite a quantidade de elementos, numero par inteiro positivo."/)
read(5,*,err=1)n
if ((mod(n,2)/=0).or.(n<=1)) then
	write(6,12)
	12 format(//t10,"O valor deve ser maior que 1 e par."/)
else
	v=0
end if
end do

allocate(b(n))

!leitura do conjunto.

2 write(6,14)
14 format(//t10,"Escreva os elementos do conjunto"/)
	read(5,*,err=2)(b(i),i=1,n,1)

!trocar o valor dos elementos pares pelos predecessores de posição impar

do i=2,n,2
  v=b(i)
  c=b(i-1)
  b(i-1)=v
  b(i)=c
end do

!Imprimir o conjunto
write(6,15)
15 format(//,t10,"Os elementos do conjunto sao: "/)
write(6,16)(b(i),i=1,n,1)
16 format(//t5,12(f7.2,1x))
deallocate(b)

write(*,32)
32 format (/t20,"Deseja calcular novamente?&
 Tecle S se	sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 98
  end if
write(6,46)
46 format(//t20,"Fim do programa de Troca de Posições"/)
stop "Encerrado"
end program prob04
 



