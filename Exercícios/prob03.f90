!Problema 3- Ler um número n (inteiro, maior que 2)
!Gerar os n primeiros elementos da sequência Fibonacci.
program prob03

!Declaração das variáveis.

implicit none
integer:: n,i
real, allocatable::a(:)*8
real::s*8
character::resp

! 1a Parte - Ler e validar n.

99 write(*,98)
98 format(/t5," Programa que gera numeros da sequencia de Fibonacci."/)

10 Write(6,21)
21 format(//t10,"Entre com o numero de elementos da sequencia n, inteiro e maior que 2."/)
read(5,*,err=10)n
if(n<3)go to 10
!
!  2a Parte - Gerar os elementos da sequência Fibonacci
!
allocate (a(n))
a(1)=0
a(2)=1
s=1
do i=3,n,1
a(i)=a(i-1)+a(i-2)
s=s+a(i)
end do
!
!  3a Parte - Imprimir a sequência





if (n<=55) then
	write(6,27)
	27 format(//t5,"Os elementos da sequencia Fibonacci sao: "/)
	write(6,14) (a(i),i=1,n,1)
	14 format(/t1,5(f15.0))
	write(6,26)s
26 format(//t5,"Soma dos numeros da Sequencia Fibonacci:"/,f15.0,/)
else
	write(6,270)
	270 format(//t5,"Os elementos da sequencia Fibonacci sao: "/)
	write(6,140) (a(i),i=1,n,1)
	140 format(/t1,5(es12.4))
	write(6,260)s
260 format(//t5,"Soma dos numeros da Sequencia Fibonacci:"/,es30.8,/)


end if



!
!  4a Parte - Encerramento do Programa
deallocate(a)

write(*,32)
32 format (/t20,"Deseja calcular novamente?&
 Tecle S se	sim ou qualquer outra tecla se nao"//)
 read(*,*)resp
 if (resp=="s".or.resp=="S") then
  go to 10
  end if
write(6,46)
46 format(//t20,"Fim do programa de imprimir a sequencia de Fibonacci."/)
stop "Encerrado"
end program prob03
