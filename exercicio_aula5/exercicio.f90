implicit none
integer N,a,status,aux
real metadeN
integer infinito
integer i,j
real total
integer , allocatable :: ordem(:),desordenado(:)
!encontrar o número de entradas
N = 0
a = 100
open(a,file="valores.dat",status="old")
do
    read(a,*,IOSTAT=status) aux
    if(status<0) exit
    N=N+1
end do
close(a)
!criar um array para receber os dados
allocate(desordenado(N))
open(a,file="valores.dat",status="old")
total =0.0
i=1
do i=1,N
        read(a,*) desordenado(i)
        total = total + desordenado(i)
end do
close(a)
!ordernar Os elementos
allocate(ordem(N))
infinito = 2*(10**9)
i=1
do i =1,N
   ordem(i) = infinito !normalizando os elementos
end do
i=1
j=1
do
    if (i==1) then
        if(desordenado(j) <= ordem(i)) then
            ordem(i) = desordenado(j)
        end if
        if (j==N) then
            i= i+1
            j= 1
       else
           j=j+1
       end if
    else
        if(desordenado(j) <= ordem(i)) then
            if(desordenado(j)>ordem(i-1))then
                ordem(i) = desordenado(j)
            endif
        end if
        if (j==N) then
             i= i+1
             j= 1
        else
            j=j+1
        end if
    end if
    if(i>N) exit
end do
!exibir resultados
print*,"O número de Linhas é:"
print*,N
print*,"O maior inteiro é"
print*,ordem(N)
Print*,"O menor inteiro é"
print*,ordem(1)
Print*,"A mediana é"
metadeN = N/2
print*,ordem(ceiling(metadeN)+1)
print*,"e por fim, a média é:"
total = total/N
print*,total
end
