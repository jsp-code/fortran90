implicit none
integer a,i
integer b(7)
a=20
b(1) = 4
b(2) = 1
b(3) = 3
b(4) = 8
b(5) = 15
b(6) = 10
b(7) = 51
open(a,file='valores.dat')
do i =1,7
    write(a,*) b(i)
end do
close(a)
end
