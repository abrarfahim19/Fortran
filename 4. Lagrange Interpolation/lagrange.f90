program lagrange
    implicit none
    real::x(6),y(6),term,a,s
    integer::i,j,n=6

    open(unit=98,file="input_x.txt")
    open(unit=99,file="input_y.txt")

    read(98,*),(x(i),i=1,n-1)
    read(99,*),(y(i),i=1,n-1)

    print*,"Enter the a: "
    read*,a
    s=0
    do i = 1,n
        term=1
        do j = 1,n
            if (i/=j) then
                term = (term*(a-x(j)))/(x(i)-x(j))
            end if
        end do
            s = s + term*y(i)
    end do

    print*,s

end program
