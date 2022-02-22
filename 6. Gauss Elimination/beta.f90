program gauss
    implicit none
    real::x(10),a(10,10),ratio,l(10,10)
    integer:: i,j,k,n

    open(98,file="input.txt")
    read(98,*),n
    read(98,*),((a(i,j),j=1,n+1),i=1,n)

    do i = 1,n-1
        if (a(i,i)==0) then
            print*,"error"
            stop
        end if
        do j = i+1,n
            ratio = a(j,i)/a(i,i)
            l(j,i) = ratio
            do k = 1,n+1
                a(j,k) = a(j,k) - ratio*a(i,k)
            end do
        end do
    end do

    x(n) = a(n,n+1)/a(n,n)
    do i = n-1,1,-1
        x(i) = a(i,n+1)
        do j = 1+i,n
            x(i) = x(i) - x(j)*a(i,j)
        end do
        x(i) = x(i)/a(i,i)
    end do

    print*,x(1),x(2),x(3)

    do i = i,n
        l(i,i) = 1
    end do

    do i = 1,n
        do j = 1,n
            write(*,10, advance="no"),l(i,j)
        end do
        print*,""
    end do
10  Format(f10.3)

end program
