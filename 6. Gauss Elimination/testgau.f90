program gauss
    implicit none
    real::a(3,4),ratio,x(10)
    integer::i,j,k,n
    open(98,file= "input.txt")

    read(98,*),n
    read(98,*),((a(i,j),j=1,n+1),i=1,n)

    do i =1, n-1
        if(a(i,i)==0)then
            print*,"math error"
            stop
        endif

        do j = i+1,n
            ratio = a(j,i)/a(i,i)
            do k=1,n+1
                a(j,k) = a(j,k)-ratio*a(i,k)
            end do
        end do
    end do

    !back substitution
    do i = 1,n
        do j = 1,n+1
            write(*,10, advance = "no"),a(i,j)
        end do
        print*,""
    end do

10  Format(f10.3)

    x(n) = a(n,n+1)/a(n,n)
    do i = n-1,1,-1
        x(i) = a(i,n+1)
        do j = i+1 ,n
            x(i) = x(i) - a(i,j)*x(j)
        end do
        x(i) = x(i)/a(i,i)
    end do

    !display solution
    do i=1,n
        print*,x(i)
    end do
end program
