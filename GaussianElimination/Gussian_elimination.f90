program Gaussian_elimination
    implicit none
    real,allocatable::a(:,:),b(:),c(:),x(:)
    real:: ratio
    integer:: i,j,k,n

    print*,'Enter Dimension'
    read*,n
    allocate(a(n,n),b(n),c(n),x(n))

    open(unit=10,file='a_value.txt')
    open(unit=11,file='b_value.txt')

    read(10,*)((a(i,j),j=1,n),i=1,n)
    read(11,*)(b(j),j=1,n)

    !convert to zeros
    do k=1,n-1
        do i=k+1,n
            ratio = a(i,k)/a(k,k)
            if (a(k,k)/=0) then
                b(i) = b(i)-b(k)*ratio
            else
                print*,'Enter Valid value'
                goto 100
            end if
            do j= 1,n
                a(i,j) = a(i,j) - a(k,j)*ratio
            end do
        end do
    end do

    !calculate x
    do i = n,1,-1
        do j = 1,n
            if (j/=i) then
                c(i) = c(i) + a(i,j)*x(j)
            else
                cycle
            end if
        end do
        x(i) = (b(i)-c(i))/a(i,i)
    end do

    do i = 1,n
        write(*,50)i,x(i)
    end do

100 stop
50  format('The value of---',/,'x(',i1,'):   ',f0.2,/)

end program
