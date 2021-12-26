program lagrange
    implicit none

    integer :: n, i, j
    real:: a, s, term
    real,allocatable :: x(:), y(:)
    print *, 'Enter the value of n: '
    read *, n
    allocate(x(n),y(n))

    open(unit=10,file='x_value.txt')
    open(unit=11,file='y_value.txt')

    read(10,*)(x(i),i=1,n)
    read(11,*)(y(i),i=1,n)

    print *, 'Enter a: '
    read *, a

    !calculating Lagrange Interpolation
    s = 0
    do i = 1,n
        term = 1
        do j = 1,n
            if (i/=j) then
                term = term*(a-x(j))/(x(i)-x(j))
            end if
        end do
        s = s + term*y(i)
        print*, s
    end do

    print *, 'Ans :', s


end program lagrange
