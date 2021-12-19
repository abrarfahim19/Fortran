program lagrange
    implicit none

    integer :: n, i, j
    real :: x(20), y(20), a, s, term
    print *, 'Enter the value of n: '
    read *, n

    print *, 'Enter the values in form x, y: '
    do i = 0, n-1
        read *, x(i), y(i)
    end do

    print *, 'Enter a: '
    read *, a

    s = 0
    do i = 0, n-1
        term = y(i)
        do j = 0, n-1
            if (j /= i) then
                term = term * (a - x(j))/(x(i) - x(j))
            end if
        end do
        s = s + term
    end do

    print *, 'Ans :', s


end program lagrange
