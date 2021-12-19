program newton_forward
    implicit none

    integer :: n, i, j, fact
    real :: x(20), y(20, 20), u_cal, u, a, s
    print *, 'Enter the value of n: '
    read *, n

    print *, 'Enter the values in form x, y: '
    do i = 0, n-1
        read *, x(i), y(i, 0)
    end do

    print *, 'Enter a: '
    read *, a

    ! calculating the forward difference table
    do i = 1, n-1
        do j = 0, n-i-1
            y(j, i) = y(j+1, i-1) - y(j, i-1)
            print*,y(j,i)
        end do
    end do

    s = y(0, 0)
    u = (a - x(0))/(x(1)-x(0))
    do i = 1, n-1
        s = s + (u_cal(u, i) * y(0, i))/fact(i)
        print *, s
    end do

    print *, 'Ans :', s

end program newton_forward

function u_cal(u, n)
    implicit none
    real :: u_cal, u
    integer :: n, i
    u_cal = u
    do i = 1, n-1
        u_cal = u_cal * (u - i)
    end do
end function u_cal

function fact(n)
    implicit none
    integer :: fact, n, i
    fact = 1
    do i = 2, n
        fact = fact * i
    end do
end function fact
