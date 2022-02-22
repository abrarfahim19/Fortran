program newton_raphson
    implicit none
    real::x0, x, x1, f, fdif, tol, er

    er = 1000
    tol= 0.000001

    print*, 'Enter The initial Value'
    read*, x0

    if (f(x0)==0) then
        print*, 'The root is :', x0
    end if

    write(*,900)

    do while (er > tol)
        x1 = x0 - f(x0)/fdif(x0)
        er = abs(x0-x1)
        write(*,910)x0,x1
        x0 = x1
    end do

    write(*,20),x1
20  format('The value is',1x,f0.5)
900 Format(5x,'x0',13x,'xn')
910 Format(f8.3,8x,f8.3)
end program newton_raphson

function f(x)
    implicit none
    real :: x, f
    f = x**4 + x - 10
end function f

function fdif(x)
    implicit none
    real :: x, fdif
    fdif = 4*x**3 + 1
end function fdif
