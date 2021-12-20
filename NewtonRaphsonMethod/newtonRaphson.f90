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

    do while (er > tol)
        x1 = x0 - f(x0)/fdif(x0)
        er = abs(x0-x1)
        x0 = x1
    end do

    write(*,20),x1
20  format('The value is',1x,f0.5)

end program newton_raphson

function f(x)
    implicit none
    real :: x, f
    f = cos(x)-x
end function f

function fdif(x)
    implicit none
    real :: x, fdif
    fdif = -sin(x)-1
end function fdif
