program test
    implicit none
    real:: f,xn, x0
    integer:: i
    x0 = 0.5
    do i = 1,10
        xn = f(x0)
        er = abs(xn-x0)
        if er < tol then
        stop
        end if
        x0 = xn
    end do

end program

function f(x)
    implicit none
    real:: x, f
    f = (x+10)**(0.25)
end function f
