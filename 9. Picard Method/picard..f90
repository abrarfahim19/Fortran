program picard_method
    implicit none
    real::x,x0,y,y0,f,tol=0.1,er=10
    integer :: i
    x0 = 0
    x  = 0.2
    y0 = 1
    do while (er>tol)
        y = y0 + f(x,y0)
        er = abs(y-y0)
        y0 = y
    end do

    print*,y

end program

real function f(x,y)
    implicit none
    real::x,y
    f =(x+y)*log(x+y)-x-y*log(y)
end function
