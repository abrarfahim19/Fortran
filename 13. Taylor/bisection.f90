program bisection
    implicit none
    real::x,f,tol= 0.001,a = 0, b = 5,c

    do while(abs(a-b)>tol)
      c = (a+b)/2.
        if (f(a)*f(c)<0) then
            b = c
        else
            a = c
        end if
    end do

    print*,a


end program

real function f(x)
    implicit none
    real::x
    f = x**3 -x -1
end function
