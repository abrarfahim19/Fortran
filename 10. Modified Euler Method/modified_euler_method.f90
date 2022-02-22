program modified_euler
    implicit none
    real::x,y,a,b,h,f
    integer::i,n

    a = 0
    b = 0.2
    n = 10
    h = abs(a-b)/n

    x = a
    y = 1

    do i = 1,n
        y = y + h*f(x+(h/2),y+(h/2)*f(x,y))
        x = x + h
    end do

    print*,y

end program

real function f(x,y)
    implicit none
    real::x,y

    f = log(x+y)
end function
