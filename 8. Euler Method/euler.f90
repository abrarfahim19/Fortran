program euler_method
    implicit none
    real::f,x,y,a,b,h
    integer::i,n

    a = 0
    b = 0.2
    n = 10
    h = abs(a-b)/n
    x = a
    y = 1

    do i = 1,n
        y = y + h*f(x,y)
        x = x + h
    end do

    print*,y
end program

real function f(x,y)
    implicit none
    real::x,y

    f= log(x+y)
end function
