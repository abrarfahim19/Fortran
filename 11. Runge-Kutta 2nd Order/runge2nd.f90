program runge2nd
    implicit none
    real::a,b,x,y,h,f,k1,k2
    integer::i,n

    a = 0
    b = 0.4
    h = 0.1
    n = abs(a-b)/h

    x = a
    y = 1

    do i = 1,n
        k1 = h*f(x,y)
        k2 = h*f(x+h/2,y+k1/2)
        y = y + k2
        x = x + h
    end do

    print*, y
end program

real function f(x,y)
    implicit none
    real::x,y
    f = (x**2 + y**2)/10
end function
