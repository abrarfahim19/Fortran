program runge4th
    implicit none
    real::x,y,a,b,h,f,k1,k2,k3,k4
    integer::n,i

    a = 0
    b = 0.4
    h = 0.1
    n = abs(a-b)/h

    x = a
    y = 1

    do i = 1,n
        k1 = h*f(x,y)
        k2 = h*f(x+h/2.0,y+k1/2.0)
        k3 = h*f(x+h/2.0,y+k2/2.0)
        k4 = h*f(x+h,y+k3)

        y = y + (k1+2*k2+2*k3+k4)/6
        x = x + h
    end do

    print*,y
end program

real function f(x,y)
    implicit none
    real::x,y

    f = (x**2 + y**2)/10
end function
