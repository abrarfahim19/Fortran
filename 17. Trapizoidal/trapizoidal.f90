program trapizoidal
    implicit none
    real:: x,f,a,b,h,sumAll=0,total,y(100)
    integer::i,n

    a = 0
    b = 1
    n = 20
    h = abs(a-b)/n

    x = a
    y(0) = f(a)
    y(n) = f(b)

    do i = 1,n-1
        x = x + h
        y(i) = f(x)
        sumAll = sumAll + y(i)
    end do

    total = h/2.0*(y(0)+ 2*sumAll + y(n))
    print*,total

end program

real function f(x)
    implicit none
    real::x
    f = 1/sqrt(1+x**2)
end function
