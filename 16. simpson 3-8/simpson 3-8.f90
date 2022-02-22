program simpson3_8
    implicit none
    real:: x,y(100),total,h,a,b,sum3=0,sumAll=0,f
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
        if (mod(i,3)==0) then
            sum3 = sum3 + y(i)
        else
            sumAll = sumAll + y(i)
        end if
    end do

    total = (3*h)*(y(0)+2*sum3+3*sumAll+y(n))/8.
    print*,total
end program

real function f(x)
    implicit none
    real:: x
    f = 1./(1+x**2)
end function
