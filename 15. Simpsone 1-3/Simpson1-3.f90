program simpson1_3
    implicit none
    real:: x,total,f,a,b,h,sum1=0,sum2=0,y(100)
    integer::i,n

    a = 0
    b = 1
    n = 20
    h = abs(a-b)/n

    x = a
    y(0) = f(x)
    y(n) = f(b)

    do i = 1,n-1
        x = x + h
        y(i) = f(x)
        if (mod(i,2)==0) then
            sum2 = sum2 + y(i)
        else
            sum1 = sum1 + y(i)
        end if
    end do

    total = h/3.*(y(0)+y(n)+4*sum1+2*sum2)

    print*,total
end program

real function f(x)
    implicit none
    real:: x
    f = 1/(1+x**2)
end function
