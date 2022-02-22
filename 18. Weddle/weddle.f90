program weddle
    implicit none
    real::x,f,y0,y(100),a,b,h, total=0, sum6=0, sum2=0, sumAll=0, sum3=0
    integer::i,n

    a = 0
    b = 0.5
    n = 100
    h = abs(a-b)/n

    x = a
    y0 = f(a)

    do i = 1,n
        x = x + h
        y(i) = f(x)
        if (mod(i,6)==0) then
            sum6 = sum6 + y(i)
        else if (mod(i,3)==0) then
            sum3 = sum3 + y(i)
        else if (mod(i,2)==0) then
            sum2 = sum2 + y(i)
        else
            sumAll = sumAll + y(i)
        end if
    end do

    total = (3*h)*(y0 + sum6*2 + sum3*6 + sum2 + 5*sumAll)/10.
    print*, total

end program

real function f(x)
    implicit none
    real::x
    f = 1./(sqrt(1+x**2))
end function
