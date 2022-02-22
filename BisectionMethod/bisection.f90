program bisection
    implicit none
    real :: x, f, a, b, c, tol= 0.00001

    open(unit = 1, file= 'output.txt')
10  print *, 'Enter a and b. f(a) and f(b) must be of opposite sign'
    read *, a, b

    if (f(a)*f(b) > 0) then
        goto 10
    elseif (f(a)==0) then
        write(1,900),a
    elseif (f(b)==0) then
        write(1,900),b
    end if
    write(1,100)
100 Format(5x,'a',13x,'b',7x,'Selected Value')
20  c = (a+b)/2
    if (abs(a-b) < (tol)) then
        write(1,900),b
    elseif (f(a)*f(c) < 0) then
        b = c
        write(1,200)a,b
        goto 20
    else
        a = c
        write(1,300)a,b
        goto 20
    end if
200 Format(f8.3,7x,f8.3,9x,'a')
300 Format(f8.3,7x,f8.3,9x,'b')
900 Format('the root is : ',f9.5)
end program bisection

function f(x)
    implicit none
    real :: x, f
    f = x**4 + x - 10
end function f
