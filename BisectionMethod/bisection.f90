program bisection
    implicit none
    real :: x, f, a, b, c, tol

    tol = 0.0001

10  print *, 'Enter a and b. f(a) and f(b) must be of opposite sign'
    read *, a, b


    if (f(a)*f(b) > 0) then
        goto 10
    elseif (f(a)==0) then
        print *, 'the root is : ',a
    elseif (f(b)==0) then
        print *, 'the root is : ',b
    end if

    write(*,100)
100 Format(5x,'a',13x,'b',7x,'Selected Value')

20  c = (a+b)/2
    if (abs(a-b) < (tol)) then
        print *, 'the root is : ',b
    elseif (f(a)*f(c) < 0) then
        b = c
        write(*,200)a,b
        goto 20
    else
        a = c
        write(*,300)a,b
        goto 20
    end if

200 Format(f8.3,7x,f8.3,9x,'a')
300 Format(f8.3,7x,f8.3,9x,'b')

end program bisection

function f(x)
    implicit none
    real :: x, f
    f = 3*x**3 + 2*x**2 + 12
end function f
