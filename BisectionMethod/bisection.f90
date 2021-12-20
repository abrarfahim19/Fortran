program bisection
    implicit none
    real :: x, f, a, b, c, tol

    tol = 0.0001

10  print *, 'Enter a and b. f(a) and f(b) must be of opposite sign'
    read *, a, b


    if (f(a)==0) then
        print *, 'the root is : ',a
    elseif (f(b)==0) then
        print *, 'the root is : ',b
    end if
20  c = (a+b)/2
    if (abs(a-b) < (tol)) then
        print *, 'the root is : ',b
    elseif (f(a)*f(b) > 0) then
        goto 10
    elseif (f(a)*f(c) < 0) then
        b = c
        goto 20
    else
        a = c
        goto 20
    end if


end program bisection

function f(x)
    implicit none
    real :: x, f
    f = 3*x**3 + 2*x**2 + 12
end function f
