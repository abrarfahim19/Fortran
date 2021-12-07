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
    if (abs(a-b).lt.(tol)) then
        print *, 'the root is : ',b
    elseif (f(a)*f(b).ge.0) then
        goto 10
    elseif (f(a)*f(c).lt.0) then
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
    f = x**2 + 3*x
end function f
