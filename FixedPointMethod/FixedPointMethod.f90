program fixedPoint
    implicit none
    real:: x0, xn, er, f, x, tol, diff
    integer:: i , it

    it = 100
    tol = 0.001
    er = 100

    print*, 'Enter the initial value: '
    read*,x0

    if (abs(diff(x))<1) then
    write(*,10)
10  Format(9x,'x0',16x,'xn')
        do i = 1, it
            xn = f(x0)
            er = abs(xn-x0)
            if (er<tol) then
                print*,'The root is: ',x0
                stop
            end if
            write(*,20),x0,xn
20          Format(5x,f8.5,10x,f8.5)
            x0 = xn
        end do
    else
        print*, 'Please check your function'
    end if

end program

function f(x)
    implicit none
    real:: x, f
    f = (x+10)**(0.25)
    !f = x**4 - x + 10
end function f

function diff(x)
    implicit none
    real:: x, diff
    diff = 0.25*((x+10)**(0.25-1))
    !diff = 4*(x**3) -1
end function diff
