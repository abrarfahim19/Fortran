program fixedPoint
    implicit none
    real:: x0, xn, er, f, x, tol
    integer:: i , it

    it = 10
    tol = 0.001
    er = 100

    print*, 'Enter the initial value: '
    read*,x0
    write(*,10)
10  Format(7x,'x0',14x,'xn')
    do i = 1, it
        xn = f(x0)
        er = abs(xn-x0)
        if (er<tol) then
            print*,'The root is: ',x0
            stop
        end if
        write(*,20),x0,xn
20      Format(5x,f0.5,10x,f0.5)
        x0 = xn
    end do


end program

function f(x)
    implicit none
    real:: x, f
    f = (x+1)**(0.5)
end function f
