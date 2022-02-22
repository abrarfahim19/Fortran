program fixedPoint
    implicit none
    real::x0,xn,er=100,f,x,tol=0.000001,diff
    integer::i,it=100

    open(unit=1, file= 'input.txt')
    open(unit=2, file= 'output.txt')

    !The initial Value is taken
    read(1,*),x0

    if (abs(diff(x0))<1) then
    write(2,10)
10  Format(9x,'x0',16x,'xn')
        do i = 1, it
            xn = f(x0)
            er = abs(xn-x0)
            if (er<tol) then
                write(2,900),x0
                stop
            end if
            write(2,20),x0,xn
20          Format(5x,f8.5,10x,f8.5)
            x0 = xn
        end do
    else
        print*,'check your function'
    end if
900 Format('The root is: ',f9.5)

end program

function f(x)
    implicit none
    real:: x, f
    f = log(-2*cos(x) - 2**(-x) +6)
end function f

function diff(x)
    implicit none
    real:: x, diff
    diff = (0.69*2**(-x)+2*sin(x))/(-2*cos(x)-2**(-x)+6)
end function diff
