program bisection
    implicit none
    real::a,b,c,er,tol=0.001,f
    integer::i

10  read*,a,b
    if (f(a)*f(b)<0) then
        write(*,900)
        write(*,910)
        do while (abs(a-b)>tol)
            c=(a+b)/2
            write(*,920),a,b,c
            if (f(a)*f(c)<0) then
                b = c
            else
                a = c
            end if
        end do
        else
            goto 10
    end if
    write(*,930),c

900 Format(5x,"a",12x,"b",12x,"c")
910 Format('====================================')
920 Format(f8.3,5x,f8.3,5x,f8.3)
930 Format("The root is: ",f8.3)
end program

real function f(x)
    implicit none
    real:: x
    f = x**4-x-10
end function f
