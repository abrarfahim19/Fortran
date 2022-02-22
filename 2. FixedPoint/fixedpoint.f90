program fixed_point
    implicit none
    real::f,df,x0,x,tol=0.001,er=10
    integer::it

    read*,x0

    if (df(x0)<1) then
        goto 10
        else
            print*,"Change the function"
            stop
    end if

10  write(*,900),x0
    write(*,910)
    write(*,920)

    do while (er>tol)
        x = f(x0)
        write(*,930),x0,x
        er = abs(x-x0)
        x0 = x
    end do
    print*,"The solution is: "
    write(*,940),x

900 Format("The initial Value is : ",f5.3)
910 Format(5x,"x",11x,"f(x)")
920 Format("================================")
930 Format(f8.3,5x,f8.3)
940 Format(f8.3)


end program

real function f(x)
    implicit none
    real::x
    f = (x+1)**(1/3.)
end function

real function df(x)
    implicit none
    real::x
    !df = (1/(6-2**(-x)-2*cos(x)))*(log(2.0)*2**(-x)+2*sin(x))
    df = (1/3.)*(x+1)**(1/3.-1)
end function
