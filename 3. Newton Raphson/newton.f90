program newton_raphson
    implicit none
    real::x,x0,tol=0.001, er = 100,f,df

    read*,x0
    write(*,900),x0
    write(*,910)

    do while (er>tol)
        x = x0 - f(x0)/df(x0)
        er = abs(x0-x)
        write(*,920),x0,x
        x0 = x
    end do

    write(*,930),x0

900 Format("The initial Value is: ", f8.3)
910 Format(5x,"x0",5x,"x",/,"============================")
920 Format(f8.3,f8.3)
930 Format(/,"The Solution is : ",f8.3)

end program

real function f(x)
    implicit none
    real::x
    f = cos(x)-x

end function

real function df(x)
    implicit none
    real::x
    df = -sin(x)-1
end function
