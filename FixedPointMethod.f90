program fixedPoint
    implicit none
    real:: x0, xn, er, f1, x, tol, dff,df1,f2,df2,f3,df3,f4,df4
    integer:: i , it,k
    open(29,file ="fixed.dat")
    open(27,file ="inputfix.txt")
    it = 100
    tol = 0.001
    er = 100
    do k = 1,4
    write(29,*),'----------iteration----------'
    read(27,*) x0
    if (k.eq.1) then
        dff = df1(x)
    elseif (k==2) then
        dff = df2(x)
    elseif (k==3) then
        dff = df3(x)
    else
        dff = df4(x)
    end if
    ! dff  = diff(x)
    if (abs(dff)<1) then
    write(29,10)
10  Format(9x,'x0',16x,'xn')
        do i = 1, it
            if (k.eq.1) then
                xn = f1(x0)
            elseif (k==2) then
                xn = f2(x0)
            elseif (k==3) then
                xn = f3(x0)
            else
                xn = f4(x0)
            end if
            er = abs(xn-x0)
            if (er<tol) then
                print*,'The root is: ',x0
                write(29,*), 'The root is:',x0
                exit
            end if
            write(29,20),x0,xn
20          Format(5x,f8.5,10x,f8.5)
            x0 = xn
        end do
    else
        print*, 'Please check your function'
    end if
end do
end program

function f1(x)
    implicit none
    real:: x, f1
    f1 = (x+10)**(0.25)
    !f = x**4 - x + 10
end function f1
function df1(x)
    implicit none
    real:: x, df1
    df1 = 0.25*((x+10)**(0.25-1))
    !diff = 4*(x**3) -1
end function df1
function f2(x)
    implicit none
    real:: x, f2
    !f2 = acos(x)
    f2=cos(x)
    !f = x**4 - x + 10
end function f2
function df2(x)
    implicit none
    real:: x, df2
    !df2 = - 1/(sqrt(1-x**2))
    df2=-sin(x)
    !diff = 4*(x**3) -1
end function df2
function f3(x)
    implicit none
    real:: x, f3
    f3 = (x+1)**(0.3333)
    !f = x**4 - x + 10
end function f3
function df3(x)
    implicit none
    real:: x, df3
    df3 = 0.33*((x+1)**(0.333-1))
    !diff = 4*(x**3) -1
end function df3
function f4(x)
    implicit none
    real:: x, f4
    !f4 = (x+10)**(0.25)
    f4 = log(-2*cos(x) - 2**(-x) +6)
    !f = x**4 - x + 10
end function f4
function df4(x)
    implicit none
    real:: x, df4
    !df4 = 0.25*((x+10)**(0.25-1))
    df4 = (0.69*2**(-x)+2*sin(x))/(-2*cos(x)-2**(-x)+6)
    !diff = 4*(x**3) -1
end function df4
