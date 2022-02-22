program taylor
    implicit none
    real::x,y,h,dy,d2y,d3y,d4y,t=0.2
    integer::i,n

    x= 0
    y = 1
    dy = 0

    h = 0.1
    n = abs(x-t)/h

    do i = 1,n
        y = y + h*dy + ((h**2)/2.)*d2y(x,y,dy) + ((h**3)/6.)*d3y(x,y,dy) + ((h**4)/24.)*d4y(x,y,dy)
        dy = dy + h*d2y(x,y,dy) + ((h**2)/2.)*d3y(x,y,dy) + ((h**3)/6.)*d4y(x,y,dy)
        x = x + h
    end do
    print*,y

end program

real function d2y(x,y,dy)
    implicit none
    real::x,y,dy
    d2y = x*dy**2 - y**2
end function


real function d3y(x,y,dy)
    implicit none
    real::x,y,dy,d2y
    d3y = 2*x*dy*d2y(x,y,dy) + dy**2 -2*y*dy
end function

real function d4y(x,y,dy)
    implicit none
    real::x,y,dy,d2y,d3y
    d4y = 4*dy*d2y(x,y,dy) + 2*x*(d2y(x,y,dy))**2 + 2*x*dy*d3y(x,y,dy) - 2*dy**2 - 2*y*d2y(x,y,dy)
end function
