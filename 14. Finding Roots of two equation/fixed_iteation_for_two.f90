program real_roots
    implicit none
    real::x,y,x1,y1,A,B,C,D,f1,f2,j,k
    integer::i

    x = 5
    y = 5

    do i = 1,10
        j = A(x,y)*D(x,y) - B(x,y)*C(x,y)
        x = x - (D(x,y)*f1(x,y)-B(x,y)*f2(x,y))/j
        y = y - (-C(x,y)*f1(x,y)+A(x,y)*f2(x,y))/j
        print*,x,y

    end do

end program

real function f1(x,y)
    implicit none
    real::x,y
    f1 = x**2-y**2 -4
end function

real function f2(x,y)
    implicit none
    real::x,y
    f2 = x**2+y**2 -16
end function

real function A(x,y)
    implicit none
    real::x,y
    A = 2*x
end function

real function B(x,y)
    implicit none
    real::x,y
    B = -2*y
end function

real function C(x,y)
    implicit none
    real::x,y
    C = 2*x
end function

real function D(x,y)
    implicit none
    real::x,y
    D = 2*y
end function
