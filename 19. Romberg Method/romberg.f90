program romberg
    implicit none
    real::x,a,b,R(100,100),h,sum1,f
    integer::i,j,k,n
    a = 0
    b = 1
    h = b-a
    n = 5

    R(1,1) = h*(f(a)+f(b))/2.

    do i = 2,n+1
        h = (b-a)/(2**(i-1))
        sum1 = 0

        do k = 1,2**(i-2)
            sum1 = sum1 + f(a+(2*k-1)*h)
        end do
        R(i,1) = R(i-1,1)/2. + sum1*h

        do j = 2,i
            R(i,j) = ((4**j)*R(i,j-1)-R(i-1,j-1))/(4**j-1)
        end do
    end do

    print*,R(n,n)
end program

real function f(x)
    real::x
    f = 1/(1+x**2)
end function
