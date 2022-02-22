program newton_interpolation
    implicit none
    real::x(6),y(6,6),s,p,p_calc,fact,a
    integer::i,j,n=6

    open(unit=98,file="input_x.txt")
    open(unit=99,file="input_y.txt")

    read(98,*),(x(i),i=0,n-1)
    read(99,*),(y(i,0),i=0,n-1)

    do i= 1,n-1
        do j = 0,n-1-i
            y(j,i)=y(j+1,i-1)-y(j,i-1)
        end do
    end do

    do i = 0,n-1
        write(*,900,advance='no'),x(i)
        do j=0,n-i-1
            write(*,900,advance="no"),y(i,j)
        end do
        print*," "
    end do

    print*,"Enter the value of a: "
    read*,a
    !Calculating the value
    s = y(0,0)
    print*,s
    p = (a-x(0))/(x(1)-x(0))
    print*,p
    do i=0,n-1
        s = s + (p_calc(p,i)*y(0,i))/fact(i)
    end do

    print*,s
900 Format(f11.4,5x)
end program

real function p_calc(p,n)
    implicit none
    real::p
    integer::i,n
    p_calc = p
    do i=1,n-1
        p_calc = p_calc*(p-i)
    end do
end function

real function fact(n)
    implicit none
    integer:: n,i
    fact = 1
    do i = 2,n
        fact = fact*i
    end do
end function
