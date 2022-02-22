program newton
    implicit none
    real::x(10),y(10,10),a,s,p,p_calc,fact
    integer::i,j,k,n

    open(98,file= "input.txt")
    read(98,*),n
    !read(98,*),(x(i),i=0,n-1)
    !read(98,*),(y(i,0),i=0,n-1)

    read(98,*),(x(i),i=0,n-1)
    read(98,*),(y(i,0),i=0,n-1)

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
    p = (a-x(0))/(x(1)-x(0))
    !Calculating the value
    s = y(0,0)+p*y(0,1)+p*(p-1)*y(0,2)/2.+p*(p-1)*(p-2)*y(0,3)/6.+p*(p-1)*(p-2)*(p-3)*y(0,4)/24.

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
