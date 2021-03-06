program newtonForward
    implicit none
    real::x(10), y(6,6), s, a, p_calc, p
    integer::i, j, n, fact
    n = 6

    open(unit=10,file = 'x_value.txt')
    open(unit=11,file = 'y_value.txt')
    read(10,*),(x(i),i=0,n-1)
    read(11,*),(y(i,0),i=0,n-1)

    !calculating table
    do i = 1,n-1
        do j = 0,n-i-1
            y(j,i)=y(j+1,i-1)-y(j,i-1)
        end do
    end do

    !printing table
    print*,'The table is:'

    do i = 0,n-1
        write(*,50,advance='no'),x(i)
        do j = 0,n-i-1
            write(*,50,advance='no'),y(i,j)
        end do
        write(*,60)
    end do

    print*,'Enter a'
    read*,a

50  format(f8.3,2x)
60  format(/)

    s = y(0,0)
    p = (a-x(0))/(x(1)-x(0))

    do i=0,n-1
        s = s + (p_calc(p,i)*y(0,i))/fact(i)
    end do

    write(*,900)a,s
900 Format('The f(',f5.3,') = ',f8.5)

end program

function p_calc(p,n)
    implicit none
    real::p_calc,p
    integer::i,n
    p_calc = p
    do i = 1,n-1
        p_calc = p_calc*(p-i)
    end do
end function p_calc

function fact(n)
    implicit none
    integer::i,fact,n
    fact = 1
    do i = 2,n
        fact = fact*i
    end do

end function
