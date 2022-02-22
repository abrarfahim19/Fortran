program gauss
    implicit none
    real::u(10,10),ratio,x(10),l(10,10),b(10),y(10),summ
    integer::i,j,k,n,v
    open(98,file= "luinput.txt")

    read(98,*) n
    read(98,*) ((u(i,j),j=1,n),i=1,n)
    read(98,*) (b(j),j=1,n)
    l = 0
    l(n,n)=1
    do i =1, n-1
        l(i,i)=1
        if(u(i,i).eq.0)then
            print*,"math error"
            stop
        endif

        do j = i+1,n
            ratio = u(j,i)/u(i,i)
            l(j,i)=ratio
            do k=1,n
                u(j,k) = u(j,k)-ratio*u(i,k)
            end do
        end do
    end do

    !forward substitution
    y(1) = b(1)
    do i =2,n
        y(i) = b(i)
        do j = 1,i-1
            y(i) = y(i) - l(i,j)*y(j)
        end do

    end do



    !back substitution

    x(n) = y(n)/u(n,n)
    do i = n-1,1,-1
        x(i) = y(i)
        do j = i+1 ,n
            x(i) = x(i) - u(i,j)*x(j)
        end do
        x(i) = x(i)/u(i,i)
    end do


    !display solution
    do i=1,n
        print*,(l(i,j),j=1,n)
    end do
    print*,"MUSA"


    do i=1,n
        print*,(u(i,j),j=1,n)
    end do
    print*,"MUSA"


    do i=1,n
        print*,x(i)
    end do
end program
