program lu
    !real::L(3,3),U(3,3),mat(3,3),y(3),x(3),b(3)
    REAL, ALLOCATABLE :: L(:, :),U(:,:),mat(:,:),y(:),x(:),b(:)
    real::summ=0.0
    integer::i,j,k,n
    print*,'Enter number of equation: '
    read*,n
    ALLOCATE(L(n, n),U(n,n),mat(n,n),y(n),x(n),b(n))

    open(unit = 10, file='a_value.txt')
    open(unit = 11, file='b_value.txt')

    read(10,*),((mat(i,j),j= 1,n),i=1,n)
    read(11,*),(b(i),i=1,n)

    L = 0
    U = 0

    !LU factorization

    do i=1,n
        do k = i,n
            summ =0
            do j = 1,i
                summ = summ + L(i,j)*U(j,k)
            end do
            U(i,k) = mat(i,k) - summ
        end do
        do k = i,n
            if (i.eq.k) then
                L(i,i)=1
            else
                summ = 0
                do j=1,i
                    summ = summ + L(k,j)*U(j,i)
                end do
                L(k,i) = int((mat(k,i) - summ)/U(i,i))
            end if
        end do
    end do


    !calculate the solution

    y(1) = b(1)
    do i =2,n
        summ = 0
        do j = 1,n-1
            summ = summ + L(i,j)*y(j)
        end do
        y(i) = b(i) -summ
    end do
    x(n) = y(n)/U(n,n)
    do i=n-1,1,-1
        summ =0
        do j = i+1,n
            summ = summ + U(i,j)*x(j)
        end do
        x(i) = (y(i)-summ)/U(i,i)
    end do

    print*,'The Lower Matrix is :'
    write(*,900)L

    print*,'The Upper Matrix is :'
    write(*,900)U

    print*,'The solution Matrix is :'
    write(*,900)x


900 Format(3f10.2)

end program
