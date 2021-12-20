program lu
    !real::L(3,3),U(3,3),mat(3,3),y(3),x(3),b(3)
    REAL, ALLOCATABLE :: L(:, :),U(:,:),mat(:,:),y(:),x(:),b(:)
    real::summ=0.0
    integer::i,j,k,n
    print*,'Enter number of equation: '
    read*,n
    ALLOCATE(L(n, n),U(n,n),mat(n,n),y(n),x(n),b(n))
    print*, 'enter coefficient and constant'
    read(*,*) ((mat(i,j),j=1,n),b(i),i=1,n)
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
    do i=1,n
    write( * , *) (mat(i,j),j=1,3)
    end do

    print*,x



end program
