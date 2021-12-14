program powerMethod
    implicit none
    real:: tol
    real,allocatable :: A(:,:), x(:,:), y(:,:), eig(:,:)
    integer:: i, j, k, n, it

    tol = 0.001

    print*, 'Enter The Matrix Dimension : '
    read*, n
    print*, 'Enter The Number of iteration : '
    read*, it

    ALLOCATE(a(n, n),x(n,1),y(n,1),eig(it,1))

    print*, 'Enter A matrix:'
    read*,((A(i,j),j=1,n),i=1,n)
    print*, 'Enter x matrix:'
    read*,(x(i,1),i=1,n)

    write(*,30)A
30  Format(f0.2,5x,f0.2,5x,f0.2)
    write(*,40)x
40  Format(f0.2)

    do k = 1, it
        do i = 1,n
            y(i,1) = 0
            do j = 1,n
                y(i,1)=y(i,1)+A(i,j)*x(j,1)
            end do
        end do
        maxy = 0
        maxx = 0
        do i = 1,n
            if (maxy<y(i,1)) then
                maxy = y(i,1)
            end if
            if (maxx<x(i,1)) then
                maxx = x(i,1)
            end if
        end do
        eig(k,1) = maxy/maxx
        if
    end do

end program powerMethod

