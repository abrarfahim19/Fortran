program gaussian_elimination
    implicit none

    integer :: n, i, j, k, e
    real :: A(0:2, 0:3), x, factor

    n = 2
    A = reshape((/ 3, -3, 6, -7, 5, -4, -2, 1, 0, -7, 5, 2 /), shape(A))

    do i = 0, n
        e = i
        do j = i+1, n
            if (abs(A(j, i)) > abs(A(e, i))) then
                e = j
            end if
        end do

        if (e /= i) then
            do j = i, n+1
                x = A(i, j)
                A(i, j) = A(e, j)
                A(e, j) = x
            end do
        end if

        do j = 0, n
            if (j /= i) then
                factor = A(j, i) / A(i, i)
                do k = i, n+1
                    A(j, k) = A(j, k) - factor * A(i, k)
                end do
            end if
        end do
    end do

    if (e /= n) then
        print*, 'The System of linear equations have no unique solution'
    else if (e == n) then
        print*, 'The solution of the given problem'
        print*, A(0, 0), '* x1 = ', A(0, 3)
        print*, A(1, 1), '* x2 = ', A(1, 3)
        print*, A(2, 2), '* x3 = ', A(2, 3)
    end if
    write(*,100),A
100 Format(f0.2,5x,f0.2,5x,f0.2,5x,f0.2)


end program gaussian_elimination
