program test
    implicit none
    real, allocatable :: A(:)
    integer:: i
    do i = 1,3
        read*, A(i)
    end do
    print*,A

end program
