program fixed_point
    implicit none
    real :: x, x0, x1, f, ero, tol
    integer :: i

    tol = 0.0000001
    ero = 100.0

    print*, 'Enter the initial value'
    read*, x0

    if (f(x0)==0) then
        print*, 'The Root Is: ' , x0
    end if

    do while(ero.gt.tol)
        x1 = f(x0)
        ero = abs(x1-x0)
        x0 = x1
    end do
    print*, x0
!    write(*,10),x1
!10  Format('The Root is :', 2x, f0.5)

end program

function f(x)
    implicit none
    real :: x, f
    !f = 3*x**3 + 2*x**2 + 12
    !f = ((2*x**2+12)/3)**(1/3)
    f = (x+1)**(1/3)
    end function f
