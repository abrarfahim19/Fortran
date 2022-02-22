program condition
    implicit none
    real::x,y
    integer::i

    read*,i

    !<,<=,==,.NE.,>=,>

    if (i<0) then
        print*, "The number is negative"
    else if (i.NE.0) then
        print*, "the number is positive"
    else
        print*, "The number is non-negative"
    end if

end program
