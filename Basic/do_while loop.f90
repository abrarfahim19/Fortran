program do_while_loop
    implicit none
    real::x
    integer::i,n

    do i = 10,1,-1
        if (i==5) then
            go to 10
        end if
        print*,i
    end do

10  print*,"New Loop starts"

    do while(i<=10)
        print*,i
        i=i+1
    end do

20  read*,n
    if (n<0) then
        Print*, "Please give a positive number"
        go to 20
    end if

end program
