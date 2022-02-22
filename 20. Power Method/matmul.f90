program PowerMethod
    implicit none
    real::A(3,3),B(3,1),c(3,1),tol = 0.0001,er = 100,a1 = 100
    integer::i,j,k,n=3

    open(98,file="input.txt")
    read(98,*),((A(i,j),j=1,3),i=1,3)
    read(98,*),(B(i,1),i=1,3)

    do while (er>tol)
        !c = matmul(A,B)
        do i = 1,n
            C(i,1) = 0
            do j = 1,n
                C(i,1)=C(i,1)+A(i,j)*B(j,1)
            end do
        end do

        er = abs(a1-maxval(c))
        a1 = maxval(c)
        c = c/maxval(c)
        B = c
    end do

    print*,"The Eigen Value is"
    print*,a1

    print*,"The Eigen Vector is"
    print*,c

end program
