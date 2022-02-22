program structure
    implicit none !it checks the variable is named correctly

    real:: x= 100.000,y,z,f1,f2     !you can also set value in this line
    integer:: n,i,j,k,a,b,c,it = 10

    integer,allocatable:: M(:,:)

    read*,n
    allocate(M(n,n))

    open(98,file="input.txt")
    open(99,file="output.txt")

    read(98,*),((M(i,j),j=1,n),i=1,n)

    write(99,100),M
100 Format(3I4)

end program

real function f1(x)
    implicit none
    real:: x
    f1 = x**2 + 2*x + 2
end function

real function f2(x)
    implicit none
    real:: x
    f2 = x**2 + exp(-x) + 0.3010
end function
