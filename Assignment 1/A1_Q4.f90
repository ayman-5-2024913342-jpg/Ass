program main
    implicit none
    integer :: n, i, num, g
    integer :: x = 6, y = 8, k, output
    integer, allocatable :: arr(:)

    print*, "Enter the number of nums to find gcd"

    read(*,*)n
    allocate(arr(n))

    print*, "Enter the numbers: "

    do i = 1, n
        read(*,*) num
        arr(i) = num
    end do


    g = arr(1)
    do i = 2, n
        call gcd(g, arr(i))
    end do

    print*, "The GCD of the numbers is:", k
    !call gcd(x,y,k)
    !print*, k
end program

subroutine gcd(a,b,o)
    IMPLICIT none
    integer :: a,b,temp, o
    do while ( (temp /= 1).and.(a > 0).and.(b > 0) )
        temp = mod(b,a)
        b = a
        a = temp
    end do

    o = b
end subroutine
