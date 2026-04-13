program main
    implicit none
    integer :: n, i, num, g, k
    integer, allocatable :: arr(:)

    print*, "Enter the number of nums to find gcd"
    read(*,*) n
    allocate(arr(n))

    print*, "Enter the numbers: "
    do i = 1, n
        read(*,*) num
        arr(i) = num
    end do

    g = arr(1)
    do i = 2, n
        call gcd(g, arr(i), g)
    end do

    print*, "The GCD of the numbers is:", g
end program

subroutine gcd(a, b, o)
    implicit none
    integer :: a, b, o, temp

    do while (b /= 0)
        temp = mod(a, b)
        a = b
        b = temp
    end do

    o = a
end subroutine
