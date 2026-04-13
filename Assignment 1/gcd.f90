program main
    implicit none
    integer :: n, i, g, gcd
    integer, allocatable :: arr(:)

    print*, "Enter the number of nums to find gcd"
    read(*,*) n
    allocate(arr(n))

    print*, "Enter the numbers: "
    do i = 1, n
        read(*,*) arr(i)
    end do

    g = arr(1)
    do i = 2, n
        g = gcd(g, arr(i))
    end do

    print*, "The GCD of the numbers is:", g
end program


integer function gcd(a, b)
    implicit none
    integer :: a, b, temp
    do while (b /= 0)
        temp = mod(a, b)
        a = b
        b = temp
    end do
    gcd = a
end function
