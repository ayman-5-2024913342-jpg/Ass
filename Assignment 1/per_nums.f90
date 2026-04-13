program perf_nums
    implicit none

    integer :: i, num, divs, j

    num = 6
    divs = 0

    print*, "Divisors of", num, ":"
    do i = 1, num
        if (mod(num, i) == 0) then
            print*, (j, j = 1, i)
            !1100 format(advance=no, 10I3)
            divs = divs + i
        end if
    end do

    print*, "Sum of divisors =", divs
end program perf_nums
