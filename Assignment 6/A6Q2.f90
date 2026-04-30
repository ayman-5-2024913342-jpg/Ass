program A6Q1_BackDiff
    implicit none

    real, allocatable :: mat(:,:)
    real :: x_target, p, term, f_x, h, sum_
    integer :: i, j, n, k, factorial

    sum_ = 0.0
    n = 6
    h = 2.0
    allocate(mat(n, n+1))
    mat = 0.0

    open(10, file="A6Q1_in.txt")

    read(10, *) (mat(i, 1), i = 1, n)
    read(10, *) (mat(i, 2), i = 1, n)
    close(10)

    do j = 3, n + 1
        do i = n, (j - 1), -1
            mat(i, j) = mat(i, j-1) - mat(i-1, j-1)
        end do

        k = j - 2
        sum_ = sum_ + mat(n, j) * (1.0 / real(k))
    end do

    sum_ = sum_ / h

    x_target = 11.0

    print*, "Backward Difference Table:"
    print*, "   x       y      By     B2y     B3y     B4y     B5y"
    do i = 1, n
        write(*, '(7F8.4)') (mat(i, j), j = 1, n + 1)
    end do

    print*, ""

    print*, "f'(11.0) using Backward Difference:", sum_

end program A6Q1_BackDiff
