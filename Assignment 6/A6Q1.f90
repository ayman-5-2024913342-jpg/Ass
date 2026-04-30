program A6Q1
    implicit none

    real, allocatable :: mat(:,:)
    real :: sum_
    integer :: i, j, n, h, k

    n = 6
    h = 2
    sum_ = 0.0

    open(10, file="A6Q1_in.txt")
    allocate(mat(6, 7))
    do i = 1, 2
        read(10, *) (mat(j, i), j = 1, n)
    end do

    do j = 3, n + 1
        do i = 1, n - (j - 2)
            mat(i, j) = mat(i+1, j-1) - mat(i, j-1)
        end do
    end do

    do j = 3, n + 1
        k = j - 2

        sum_ = sum_ + ( ((-1.0)**(k+1)) * mat(1, j) / real(k) )
    end do

    sum_ = sum_ / real(h)

    print*, "Forward Difference Table:"
    print*, "   x         y         del_y     del_2_y   del_3_y   del_4_y   del_5_y"
    do i = 1, n
        write(*, '(7F10.4)') (mat(i, j), j = 1, 7)
    end do

    print*, ""
    print*, "The derivative f'(1) is approximately:", sum_

    close(10)
end program A6Q1
