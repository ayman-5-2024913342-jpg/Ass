program gen_sym1
    implicit none

    integer :: n,i,j;
    integer, allocatable :: mat_in(:,:)
    integer, allocatable :: mat_out(:,:)
    print*, 'Enter the value of n: '
    read*, n

    allocate(mat_in(n,n))
    allocate(mat_out(n,n))


    call gen_skew_sym(n)


end program

subroutine gen_skew_sym(n)
    implicit none

    integer :: i,j,n

    integer, dimension(n,n) :: matin
    integer, dimension(n,n) :: matout

    do i = 1,n
        do j = 1,n
            matin(i,j) = 0
        end do
    end do

    do i = 1, n
        do j = 1, i-1
            matin(i,j) = i*j
        end do
    end do

    do i = 2, n
        do j = 1, i-1
            matin(j,i) = (-1) * matin(i,j)
        end do
    end do

    print*, "Matrix 1:"
    do i = 1, n
        do j = 1, n
            write(*, '(I6)', advance='no')  matin(i,j)
        end do
        print*,""
    end do
end subroutine
