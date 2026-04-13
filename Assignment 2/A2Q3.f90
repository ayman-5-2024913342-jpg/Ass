program gen_sym1
    implicit none

    integer :: n,i,j;
    integer, allocatable :: mat_in(:,:)
    integer, allocatable :: mat_out(:,:)
    print*, 'Enter the value of n: '
    read*, n

    allocate(mat_in(n,n))
    allocate(mat_out(n,n))

    do i = 1, n
        do j = 1, n
            mat_in(i,j) = i*j
        end do
    end do

    !print*, "OG MATRIX: "
    !do i = 1, n
    !    do j = 1, n
    !        write(*, '(I6)', advance='no') mat_in(i,j)
    !    end do
    !    print*,""
    !end do

    print*,
    print*, "Symmetric MATRIX: "
    call gen_sym(mat_in, mat_out, n)



end program

subroutine gen_sym(matI, mato, n)
    implicit none

    integer :: i, j, n
    integer, dimension(n,n) :: matI
    integer, dimension(n,n) :: mato

    do i = 2, n
        do j = 1, i-1
            mato(i,j) = mati(j,i)
        end do
    end do

    !do i = 1, n
    !    do j = 1, n
    !        write(*, '(I6)', advance='no') matO(i,j)
    !    end do
    !    print*,
    !end do

    do i = 1, n
        do j = 1, n
            write(*, '(I6)', advance='no') mati(j,i)
        end do
        print*,""
    end do

end subroutine
