program check_mat_shii
    implicit none

    integer :: N, i, j
    integer, allocatable :: matB(:,:), matA(:,:), matAxmatB, mat_mal_out

    open(10, file="A2_Q5_in.txt")

    read(10, *) N

    if (N <= 2) then
        print*, "N must be greater than 2"
        return
    end if

    allocate(mata(n,n))
    allocate(matb(n,n))

    read(10, *) matA
    read(10, *) matB

    call matMul_(n,n,n,n, matA, matB, mat_mal_out)

    print*, mat_mal_out

    call transpose_(matA, matB, n)

end program

subroutine matMul_(mat1_r, mat1_c, mat2_r, mat2_c, mat1, mat2, mat3)
    implicit none
    integer :: i, j, k
    integer :: mat1_r, mat1_c, mat2_r, mat2_c
    integer, dimension(mat1_r, mat1_c) :: mat1
    integer, dimension(mat2_r, mat2_c) :: mat2
    integer, dimension(mat1_r, mat2_c) :: mat3

    do i = 1, mat1_r
        do j = 1, mat2_c
            mat3(i,j) = 0
            do k = 1, mat1_c
                mat3(i,j) = mat3(i,j) + mat1(i,k) * mat2(k,j)
            end do
        end do
    end do
end subroutine

subroutine transpose_(mat_in, mat_out, n)
    implicit none

    integer :: i, j, n

    integer, dimension(n, n) :: mat_in
    integer, dimension(n, n) :: mat_out

    do i = 1, n
        do j = 1, n
            mat_out(j,i) = mat_in(j,i)
        end do
    end do

    do i = 1, n
        do j = 1, n
            write(*, '(I6)', advance='no')  mat_out(i,j)
        end do
        print*,""
    end do
end subroutine
