program check_mat_shii
    implicit none

    integer :: N, i, j
    integer, allocatable :: matA(:,:), matB(:,:), mat_mal_out(:,:)

    open(10, file="A2_Q5_in.txt")

    read(10, *) N

    if (N <= 2) then
        print*, "N must be greater than 2"
        return
    end if

    allocate(matA(N,N))
    allocate(matB(N,N))
    allocate(mat_mal_out(N,N))

    read(10, *) matA
    read(10, *) matB

    call matMul_(matA, matB, mat_mal_out)

    print*, "Matrix Multiplication Result:"
    do i = 1, N
        write(*,'(100I6)') mat_mal_out(i,:)
    end do

    print*, "Transpose of Matrix A:"
    call transpose_(matA, matB, N)

end program


subroutine matMul_(mat1, mat2, mat3)
    implicit none
    integer, intent(in) :: mat1(:,:), mat2(:,:)
    integer, intent(out) :: mat3(:,:)
    integer :: i, j, k

    do i = 1, size(mat1,1)
        do j = 1, size(mat2,2)
            mat3(i,j) = 0
            do k = 1, size(mat1,2)
                mat3(i,j) = mat3(i,j) + mat1(i,k) * mat2(k,j)
            end do
        end do
    end do
end subroutine


subroutine transpose_(mat_in, mat_out, n)
    implicit none
    integer, intent(in) :: n
    integer, intent(in) :: mat_in(n,n)
    integer, intent(out) :: mat_out(n,n)
    integer :: i, j

    do i = 1, n
        do j = 1, n
            mat_out(j,i) = mat_in(i,j)
        end do
    end do

    do i = 1, n
        write(*,'(100I6)') mat_out(i,:)
    end do
end subroutine
