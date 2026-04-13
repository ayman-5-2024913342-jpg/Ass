program MatAdd
    implicit none

    integer :: M, N, i, j
    integer, allocatable :: mat1(:,:), mat2(:,:), mat3(:,:)

    open(10, file="A2_Q1_in.txt")
    !open(11, file="mat2.txt")


    read(10, *) M, N

    allocate(mat1(M,N))
    allocate(mat2(M,N))
    allocate(mat3(M,N))

    print*, M, N
    read(10,*) mat1
    read(10,*) mat2

    !prints mat1
    print*, "Matrix 1:"
    do i = 1, m
        do j = 1, n
            write(*, '(I6)', advance='no')  mat1(i,j)
        end do
        print*,""
    end do

    print*,""
    print*, "Matrix 2:"
    do i = 1, m
        do j = 1, n
            write(*, '(I6)', advance='no')  mat2(i,j)
        end do
        print*,""
    end do
    !print*, mat1
    !print*, mat2
    print*,""
    call addMat(m, n, mat1, mat2, mat3)


    print*, "Resultant Matrix (mat3):"
        print*, "Matrix 2:"
    do i = 1, m
        do j = 1, n
            write(*, '(I6)', advance='no')  mat3(i,j)
        end do
        print*, ""
    end do

end program MatAdd


subroutine addMat(m,n, mat1, mat2, mat3)
    implicit none

    integer :: m, n, i, j

    integer, dimension(m,n) :: mat1
    integer, dimension(m,n) :: mat2
    integer, dimension(m,n) :: mat3

    do i = 1, m
        do j = 1, n
            mat3(i,j) = mat1(i,j) + mat2(i,j)
        end do
    end do

    !print*, mat1
end subroutine addMat
