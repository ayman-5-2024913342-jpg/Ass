program matMal
    implicit none

    integer :: mat1_r, mat1_c, mat2_r, mat2_c, x, y
    integer, allocatable :: mat1(:,:), mat2(:,:), mat3(:,:)

    open(10, file="A2_Q2_in.txt")

    ! Read dimensions
    read(10, *) mat1_r, mat1_c
    read(10, *) mat2_r, mat2_c

    ! Check dimension compatibility
    if (mat1_c == mat2_r) then
        print*, "Dimension matches"

        ! Allocate matrices
        allocate(mat1(mat1_r, mat1_c))
        allocate(mat2(mat2_r, mat2_c))
        allocate(mat3(mat1_r, mat2_c))

        ! Read matrix data
        read(10, *) mat1
        read(10, *) mat2

        ! Multiply
        call matMul(mat1_r, mat1_c, mat2_r, mat2_c, mat1, mat2, mat3)

        ! Print matrices
        print*, "Matrix 1: (", mat1_r, "x", mat1_c, ")"
        do x = 1, mat1_r
            do y = 1, mat1_c
                write(*, "(I6)", advance='no') mat1(x, y)
            end do
            print*
        end do

        print*, "Matrix 2: (", mat2_r, "x", mat2_c, ")"
        do x = 1, mat2_r
            do y = 1, mat2_c
                write(*, "(I6)", advance='no') mat2(x, y)
            end do
            print*
        end do


        print*, ""
        print*, "Matrix 3: (", mat1_r, "x", mat2_c, ")"
        do x = 1, mat1_r
            do y = 1, mat2_c
                write(*, "(I6)", advance='no') mat3(x, y)
            end do
            print*
        end do

    else
        print*, "Learn basic linear algebra dummy"
    end if

end program

subroutine matMul(mat1_r, mat1_c, mat2_r, mat2_c, mat1, mat2, mat3)
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
