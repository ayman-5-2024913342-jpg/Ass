program MatrixAddition
    implicit none
    integer, parameter :: n = 3, m = 3
    integer :: A(n,m), B(n,m), C(n,m)
    integer :: i, j

    A = reshape([1,2,3,4,5,6,7,8,9], shape(A))
    B = reshape([9,8,7,6,5,4,3,2,1], shape(B))


    call addMatrices(A, B, C, n, m)

    print *, "Matrix A:"
    do i = 1, n
        print *, A(i,:)
    end do

    print *, "Matrix B:"
    do i = 1, n
        print *, B(i,:)
    end do

    print *, "Sum of A and B (Matrix C):"
    do i = 1, n
        print *, C(i,:)
    end do

contains

    subroutine addMatrices(X, Y, Z, rows, cols)
        integer, intent(in) :: rows, cols
        integer, intent(in) :: X(rows,cols), Y(rows,cols)
        integer, intent(out) :: Z(rows,cols)
        integer :: i, j

        do i = 1, rows
            do j = 1, cols
                Z(i,j) = X(i,j) + Y(i,j)
            end do
        end do
    end subroutine addMatrices

end program MatrixAddition
