program matrix_property_check
    implicit none
    integer :: n, i, j
    real, allocatable :: A(:,:), B(:,:), AB(:,:), LHS(:,:), AT(:,:), BT(:,:), RHS(:,:)
    logical :: equal

    ! 1. Get matrix size from keyboard
    print *, "Enter the size of the square matrix (n >= 2):"
    read *, n
    if (n < 2) stop "n must be at least 2."

    ! Allocate matrices
    allocate(A(n,n), B(n,n), AB(n,n), LHS(n,n), AT(n,n), BT(n,n), RHS(n,n))

    ! 2. Read matrices from file 'input.txt'
    open(unit=10, file='input.txt', status='old', action='read')
    do i = 1, n
        read(10, *) (A(i,j), j=1, n)
    end do
    do i = 1, n
        read(10, *) (B(i,j), j=1, n)
    end do
    close(10)

    ! 3. Compute LHS: (A * B)^T
    call MAT_PRODUCT(A, B, AB, n)
    call MAT_TRANSPOSE(AB, LHS, n)

    ! 4. Compute RHS: B^T * A^T
    call MAT_TRANSPOSE(A, AT, n)
    call MAT_TRANSPOSE(B, BT, n)
    call MAT_PRODUCT(BT, AT, RHS, n)

    ! 5. Check if LHS == RHS
    equal = .true.
    do i = 1, n
        do j = 1, n
            if (abs(LHS(i,j) - RHS(i,j)) > 1.0e-5) equal = .false.
        end do
    end do

    if (equal) then
        print *, "The equation (AB)^T = B^T A^T holds for the given matrices."
    else
        print *, "The equation does NOT hold."
    end if

contains

    subroutine MAT_TRANSPOSE(mat, result, size)
        integer, intent(in) :: size
        real, intent(in) :: mat(size, size)
        real, intent(out) :: result(size, size)
        integer :: i, j
        do i = 1, size
            do j = 1, size
                result(j, i) = mat(i, j)
            end do
        end do
    end subroutine MAT_TRANSPOSE

    subroutine MAT_PRODUCT(mat1, mat2, result, size)
        integer, intent(in) :: size
        real, intent(in) :: mat1(size, size), mat2(size, size)
        real, intent(out) :: result(size, size)
        integer :: i, j, k
        result = 0.0
        do i = 1, size
            do j = 1, size
                do k = 1, size
                    result(i, j) = result(i, j) + mat1(i, k) * mat2(k, j)
                end do
            end do
        end do
    end subroutine MAT_PRODUCT

end program matrix_property_check