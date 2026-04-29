program matrix_property_check
    implicit none
    integer :: n, i, j
    real, allocatable :: A(:,:), B(:,:), AB(:,:), LHS(:,:), AT(:,:), BT(:,:), RHS(:,:)
    logical :: equal

    ! 1. Get matrix size from keyboard
    print *, "Enter the size of the square matrix (n >= 2):"
    read *, n
    if (n < 2) then
        print *, "Error: n must be at least 2."
        stop
    end if

    ! Allocate memory for matrices
    allocate(A(n,n), B(n,n), AB(n,n), LHS(n,n), AT(n,n), BT(n,n), RHS(n,n))

    ! 2. Read matrices A and B from 'input.txt'
    ! The file should have n*n values for A, then n*n values for B
    open(unit=10, file='input.txt', status='old', action='read', err=99)
    do i = 1, n
        read(10, *) (A(i,j), j=1, n)
    end do
    do i = 1, n
        read(10, *) (B(i,j), j=1, n)
    end do
    close(10)

    ! 3. Display Input Matrices
    print *, ""
    print *, "--- Original Matrix A ---"
    call print_matrix(A, n)
    print *, "--- Original Matrix B ---"
    call print_matrix(B, n)

    ! 4. Compute LHS: (A * B)^T
    call MAT_PRODUCT(A, B, AB, n)
    print *, "--- Intermediate Matrix (A * B) ---"
    call print_matrix(AB, n)

    call MAT_TRANSPOSE(AB, LHS, n)
    print *, "--- LHS result: (A * B)^T ---"
    call print_matrix(LHS, n)

    ! 5. Compute RHS: B^T * A^T
    call MAT_TRANSPOSE(A, AT, n)
    call MAT_TRANSPOSE(B, BT, n)
    call MAT_PRODUCT(BT, AT, RHS, n)

    print *, "--- RHS result: B^T * A^T ---"
    call print_matrix(RHS, n)

    ! 6. Check if LHS == RHS (within a small numerical tolerance)
    equal = .true.
    do i = 1, n
        do j = 1, n
            if (abs(LHS(i,j) - RHS(i,j)) > 1.0e-5) equal = .false.
        end do
    end do

    print *, "***************************************************"
    if (equal) then
        print *, " SUCCESS: The equation (AB)^T = B^T A^T holds."
    else
        print *, " FAILURE: The equation does NOT hold."
    end if
    print *, "***************************************************"

    deallocate(A, B, AB, LHS, AT, BT, RHS)
    stop

    ! Error handling for file opening
99  print *, "Error: Could not find or open 'input.txt'."
    stop

contains

    subroutine MAT_TRANSPOSE(mat, result, size)
        integer, intent(in) :: size
        real, intent(in) :: mat(size, size)
        real, intent(out) :: result(size, size)
        integer :: r, c
        do r = 1, size
            do c = 1, size
                result(c, r) = mat(r, c)
            end do
        end do
    end subroutine MAT_TRANSPOSE

    subroutine MAT_PRODUCT(mat1, mat2, result, size)
        integer, intent(in) :: size
        real, intent(in) :: mat1(size, size), mat2(size, size)
        real, intent(out) :: result(size, size)
        integer :: r, c, k
        result = 0.0
        do r = 1, size
            do c = 1, size
                do k = 1, size
                    result(r, c) = result(r, c) + mat1(r, k) * mat2(k, c)
                end do
            end do
        end do
    end subroutine MAT_PRODUCT

    subroutine print_matrix(mat, size)
        integer, intent(in) :: size
        real, intent(in) :: mat(size, size)
        integer :: r
        do r = 1, size
            print "(10F10.4)", mat(r, :)
        end do
        print *, "" ! New line for spacing
    end subroutine print_matrix

end program matrix_property_check