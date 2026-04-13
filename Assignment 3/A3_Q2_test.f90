program solve_by_inverse
    implicit none
    integer, parameter :: n = 4
    real :: A(n, n), A_inv(n, n), B(n), X(n)
    real :: aug(n, n+n), factor
    integer :: i, j, k

    ! Open and read from the file
    open(10, file="A3_Q1_in.txt")

    ! Read row-by-row to avoid column-major flip
    do i = 1, n
        read(10, *) A(i, :), B(i)
    end do
    close(10)

    ! Initialize Augmented Matrix: [A | Identity]
    aug = 0.0
    aug(1:n, 1:n) = A
    do i = 1, n
        aug(i, n+i) = 1.0
    end do

    ! Gauss-Jordan Elimination
    do i = 1, n
        ! Check for zero pivot to avoid division by zero
        if (abs(aug(i, i)) < 1e-6) then
            print *, "Error: Zero pivot encountered at row ", i
            stop
        end if

        ! Step 1: Normalize pivot row (make diagonal element 1)
        factor = aug(i, i)
        aug(i, :) = aug(i, :) / factor

        ! Step 2: Eliminate all other entries in current column
        do j = 1, n
            if (i /= j) then
                factor = aug(j, i)
                aug(j, :) = aug(j, :) - factor * aug(i, :)
            end if
        end do
    end do

    ! Extract Inverse Matrix from the right half of aug
    A_inv = aug(:, n+1:2*n)

    ! Finding result (X = A_inv * B)
    X = matmul(A_inv, B)

    !call manual_matmul(A_inv, reshape(B, [n, 1]), reshape(X, [n, 1]))

    !call manual_matmul(A_inv, B, X)

    ! Display Results
    print *, "Inverse Matrix (A^-1):"
    do i = 1, n
        print '(4F10.4)', A_inv(i, :)
    end do

    print *, ""
    print '(A, 4F10.4)', "Solution (X): ", X

end program solve_by_inverse

subroutine manual_matmul(A, B, C)
        real, intent(in)  :: A(:,:), B(:,:)
        real, intent(out) :: C(size(A,1), size(B,2))
        integer :: i, j, k
        integer :: L, M, N

        L = size(A, 1)
        M = size(A, 2)
        N = size(B, 2)

        ! Initialize C to zero
        C = 0.0

        ! Standard triple-nested loop O(n^3)
        do i = 1, L          ! Rows of A
            do j = 1, N      ! Columns of B
                do k = 1, M  ! Elements to sum (Columns of A / Rows of B)
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                end do
            end do
        end do
end subroutine manual_matmul
