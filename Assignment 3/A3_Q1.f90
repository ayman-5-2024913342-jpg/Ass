program gaussian_elimination
    implicit none
    integer, parameter :: n = 4

    real :: A(4, 5), temp(5), factor
    integer :: i, j, k, pivot_row


    A = reshape([1.0, 1.0, 4.0, 4.0, 7.0, &
                 1.0, 3.0, 7.0, 9.0, 4.0, &
                 -1.0, -2.0,-4.0, -6.0, 6.0, &
                 0.0, -1.0, -2.0, -3.0, 0.0], [4, 5])


    !pertial pivot algo
    do k = 1, n-1

        pivot_row = k
        do i = k+1, n
            if (abs(A(i, k)) > abs(A(pivot_row, k))) then
                pivot_row = i
            end if
        end do

        !Swap rows if necesssary
        if (pivot_row /= k) then
            temp = A(k, :)
            A(k, :) = A(pivot_row, :)
            A(pivot_row, :) = temp
        end if

        !forward elimation
        do i = k+1, n
            factor = A(i, k) / A(k, k)
            A(i, k:n+1) = A(i, k:n+1) - factor * A(k, k:n+1)
        end do
    end do

    ! Back Substitution
    A(n, n+1) = A(n, n+1) / A(n, n)
    do i = n-1, 1, -1
        A(i, n+1) = (A(i, n+1) - sum(A(i, i+1:n) * A(i+1:n, n+1))) / A(i, i)
    end do

    ! Result
    print *, "Result (x, y, z):", A(:, n+1)
end program gaussian_elimination
