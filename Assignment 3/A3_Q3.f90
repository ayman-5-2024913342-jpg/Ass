program cramers_rule
    implicit none
    real :: A(4, 4), B(4), temp(4, 4), X(4)
    real :: detA, detK
    integer :: i, j, k

    ! define a and b
    A = reshape([1.0, 1.0, 4.0, 4.0, &
                 1.0, 3.0, 7.0, 9.0, &
                 -1.0, -2.0, -4.0, -6.0, &
                 0.0, -1.0, -2.0, -3.0], [4, 4])
    B = [7.0, 4.0, 6.0, 0.0]

    ! Finds det
    detA = determinant4x4(A)

    if (abs(detA) < 1e-6) then
        print *, "determinant is zero"
        stop
    end if

    ! Crammers rule
    do k = 1, 3
        temp = A
        temp(:, k) = B
        detK = determinant3x3(temp)
        X(k) = detK / detA
    end do

    print *, "Result (X, Y, Z):", X

contains
    ! 4x4 determinant function
    function determinant3x3(m) result(d)
        real, intent(in) :: m(3, 3)
        real :: d
        d = m(1,1)*(m(2,2)*m(3,3) - m(2,3)*m(3,2)) - &
            m(1,2)*(m(2,1)*m(3,3) - m(2,3)*m(3,1)) + &
            m(1,3)*(m(2,1)*m(3,2) - m(2,2)*m(3,1))
    end function determinant3x3

end program cramers_rule



    subroutine get_determinant4x4(m, d)
        real, intent(in) :: m(4, 4)
        real, intent(out) :: d
        real :: sub_m(3, 3)
        real :: det3
        integer :: i, j, k, col

        d = 0.0

        ! Expanding along the first row
        do col = 1, 4
            ! Create the 3x3 minor matrix by excluding row 1 and the current column
            k = 1
            do j = 1, 4
                if (j == col) cycle
                sub_m(:, k) = m(2:4, j)
                k = k + 1
            end do

            ! Calculate the determinant of the 3x3 minor
            det3 = determinant3x3(sub_m)

            ! Add/subtract based on the checkerboard pattern (-1**(1+col))
            if (mod(col, 2) == 0) then
                d = d - m(1, col) * det3
            else
                d = d + m(1, col) * det3
            end if
        end do
    end subroutine get_determinant4x4
