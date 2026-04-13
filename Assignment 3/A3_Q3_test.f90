
program crammer_rule
    implicit none

    integer, parameter :: n = 4
    real :: A(4, 4), B(4), temp(4, 4), X(4)
    real :: detA, detK, det
    integer :: i, j, k

    ! Open and read from the file
    open(10, file="A3_Q1_in.txt", status='old')

    do i = 1, n
        read(10, *) A(i, :), B(i)
    end do
    close(10)

    print*, "Og mat: "
    do i = 1, n
            do j = 1, n
                write(*, "(f12.6)", advance='no') A(i, j)
            end do
            print*
    end do

    call get_det4x4(A, detA)

    print *, "The determinant is: ", detA
    print*,

    if (abs(detA) < 1e-6) then
        print *, "determinant is zero"
        stop
    end if

    ! Crammers rule
    do k = 1, 4
        temp = A
        temp(:, k) = B
        CALL get_det4x4(temp, detK)
        print*, "Determinant mat (X", k, ")"
        do i = 1, n
                do j = 1, n
                    write(*, "(f12.6)", advance='no') temp(i, j)
                end do
                print*
        end do
        print *, "The determinant is: ", detk
        print*,""
        !detK = determinant3x3(temp)
        X(k) = detK / detA
    end do

    print *, "Result (W, X, Y, Z):", X

contains

    subroutine get_det4x4(m, d)
        real, intent(in) :: m(4, 4)
        real, intent(out) :: d
        real :: sub_m(3, 3)
        real :: det3
        integer :: col, j, k

        d = 0.0

        ! Expanding along the first row
        do col = 1, 4
            ! Extract the 3x3 minor matrix
            k = 1
            do j = 1, 4
                if (j == col) cycle
                ! m(2:4, j) takes rows 2, 3, and 4 of the current column
                sub_m(:, k) = m(2:4, j)
                k = k + 1
            end do

            ! Calculate determinant of the 3x3 minor
            det3 = det3x3(sub_m)

            ! Apply checkerboard sign: + - + -
            if (mod(col, 2) == 0) then
                d = d - m(1, col) * det3
            else
                d = d + m(1, col) * det3
            end if
        end do
    end subroutine get_det4x4

    function det3x3(m) result(res)
        real, intent(in) :: m(3, 3)
        real :: res
        res = m(1,1)*(m(2,2)*m(3,3) - m(2,3)*m(3,2)) - &
              m(1,2)*(m(2,1)*m(3,3) - m(2,3)*m(3,1)) + &
              m(1,3)*(m(2,1)*m(3,2) - m(2,2)*m(3,1))
    end function det3x3

end program crammer_rule
