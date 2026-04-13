program A5Q2
    implicit none
    integer :: n, i
    real :: T, simb, simc
    real, dimension(0:11) :: f1, x1, f2, x2

    ! --- TABLE 1 ---
    open(10, file="A5Q2table1.txt")
    n = 11
    read(10, *) (x1(i), i=0, n)
    read(10, *) (f1(i), i=0, n)
    close(10)

    ! --- TABLE 2 ---
    open(11, file="A5Q2table2.txt")
    n = 11
    read(11, *) (x2(i), i=0, n)
    read(11, *) (f2(i), i=0, n)
    close(11)

    ! Trapezoidal Rule
    T = 0.0
    do i = 1, n
        T = T + 0.5 * (f1(i) + f1(i-1)) * (x1(i) - x1(i-1))
    end do

    ! Simpson's 1/3 Rule
    simb = 0.0
    do i = 1, n-1, 2
        simb = simb + ((x1(i+1) - x1(i-1)) / 6.0) * (f1(i-1) + 4.0*f1(i) + f1(i+1))
    end do
    ! Handling the leftover interval for n=11 using Trapezoidal
    simb = simb + 0.5 * (f1(11) + f1(10)) * (x1(11) - x1(10))

    ! Simpson's 3/8 Rule
    simc = 0.0
    do i = 1, n-2, 3
        simc = simc + ((x1(i+2) - x1(i-1)) / 8.0) * (f1(i-1) + 3.0*f1(i) + 3.0*f1(i+1) + f1(i+2))
    end do
    ! Handling the remaining two intervals for n=11 using Trapezoidal
    do i = 10, 11
        simc = simc + 0.5 * (f1(i) + f1(i-1)) * (x1(i) - x1(i-1))
    end do


    write(*,*) "--- Results for Table 1 ---"
    write(*,*) "Trapezoidal: ", T
    write(*,*) "Simpson 1/3: ", simb
    write(*,*) "Simpson 3/8: ", simc

    print*, " "
    !for table 2
    ! Trapezoidal Rule
    T = 0.0
    do i = 1, n
        T = T + 0.5 * (f2(i) + f2(i-1)) * (x2(i) - x2(i-1))
    end do

    ! Simpson's 1/3 Rule
    simb = 0.0
    do i = 1, n-1, 2
        simb = simb + ((x2(i+1) - x2(i-1)) / 6.0) * (f2(i-1) + 4.0*f2(i) + f2(i+1))
    end do
    ! Handling the leftover interval for n=11 using Trapezoidal
    simb = simb + 0.5 * (f2(11) + f2(10)) * (x2(11) - x2(10))

    ! Simpson's 3/8 Rule
    simc = 0.0
    do i = 1, n-2, 3
        simc = simc + ((x2(i+2) - x2(i-1)) / 8.0) * (f2(i-1) + 3.0*f2(i) + 3.0*f2(i+1) + f2(i+2))
    end do
    ! Handling the remaining two intervals for n=11 using Trapezoidal
    do i = 10, 11
        simc = simc + 0.5 * (f2(i) + f2(i-1)) * (x2(i) - x2(i-1))
    end do

    ! Fixed "f1or" to "for"
    write(*,*) "--- Results for Table 1 ---"
    write(*,*) "Trapezoidal: ", T
    write(*,*) "Simpson 1/3: ", simb
    write(*,*) "Simpson 3/8: ", simc
end program
