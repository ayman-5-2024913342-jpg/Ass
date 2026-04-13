program A5Q2
    implicit none
    integer::n, i
    real::T, simb, simc
    real, dimension(0:11)::f, X

    ! --- TABLE 1 ---
    open(10, file="A5Q2table1.txt")
    n = 11
    read(10, *) (X(i), i=0, n)
    read(10, *) (f(i), i=0, n)
    close(10)


    T = 0.0
    do i = 1, n
        T = T + 0.5 * (f(i) + f(i-1)) * (X(i) - X(i-1))
    end do


    simb = 0.0
    do i = 1, n-1, 2
        simb = simb + ((X(i+1) - X(i-1)) / 6.0) * (f(i-1) + 4.0*f(i) + f(i+1))
    end do

    simb = simb + 0.5 * (f(11) + f(10)) * (X(11) - X(10))


    simc = 0.0
    do i = 1, n-2, 3
        simc = simc + ((X(i+2) - X(i-1)) / 8.0) * (f(i-1) + 3.0*f(i) + 3.0*f(i+1) + f(i+2))
    end do

    do i = 10, 11
        simc = simc + 0.5 * (f(i) + f(i-1)) * (X(i) - X(i-1))
    end do

    write(*,*) "--- Results for Table 1 ---"
    write(*,*) "Trapezoidal: ", T
    write(*,*) "Simpson 1/3: ", simb
    write(*,*) "Simpson 3/8: ", simc

    ! --- TABLE 2 ---
    ! (Repeat the logic above for Table 2 if needed)
end program
