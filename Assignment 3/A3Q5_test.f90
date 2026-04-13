program A3Q5
    implicit none

    real :: tol
    integer :: maxit, i, j, n
    real, allocatable :: A(:,:), b(:), xo(:)

    open(10, file="in.txt")
    open(11, file="out.txt")

    read(10, *) n

    allocate(A(n,n), b(n), xo(n))

    ! Read matrix A
    write(*,*) "Matrix A:"
    write(11,*) "Matrix A:"
    do i = 1,n
        read(10,*)(A(i,j), j=1,n)
        write(*,'(100F10.4)') (A(i,j), j=1,n)
        write(11,'(100F10.4)') (A(i,j), j=1,n)
    end do

    ! Read vector b
    read(10,*) (b(i), i=1,n)
    write(*,*) "Vector b:"
    write(*,'(100F10.4)') b
    write(11,*) "Vector b:"
    write(11,'(100F10.4)') b

    xo = 0.0
    tol = 1.0e-3
    maxit = 1000   ! maximum iterations

    call jacobi(A, b, xo, n, tol, maxit)

    close(10)
    close(11)

end program A3Q5


subroutine jacobi(A, b, xo, n, tol, maxit)
    implicit none

    integer :: n, maxit
    real :: A(n,n), b(n), xo(n), x(n)
    real :: tol, error, sum
    integer :: i, j, k

    k = 1

    do while (k <= maxit)
        do i = 1,n
            sum = 0.0
            do j = 1,n
                if (j /= i) then
                    sum = sum + A(i,j)*xo(j)
                end if
            end do
            x(i) = (b(i) - sum) / A(i,i)
        end do

        ! Compute error as infinity norm
        error = maxval(abs(x - xo))

        if (error < tol) then
            print*, "----------------------------------"
            print*, "   Solution (roots) Table"
            print*, "----------------------------------"
            print*, " Var     Value"
            print*, "----------------------------------"
            do i = 1,n
                write(*,'(A,I2,A,F12.6)') "x", i, " = ", x(i)
            end do
            print*, "----------------------------------"
            print*, "Iterations =", k

            write(11,*) "----------------------------------"
            write(11,*) "   Solution (roots) Table"
            write(11,*) "----------------------------------"
            write(11,*) " Var     Value"
            write(11,*) "----------------------------------"
            do i = 1,n
                write(11,'(A,I2,A,F12.6)') "x", i, " = ", x(i)
            end do
            write(11,*) "----------------------------------"
            write(11,*) "Iterations =", k

            return
        end if

        xo = x
        k = k + 1
    end do

    print*, "Max iterations exceeded"
    write(11,*) "Max iterations exceeded"

end subroutine jacobi
