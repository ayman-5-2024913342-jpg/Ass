program A3Q5
    implicit none

    real :: tol
    integer :: maxit, i, j, n
    real, allocatable :: A(:,:), b(:), xo(:)

    open(10, file="in.txt")
    open(11, file="out.txt")

    read(10, *) n

    allocate(A(n,n), b(n), xo(n))

    do i = 1,n
        read(10,*)(A(i,j), j=1,n)
        write(*,*)(A(i,j), j=1,n)
    end do

    read(10,*) (b(i), i=1,n)

    xo = 0.0
    tol = 1.0e-3
    maxit = 1000   ! set maximum iterations

    call jacobi(A, b, xo, n, tol, maxit)

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

        ! Compute error as norm of difference
        error = maxval(abs(x - xo))

        if (error < tol) then
            print*, "Solution: "
            print*, x
            print*, "Iteration =", k
            return
        end if

        xo = x
        k = k + 1
    end do

    print*, "Max iterations exceeded"

end subroutine jacobi
