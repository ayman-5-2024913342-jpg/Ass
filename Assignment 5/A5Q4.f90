program A5Q4_Final
    implicit none
    real(8) :: a1, b1, c1, d1 ! Limits for F1
    real(8) :: a2, b2, c2, d2, e2, g2 ! Limits for F2
    integer :: n, m, p

    n = 6; m = 6; p = 6  

    ! --- Limits for F1 (Avoids NaN) ---
    a1 = 0.6d0; b1 = 1.0d0
    c1 = 1.0d0; d1 = 1.6d0

    ! --- Limits for F2 ---
    a2 = 0.0d0; b2 = 2.0d0
    c2 = 0.0d0; d2 = 3.0d0
    e2 = 0.0d0; g2 = 1.0d0

    print*, "Numerical Integration Results"
    print*, "============================================="
    
    print*, "FUNCTION F1 (2D)"
    ! Use the specific F1 limits here
    call trapezoidal_2d(a1, b1, c1, d1, n, m)
    call simpson_13_2d(a1, b1, c1, d1, n, m)
    call simpson_38_2d(a1, b1, c1, d1, n, m)

    print*, ""
    print*, "FUNCTION F2 (3D: x * y * z)"
    ! Use the specific F2 limits here
    call trapezoidal_3d(a2, b2, c2, d2, e2, g2, n, m, p)
    call simpson_13_3d(a2, b2, c2, d2, e2, g2, n, m, p)
    call simpson_38_3d(a2, b2, c2, d2, e2, g2, n, m, p)
    print*, "============================================="

contains
    ! ----------------------------------------------------------
    ! Mathematical Functions
    ! ----------------------------------------------------------
    real(8) function f1(x, y)
        real(8), intent(in) :: x, y
        f1 = (2.0d0 * y**2) / (x + y)
    end function f1

    real(8) function f2(x, y, z)
        real(8), intent(in) :: x, y, z
        f2 = x * y * z
    end function f2

    ! ----------------------------------------------------------
    ! 2D SUBROUTINES
    ! ----------------------------------------------------------
    subroutine trapezoidal_2d(a, b, c, d, n, m)
        real(8), intent(in) :: a, b, c, d
        integer, intent(in) :: n, m
        real(8) :: h, k, total_sum, wx, wy
        integer :: i, j
        h = (b-a)/n; k = (d-c)/m; total_sum = 0.0d0
        do i = 0, n
            wx = merge(1.0d0, 2.0d0, i==0 .or. i==n)
            do j = 0, m
                wy = merge(1.0d0, 2.0d0, j==0 .or. j==m)
                total_sum = total_sum + (wx * wy) * f1(a+i*h, c+j*k)
            end do
        end do
        print*, "a) Trapezoidal 2D: ", total_sum * (h*k/4.0d0)
    end subroutine

    subroutine simpson_13_2d(a, b, c, d, n, m)
        real(8), intent(in) :: a, b, c, d
        integer, intent(in) :: n, m
        real(8) :: h, k, total_sum, wx, wy
        integer :: i, j
        h = (b-a)/n; k = (d-c)/m; total_sum = 0.0d0
        do i = 0, n
            if(i==0 .or. i==n) then; wx=1.d0; else if(mod(i,2)/=0) then; wx=4.d0; else; wx=2.d0; endif
            do j = 0, m
                if(j==0 .or. j==m) then; wy=1.d0; else if(mod(j,2)/=0) then; wy=4.d0; else; wy=2.d0; endif
                total_sum = total_sum + (wx * wy) * f1(a+i*h, c+j*k)
            end do
        end do
        print*, "b) Simpson 1/3 2D: ", total_sum * (h*k/9.0d0)
    end subroutine

    subroutine simpson_38_2d(a, b, c, d, n, m)
        real(8), intent(in) :: a, b, c, d
        integer, intent(in) :: n, m
        real(8) :: h, k, total_sum, wx, wy
        integer :: i, j
        h = (b-a)/n; k = (d-c)/m; total_sum = 0.0d0
        do i = 0, n
            if(i==0 .or. i==n) then; wx=1.d0; else if(mod(i,3)==0) then; wx=2.d0; else; wx=3.d0; endif
            do j = 0, m
                if(j==0 .or. j==m) then; wy=1.d0; else if(mod(j,3)==0) then; wy=2.d0; else; wy=3.d0; endif
                total_sum = total_sum + (wx * wy) * f1(a+i*h, c+j*k)
            end do
        end do
        print*, "c) Simpson 3/8 2D: ", total_sum * (9.0d0*h*k/64.0d0)
    end subroutine

    ! ----------------------------------------------------------
    ! 3D SUBROUTINES
    ! ----------------------------------------------------------
    subroutine trapezoidal_3d(a, b, c, d, e, g, n, m, p)
        real(8), intent(in) :: a, b, c, d, e, g
        integer, intent(in) :: n, m, p
        real(8) :: h, k, l, total_sum, wx, wy, wz
        integer :: i, j, q
        h=(b-a)/n; k=(d-c)/m; l=(g-e)/p; total_sum=0.0d0
        do i = 0, n
            wx = merge(1.0d0, 2.0d0, i==0 .or. i==n)
            do j = 0, m
                wy = merge(1.0d0, 2.0d0, j==0 .or. j==m)
                do q = 0, p
                    wz = merge(1.0d0, 2.0d0, q==0 .or. q==p)
                    total_sum = total_sum + (wx*wy*wz) * f2(a+i*h, c+j*k, e+q*l)
                end do
            end do
        end do
        print*, "a) Trapezoidal 3D: ", total_sum * (h*k*l/8.0d0)
    end subroutine

    subroutine simpson_13_3d(a, b, c, d, e, g, n, m, p)
        real(8), intent(in) :: a, b, c, d, e, g
        integer, intent(in) :: n, m, p
        real(8) :: h, k, l, total_sum, wx, wy, wz
        integer :: i, j, q
        h=(b-a)/n; k=(d-c)/m; l=(g-e)/p; total_sum=0.0d0
        do i = 0, n
            if(i==0 .or. i==n) then; wx=1.d0; else if(mod(i,2)/=0) then; wx=4.d0; else; wx=2.d0; endif
            do j = 0, m
                if(j==0 .or. j==m) then; wy=1.d0; else if(mod(j,2)/=0) then; wy=4.d0; else; wy=2.d0; endif
                do q = 0, p
                    if(q==0 .or. q==p) then; wz=1.d0; else if(mod(q,2)/=0) then; wz=4.d0; else; wz=2.d0; endif
                    total_sum = total_sum + (wx*wy*wz) * f2(a+i*h, c+j*k, e+q*l)
                end do
            end do
        end do
        print*, "b) Simpson 1/3 3D: ", total_sum * (h*k*l/27.0d0)
    end subroutine

    subroutine simpson_38_3d(a, b, c, d, e, g, n, m, p)
        real(8), intent(in) :: a, b, c, d, e, g
        integer, intent(in) :: n, m, p
        real(8) :: h, k, l, total_sum, wx, wy, wz
        integer :: i, j, q
        h=(b-a)/n; k=(d-c)/m; l=(g-e)/p; total_sum=0.0d0
        do i = 0, n
            if(i==0 .or. i==n) then; wx=1.d0; else if(mod(i,3)==0) then; wx=2.d0; else; wx=3.d0; endif
            do j = 0, m
                if(j==0 .or. j==m) then; wy=1.d0; else if(mod(j,3)==0) then; wy=2.d0; else; wy=3.d0; endif
                do q = 0, p
                    if(q==0 .or. q==p) then; wz=1.d0; else if(mod(q,3)==0) then; wz=2.d0; else; wz=3.d0; endif
                    total_sum = total_sum + (wx*wy*wz) * f2(a+i*h, c+j*k, e+q*l)
                end do
            end do
        end do
        print*, "c) Simpson 3/8 3D: ", total_sum * (27.0d0*h*k*l/512.0d0)
    end subroutine

end program A5Q4_Final