  program NumericalIntegration
    implicit none

    ! n must be 30 to work for Trapezoidal, Simpson's 1/3, 3/8, and Weddle's (multiple of 6)
    real :: a, b, n, h, s, real_out, error_
    real :: res13, res38, resWeddle
    integer :: i
    real, dimension(31) :: x_vals, f_vals

    ! External functions
    real, external :: f, f_

    ! Setup values
    a = 1.0
    b = 9.0
    n = 30.0
    h = (b - a) / n

    ! Fill arrays
    do i = 1, 31
        x_vals(i) = a + (i-1) * h
        f_vals(i) = f(x_vals(i))
    end do

    ! 1. Trapezoidal Rule
    s = 0.0
    do i = 1, 30
        s = s + 0.5 * (f_vals(i) + f_vals(i+1)) * h
    end do

    ! 2. Simpson's 1/3 Rule
    res13 = f_vals(1) + f_vals(31)
    do i = 2, 30
        if (mod(i-1, 2) /= 0) then
            res13 = res13 + 4.0 * f_vals(i)
        else
            res13 = res13 + 2.0 * f_vals(i)
        end if
    end do
    res13 = res13 * (h / 3.0)

    ! 3. Simpson's 3/8 Rule (n must be multiple of 3)
    res38 = f_vals(1) + f_vals(31)
    do i = 2, 30
        if (mod(i-1, 3) == 0) then
            res38 = res38 + 2.0 * f_vals(i)
        else
            res38 = res38 + 3.0 * f_vals(i)
        end if
    end do
    res38 = res38 * (3.0 * h / 8.0)

    ! 4. Weddle's Rule (n must be multiple of 6)
    resWeddle = 0.0
    do i = 1, 25, 6
        resWeddle = resWeddle + (3.0*h/10.0) * (f_vals(i) + 5.0*f_vals(i+1) + &
                    f_vals(i+2) + 6.0*f_vals(i+3) + f_vals(i+4) + 5.0*f_vals(i+5) + f_vals(i+6))
    end do

    ! Real sol
    real_out = f_(b) - f_(a)


    print*, "--------------------------- Results -------------------------------"
    print*, "Method      ", "     Approximate Value", "      Exact Value", "        Relative Error "
    print *, "Trapezoidal:       ", s, real_out, abs((real_out-s)/real_out)*100
    print *, "Simpson's 1/3:     ", res13, real_out, abs((real_out-res13)/real_out)*100
    print *, "Simpson's 3/8:     ", res38, real_out, abs((real_out-res38)/real_out)*100
    print *, "Weddle's Rule:     ", resWeddle, real_out, abs((real_out-resWeddle)/real_out)*100

end program

! The function to integrate
real function f(t) result(y)
    implicit none
    real :: t
    y = ((2.0 * t**2) + (t**(2.5)) - 1.0) / (t**2.0)
end function

! The analytical antiderivative
real function f_(t) result(y)
    implicit none
    real :: t
    y = (2.0*t + (2.0/3.0) * t**1.5 + (1.0/t))
end function
