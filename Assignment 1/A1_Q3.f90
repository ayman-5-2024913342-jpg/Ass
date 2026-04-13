program main
    implicit none

    integer :: num, c, sq
    real :: sq_rt

    print*, "Enter number: "
    read(*,*)num

    call dsp(num, sq_rt, c, sq)

end program

subroutine dsp(n, sqr_root, cube, square)
    implicit none

    integer :: I
    !REAL :: J
    integer :: n, cube, square
    real :: sqr_root

    write(*,*) "        Number   ", "     Square Root", "            Square", "        Cube"
    write(*,*) "        ______   ", "     ___________", "            ______", "        ____"
    do i = 1,n
        !J = I
        write(*,*) i, "        ", sqrt(REAL(I)), i*i, i*i*i
    end do
end subroutine
