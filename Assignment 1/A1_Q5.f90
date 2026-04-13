program main
    implicit none
    integer :: i, n, CNT_D=0;
    !a = 20

    do i = 1, 20
        CNT_D = 0
        call isPrime(i, cnt_d);
    end do


end program

subroutine isPrime(p, divs_cnt)
    implicit none

    integer :: p, i;
    integer :: divs_cnt

    if (p < 2) then
        write(*,*) p, "is not prime"
        return;
    else
        do i = 2, p
            if (mod(p, i) == 0) then
                divs_cnT = divs_cnt + 1
            end if
        end do
    end if

    !write(*,*)Divs_Cnt
    if (divs_cnt == 1) then
        WRITE(*,*) p, "is a PRIME"
    else
        write(*,*) p, "is not a prime"
    end if
end subroutine
