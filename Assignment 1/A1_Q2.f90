program main
    implicit none
    integer :: c,f

    PRINT*, "Temperature Conversion Tabe:"
    print*, "        Celsius  ", "    Farenheit "
    print*, "        _______  ", "    __________"
    do c = -20,50,10
        call conv_temp(c,f)
        print*, c, f
    end do

end program

subroutine conv_temp(celsius, farenheit)
    implicit none

    integer :: celsius, farenheit

    farenheit = (celsius * (180/100)) + 32
end subroutine
