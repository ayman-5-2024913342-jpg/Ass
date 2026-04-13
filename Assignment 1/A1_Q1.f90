program main
    implicit none

    real :: m, t;
    real :: a;

    read(*,*) m

    call cnt_miles(m, t, a)

    print*, "total cost: ", t, "$"
    print*, "avg cost: ", A, "$"
end program

subroutine cnt_miles(miles, total_cost, avg_cost)
    implicit none

    real :: miles, total_cost
    real :: avg_cost

    if (miles <= 100) then
        total_cost = total_cost + miles * 50
    elseif (miles < 300) then
        total_cost = (100*50) + (miles-100) * 30
    else
        total_cost = (100*50) + (200*30) + (miles-300) * 20
    end if
    total_cost = total_cost /100
    avg_cost = total_cost / miles

end subroutine
