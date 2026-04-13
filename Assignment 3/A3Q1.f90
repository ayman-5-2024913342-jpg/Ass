PROGRAM GAUSSIAN_ELIMINATION
  IMPLICIT NONE
  INTEGER :: i, j, k, n, p
  REAL, dimension(4,5) :: MatA

  OPEN(10, FILE='input.txt', STATUS='OLD')
  OPEN(20, FILE='output.txt', STATUS='REPLACE')

  !READ(10,*) n
  DO i = 1, 4
     READ(10,*) (mata(i,j), j=1,4+1)
  END DO


  do i = 1, 4-1

     p = i
     do k = i, 4
        if (MatA(k,i) /= 0.0) then
           p = k
           exit
        end if
     end do

  end do

  if (mata(p,i) == 0.0) then
        print *, 'No unique solution exists'
        stop
  end if

END PROGRAM GAUSSIAN_ELIMINATION
