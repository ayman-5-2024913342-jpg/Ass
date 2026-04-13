program gauss_elimination
  implicit none
  integer :: n=4, i, j, k, p
  real, allocatable :: A(:,:)
  real :: factor
  real, allocatable :: x(:)
  open(10, file="A3_Q1_in.txt")

  ! Input number of unknowns/equations
  !print *, 'Enter number of unknowns/equations:'


  allocate(A(n,n+1))
  allocate(x(n))


  print*,"Augmented matrix: "

    do i = 1, n
        read(10, *) A(i, :)
    end do

  !read(10, *) A

  do i = 1,4
    do j = 1,5
        write(*, '(f10.4)', advance='no') A(i,j)
    end do
    print*, " "
  end do
  ! Input augmented matrix
  !print *, 'Enter augmented matrix A (n x (n+1)):'
  !do i = 1, n
  !!   do j = 1, n+1
   !!!     read *, A(i,j)
   !  end do
  !end do

  ! Step 1: Pertial pivot
  do i = 1, n-1
     ! Step 2: Find pivot row
     p = i
     do k = i, n
        if (A(k,i) /= 0.0) then
           p = k
           exit
        end if
     end do

     if (A(p,i) == 0.0) then
        print *, 'No unique solution exists'
        stop
     end if

     ! Step 3: Swap rows if needed
     if (p /= i) then
        A([i,p],:) = A([p,i],:)   ! Fortran 2003+ array swap
     end if

     ! Step 4: Eliminate below pivot
     do k = i+1, n
        factor = A(k,i) / A(i,i)
        do j = i, n+1
           A(k,j) = A(k,j) - factor * A(i,j)
        end do
     end do
  end do

  ! Check last pivot
  if (A(n,n) == 0.0) then
     print *, 'No unique solution exists'
     stop
  end if

  ! Back substitution
  x(n) = A(n,n+1) / A(n,n)
  do i = n-1, 1, -1
     x(i) = A(i,n+1)
     do j = i+1, n
        x(i) = x(i) - A(i,j) * x(j)
     end do
     x(i) = x(i) / A(i,i)
  end do

  ! Output solution
  print *, 'Solution vector:'
  do i = 1, n
     print *, 'x(', i, ') = ', x(i)
  end do

end program gauss_elimination
