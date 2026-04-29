program A3Q5
    implicit none
    integer::i,j,k,countrel=0,coumt=2
    real::s,rel,tol=1.0e-3,omega=1.1
    real,dimension(4,5)::aug
    real,dimension(4)::a,a1
    character,dimension(4)::variables
    variables(1)='w'
    variables(2)='x'
    variables(3)='y'
    variables(4)='z'
    a=0
    open(10, file='A3Q5_in.txt')
    open(11, file='A3Q5_out1.txt')
    open(12, file='A3Q5_out2.txt')
    open(13, file='A3Q5_out3.txt')
    do i=1,4
        read(10,*)aug(i,:)
    end do

    do i=1,4
        s=0
        do j=1,4
            if(j==i)cycle
            s=s+abs(aug(i,j))
        end do
        if(abs(aug(i,i))<=s)then
            write(*,*)'diagonally not dominant'
            stop
        end if
    end do

    write(11,*)1,a
    do while(countrel<4)
        do i=1,4
            s=aug(i,5)
            do j=1,4
                if(i==j)cycle
                s=s-a(j)*aug(i,j)
            end do
            a1(i)=s/aug(i,i)
        end do
        write(11,*)coumt,a1
        countrel=0
        do i=1,4
            rel=abs((a(i)-a1(i))/a1(i))
            if(rel<tol)then
                countrel=countrel+1
            end if
        end do
        a=a1
        coumt=coumt+1
    end do
    write(11,*)''
    write(11,*)'final result:'
    do i=1,4
        write(11,*)variables(i),' =',a(i)
    end do

    a=0
    coumt=2
    countrel=0
    write(12,*)1,a
    do while(countrel<4)
        a1=a
        do i=1,4
            s=aug(i,5)
            do j=1,4
                if(i==j)cycle
                s=s-a(j)*aug(i,j)
            end do
            a(i)=s/aug(i,i)

        end do
        write(12,*)coumt,a
        countrel=0
        do i=1,4
            rel=abs((a(i)-a1(i))/a(i))
            if(rel<tol)then
                countrel=countrel+1
            end if
        end do
        coumt=coumt+1
    end do
    write(12,*)''
    write(12,*)'final result:'
    do i=1,4
        write(12,*)variables(i),' =',a(i)
    end do

    a=0
    coumt=2
    countrel=0
    write(13,*)1,a
    do while(countrel<4)
        a1=a
        do i=1,4
            s=aug(i,5)
            do j=1,4
                if(i==j)cycle
                s=s-a(j)*aug(i,j)
            end do
            a(i)=(1.0-omega)*a(i)+omega*(s/aug(i,i))

        end do
        write(13,*)coumt,a
        countrel=0
        do i=1,4
            rel=abs((a(i)-a1(i))/a(i))
            if(rel<tol)then
                countrel=countrel+1
            end if
        end do
        coumt=coumt+1
    end do
    write(13,*)''
    write(13,*)'final result:'
    do i=1,4
        write(13,*)variables(i),' =',a(i)
    end do
end program
