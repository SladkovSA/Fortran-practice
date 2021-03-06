!Программа реализует операция с метрицами и векторами в соответствии с выбранным ппунктом
!Av,Bv,Cv - Вектора с динамичными размерами
!Am,Bm,Cm - Матрицы с динамиными размерами 
!N,M,K    - Параметры размера матрицы
!Программу написал студент группы 

program zadanie6
    integer :: menu,flag
    integer N,M,K
    real ,dimension(:,:),allocatable :: Am,Cm,Bm
    real ,dimension(:),allocatable :: Av,Bv,Cv
    1 format(1x,I2,$)
    10 continue
    print*,'1. Scalar product of vectors'
    print*,'2. Product of matrix and vector'
    print*,'3. Product of matrixes'
    print*,'4. Transpose of a matrix'
    print*,'5. Inversion of a matrix'
    print*,'6. Slices of a matrix or array'
    print*,'7. End'
    print *,'Vvedite nomer'
    read *,i
    select case(i)
        case(1)
            print *,"Input size of vectors:"
            read *, N
            allocate (Av(N))
            allocate (Bv(N))
            call skal(N,Av,Bv)
            deallocate(Av)
            deallocate(Bv)
        case(2)
            print *,"Input size of Matrix N*M:"
            read *,N,M
            print *,N,M
            allocate (Am(N,M))
            allocate (Bv(M))
            allocate (Cv(N))
            call MnaV(N,M,Am,Bv,Cv)
            print *,"Result:"
            print *,Cv
            deallocate(Am)
            deallocate(Bv)
            deallocate(Cv)
        case(3)
            print *,"Input size of two matrix N*M and M*K:"
            read *, N,M,M,K
            allocate(Am(N,M))
            allocate(Bm(M,K))
            allocate(Cm(N,K))
            call MnaM(N,M,K,Am,Bm,Cm)
            print *,"Result:"
            call VIVOD(N,K,Cm)
            deallocate(Am)
            deallocate(Bm)
            deallocate(Cm)
        case(4)
            print *, "Input size of matrix N*M:"
            read *,N,M
            allocate(Am(N,M))
            allocate(Bm(M,N))
            print *,"Input Matrix N*M:"
            call VVOD(N,M,Am)
            call Transpon(N,M,Am,Bm)
            call VIVOD(M,N,Bm)
            deallocate(Am)
            deallocate(Bm)
        case(5)
            print *, "Input size of matrix N*N:"
            read *,N
            allocate (Am(N,N))
            allocate (Bm(N,N))
            print *,"Input matrix N*:"
            call VVOD(N,N,Am)
            call INVERT1(Am,Bm,N)
            print *,"Result:"
            call VIVOD (N,N,Bm)
            deallocate(Am)
            deallocate(Bm)
        case(6)
            print *,"Input size of matrix N*N:"
            read *,N
            allocate (Am(N,N))
            call treyg(N,Am)
            deallocate(Am)
        case(7)
        print *,"Exit"
        case default
            print *,'Vvedite nomer zanovo'
    end select
    print *,'to continue input 1, to end program input 0'
    read *, g
    if (g==1) goto 10
end program zadanie6

subroutine VVOD(N,M,A)
    real A(N,M)
    do i=1,N
        print *,'Enter the number of string: ',i
        read *, a(i,:)
    end do     
    END 

subroutine VIVOD(N,M,A)
    1 format(1x,F6.3,$)
    real A(N,M)
    do i=1,N
        do j=1,M
            write(*,1) a(i,j)
        end do
        print *, ' '
    end do
END          

subroutine SKAL(N,A,B)  ! SKAL=(A,B)
    real A(N),B(N),C(N)
    print *,"Enter first vector:"
    read *,(a(i),i=1,N)
    print *,"Enter second vector:"
    read *,(b(i),i=1,N)
    do i=1,N
        c(i)=a(i)*b(i)
    end do
    print *, "Skalyarnoe proizvedenie vektorov ravno"
    print *, c
END    

subroutine MnaV(N,M,A,B,C) ! C=A*B
    real A(N,M),B(M),C(N)
    print *,"Enter Matrix NxM:"
    call VVOD(N,M,A)
    print*,"Enter Vector size of M:"
    read *,(b(i),i=1,M)
    print *,B
    do i=1,N
        c(i)=0
        do j=1,M
           c(i)=c(i)+a(i,j)*b(j)
        end do
    end do
END

subroutine MnaM(N,M,K,A,B,C) ! C=A*B
    real A(N,M),B(M,K),C(N,K)
    print *, "Input first Matrix NxM:"
    call VVOD(N,M,A)
    print *, "Input second Matrix MxK:"
    call VVOD(M,K,B)
    do i=1,N
        do j=1,K
            c(i,j)=0
        end do
    end do
    print*, 'N =, K =', N, K
    do i=1,N
        do j=1,K
            do t=1,M
                c(i,j)=c(i,j)+a(i,t)*b(t,j)
            end do
        end do
    end do
END

subroutine Transpon(N,M,A,B)
    real A(N,M),B(M,N)
    do i=1,N
        do j=1,M
            b(j,i)=a(i,j)
        end do
    end do
end

subroutine INVERT1(A,A1,N)! A1=INV(A)
      IMPLICIT INTEGER*4(I-N)
      DIMENSION A(N,N),A1(N,N)
      A1=A
      EPS=1E-20
      do 21 I=1,N
      Q=A1(I,I)
      Q1=Q
      if(Q.LT.0.) Q1=-Q1
      if(Q1.GE.EPS) GO TO 3
	PRINT*,'matritsa A virojdennaia'  
       READ*; STOP
    3 A1(I,I)=1.
      do 22 K=1,N
   22 A1(I,K)=A1(I,K)/Q
      do 25 J=1,N
      IF(I.EQ.J) GO TO 25
      Q=A1(J,I)
      A1(J,I)=0.
      DO 26 K=1,N
      A1(J,K)=A1(J,K)-A1(I,K)*Q
   26 continue
   25 continue
   21 continue
      return
end



subroutine treyg (N,A)
   1 format(1x,F6.3,$)
 
   real A(N,N)
   print *,"Enter Matrix NxN:"
   call VVOD(N,N,A)

   do i=1,N
        j=1
        do while (j<=i)
           write(*,1) a(i,j)
           j=j+1
        enddo
        print *, ' '
    enddo
        
end
