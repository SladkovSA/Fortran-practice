!! Амонгелдиев Шихназар Б18-101 
!! Вариант №13
!!   Table of variables 
!! S   -  analit value of function 
!! Sn  -  partial sum of Fk functions (1:n)
!! R   -  remainder
!! Rn  -  partial sum of Fk-functions (n+1:2n)
!! c,d -  interval limits
!!M   -  serial number of 0x line segment



program v13
    real S, Sn, R, Rn, eps(5), Xm, c, d, L, m
    integer i, mm,q
    1 format(i2,f6.2,1x,i3,1x,2(e11.4,1x),4(e10.3,1x))
10  print*, '-infinity< x <+infinity'
    print*, 'input interval for x [c,d] and M'
    print '(a, $)', 'c='
    read*, c
    print '(a, $)', 'd='
    read*, d
    print '(a, $)', 'M='
    read*, m
    L = (d - c) / m
    eps = 0
    eps(1) = 1.E+9
    
    print*, ' Select the criterium'
    print*, '1. n = N = eps(1)'
    print*, '2. abs(R) < eps(2)'
    print*, '3. abs(Rn) < eps(3)'
    print*, '4. abs(R/Fn) < eps(4)'
    print*, '5. abs(Rn/Fn) < eps(5)'
    print*, 'Enter number of the criterium (i) and eps(i)  by spase'
    read*, i, e
    eps(i) = e
    
    print '(7x, a)', 'Functional series sin(x)'
    print '(a2, 1x, a4, 2x, a3, 2(3x, a4, 4x), 4(4x, a7))', 'm', 'x(m)', 'n', 'S', 'Sn', '|R|', '|Rn|', '|R/Fn|', '|Rn/Fn|'
    mm = 1
    do j = 0, m
        Xm = c + L*(j)
        S = sin(Xm)
        Sn = Xm
        call calPrts(Xm,S,Sn,n,R,Rn,eps)
        print 1,mm, Xm , n , S , Sn, R , Rn, R/Sn, Rn/Sn
        mm=mm+1
    enddo
    print*, 'to continue input 1, to end program input 0'
    read*, q
    if (q==1) goto 10
end program v13

SUBROUTINE calPrts(Xm,S,Sn,n,R,Rn,eps)
    real x1, x2, eps(5)
    x1 = Xm
    do n=1,1.E+6
        x1 = fk(Xm,x1,n)
        Sn = Sn + x1
        R=abs(abs(S) - abs(Sn))
        x2 = fk(Xm,x1,n)
        Rn = 0
        do i=n+1,2*n
            Rn = Rn + x2
            x2 = fk(Xm,x2,i)
        enddo
        if((n == eps(1)) .or. (abs(R)<eps(2)) .or. (abs(Rn)<eps(3)).or. (abs(R/Sn)<eps(4)) .or. (abs(Rn/Sn)<eps(5))) exit
    enddo
end

real function fk(Xm,x1,n)
    INTEGER n
    real Xm, x1
    fk = -(x1*((Xm)**2))/((2*n)*(2*n+1))
    return
end
