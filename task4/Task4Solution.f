!Программа реализует вычисление функций I0 и K0 для заданного интервала 
!  K, KK       -     Функции, вычисляемые в пунктах а и b соответственно
!  K0           -    Теоритическое значние искомой функции
!  с d         -     Границы задаваеиого интервала 
!  M           -     Количество разбиений заданного интервала
!  L           -     Длина отрезков после разбиения 
!Программу выполнил студентр группы 

program zadanie5
   real*8 K1, K2, K, Q, z, NF, KK, V, t, dela, delb, c,d, delaS, delbS, L
   REAL*8 GAM, I, I0, x, K0, An,Syn,San
   INTEGER*8   M,mm
   2 continue
   print*, 'Enter the values c and d'
   read*, c,d
   5 format (i2,f5.1, (1x,f5.2), 3(1x,1pe10.2) 3(1x,1pe8.1)  (3x,1pe8.1)  )
   
   if ((c >= 0) .and. (c < d)) then
        print*, 'Correct the interval limits'
   else
        print*, 'invalid interval boundaries'
        go to 1
   endif
   
   print*, 'Enter the value M'
   read*, M
   L = (d-c)/M
   x=c
   mm=0
    print*, 'enter the value "n"'
        read*, Q
        print*, 'm  x(m)   N       S         Sa         Sb       del(a)  del(b)  del(a)/|S| del(b)/|S|'
   GAM=0.5772156649D0
   do while (x<=d)

   
        An=1;Sk=0; Syn=0;
      !первая сумма
        do i=1,Q
            Sk=Sk+1D0/i
            An=An*((x/2)**2)/((i)**2)
            Syn=Syn+An*Sk
        end do
        
        
      K = Syn - (GAM + DLOG(x/2)) * I0(x)
      
      
      
      !б)
      V = EXP(-X)*SQRT(acos(-1.)/(2*x))
      t = 1/(8*x)
      
      KK = V*(1-t*(1-t*(4.5-t*37.5)))
      
      
      

     dela = abs(K0(x)-K)
     delb = abs(K0(x)-KK)
     delaS=abs (dela/K0(x))
     delbS=abs (delb/K0(x))
     print 5, mm, x  ,Q, K0(x)  ,  K  ,   KK, dela, delb, delaS, delbS
     x = x+ L
     mm=mm+1
   ENDDO
   
   print*, ' 1 -  restart,  2 - exit'
   read*, p
   if (p==1) then 
      go to 2
   endif
 
1 continue
read*
end program zadanie5


     REAL*8 FUNCTION I0(z)
        REAL*8 z, AN
    I0=1
    AN=1
    DO N=1,2*z+20 
        AN=AN*(z/2)**2/N**2 
        I0=I0+AN;
    END DO
    END  FUNCTION I0
    
    
    
       REAL*8 FUNCTION K0(X)
        REAL*8 X,AN,BN,GAM,I0
        GAM=0.5772156649D0;
    K0=0 
    AN=1 
    BN=0
    DO N=1,2*X+20 
        AN=AN*(X/2)**2/N**2
        BN=BN+1D0/N 
        K0=K0+AN*BN 
    END DO  
    K0=K0-I0(X)*(GAM+DLOG(X/2))
    END  FUNCTION K0
