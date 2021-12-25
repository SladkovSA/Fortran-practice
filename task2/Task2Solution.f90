program Zadanie3
   real Sn, Rn, a, RS, S, R, RSn, RnSn
   integer m, k,n,p
   1 continue
   print*, 'Enter the value m'
   read*, m
   Sn=0
   Rn=0
   S=(1-log10(2.))/2
   k=1
  print*, '         n       abs(R)       S(n)              abs(R(n))        abs(an)           abs(R/S(n))       abs(Rn/Sn) '
  do while (k<=m)
   call chastichnie (m,k,Sn,Rn,an)
    R= S-Sn
    n=2**(k+1)
    k=k+1
    RSn= R/Sn
    RnSn= Rn/Sn
    print*, n,   abs (R),        Sn      , abs (Rn)  ,   abs (an),  abs (RSn),   abs (RnSn)
  ENDDO
  
  print*, ' 1 -  restart,  2 - exit'
  read*, p
  if (p==1) then 
     go to 1
  endif
end program Zadanie3





subroutine chastichnie (m,k,Sn,Rn,an)
   integer k,m,p
   real Sn, Rn,an
   p=m+k
      an= ((-1.)**(k+1))  /  ( (2*k-1) *2*k* (2*k+1) )
      Sn= Sn+ ((-1.)**(k+1))  /  ( (2*k-1) *2*k* (2*k+1) )
      Rn= Rn+ ((-1.)**(p+1))  /  ( (2*p-1) *2*p* (2*p+1) )
    return
end subroutine
