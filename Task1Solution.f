program zadanie1
 real a,b,c, x1, x2
 integer ip,k,p
 k=1
 print*, 'Enter the values a,b,c :'
 Read*, a, b, c
 print*, 'vashi korni kompleksnie? 2 - yes, 1 - no'
 Read*, ip
 if (ip==2) then 
    print*, 'Enter the real and imaginary parts x, y : '
    Read*, x1, x2
 else
    print*, 'Enter the values x1 x2  :'
    Read*, x1, x2
 end if
    
    
 print*, '                      Menu'
 print*, ' 1 - Call the programm of solution of the quadratic equation'
 print*, ' 2 - Call the programm of calculation of the nevyazok'
 print*, ' 3 - Call the programm of calculation of the coefficients'
 print*, ' 4 - End'
 print*, ' Enter the number'
 read*, p
 select case (p)
    case (1)
       call kvad (a,b,c,ip,x1,x2)
    case (2)
       
    case (3)
       call coeffic (a,b,c,ip,x1,x2)
    case (4) 
       go to 1
 end select
 
 
  1 continue
  read*,
 end program
 
 
 nevyazki (a,b,c,ip,x1,x2) !ðàñ÷åò íåâÿçîê
    real a,b,c,x1,x2
    integer ip
    
 
 
 subroutine coeffic (a,b,c,ip,x1,x2) !ðàñ÷åò êîýôôèöèåíòîâ
    real a,b,c,x1,x2
    integer ip
    if (ip==2) then 
       b= -2*a*x1
       c= a*(x1**2 + x2**2)
       print*, 'b=', b, 'c=', c
    else
       b=a*(-x2-x1)
       c= a*x1*x2
       print*, 'b=', b, 'c=', c
    end if
 
  end subroutine coeffic
 
 
 
 subroutine kvad (a,b,c,ip,x1,x2) !ðåøåíèå êâàäðàòíîãî óðàâíåíèÿ
     real a,b,c,x1,x2
     integer ip
 if (a==0) then
  if(b==0) then
   if (c/=0) then 
    print*, 'no solution'
    go to 2
   else !åñëè à, b, ñ = 0
   print*, 'infinite number of solutions'
   end if
  else !åñëè b!= 0 :
   if (c==0) then
      x1=0
      print*, 'x=0'
   else ! òå åñëè ñ!=0
    x1=(-c)/b
    print*, 'only one solution x=',x1
   end if
  end if
 end if
 if (a/=0) then
  if (c==0) then
   x1=0
   x2=(-b)/a
   print*, 'x1=', x1, 'x2=',x2
  else ! à è ñ /= 0
   d=b**2-4*a*c
   if (d>0) then
    x1= ((sqrt(d))-b)/(2*a)
    x2= (-b-(sqrt(d)))/(2*a)
    print*, 'solution: x1=',x1,'x2=',x2
   end if
   if (d==0) then
    x1=(-b)/(2*a)
    print*, ' Only solution x=', x1
   end if
   if (d<0) then
    x1=(-b)/(2*a); 
    x2=(sqrt(-d))/(2*a)
    print*, 'x= ', x1, '+ i*', x2
   end if
  end if
 end if
 2 continue
 end subroutine kvad
