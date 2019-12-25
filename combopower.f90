     !this code uses both bisectionmethod and newton rhapson to find root of a given function over a given interval
     !interval is decided by a and b & function is called from subroutine
     program combopower
              logical kk
              logical ss
              j=0
              pl=0
              a=1.5e0
              b=4.5e0
              kk= .true.
              ss=.true.
           
              do while (ss)
              pl=pl+1
              
       
                      do while (kk)
            
                      call func(a,fa,s)
                      call func(b,fb,s)

                      if ((fa)*(fb).gt.0.e0) then 
                               kk=.false.
                       else 
       
                      call bisection(a,b,c)
                      call newton(c,r)
                      
                      write(*,*)'guess is',c, 'root is',r
                      end if
                     
                      if (a.gt.10) ss=.false.
                      a=a+1.5e0
                      b=b+1.5e0
                      j=j+1

                      end do
              if(j.eq.20) ss=.false.        
              a=a+1.5e0
              b=b+1.5e0
              j=j+1

              end do

             end program combopower 

             subroutine bisection(a,b,c)
                     logical ll
                     ll=.true.
                     eps=1.e0
                     i = 0
                     !a1=0.e0
                     !b1=0.e0
                     a1=a
                     b1=b
             
                   
                      do while(ll)
               
                      i = i + 1
                      c=(a1+b1)/2.e0
                      

                      call func(c,y,q)
                      call func(a1,y1,q)
                      call func(b1,y2,q)

                      if ((y*y1).gt.0.e0) a1=c
                      if ((y*y2).gt.0.e0) b1=c
                      if (abs(y).lt.eps) ll=.false.
                      !a1=a
                      !b1=b

                      end do
                      print*, ' from bisection ' , i
                      return
                     end
            subroutine newton(c,r)
                    logical lk
                    lk=.true.
                    eps=1.e-6
                    a=c
                    i = 0
                   do while(lk)
                    i = i + 1
                    call func(a,fa,dfa)
                    b=a-(fa/dfa)
                    call func(b,fb,dfb)
                    if (abs(fb).lt.eps) lk=.false.
                    a=b
                    r=b
                   end do
                   print*, ' from newton ', i
                   return 
                   end 
           subroutine func(x,f,df)

                      f=sin(x)
                      df=cos(x)
                      return
                   
                       end 







