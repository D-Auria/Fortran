!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! A generalization of the program which calculates the factorial using a recursive 
! function for large numbers;
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 

module fact

public :: f
contains

recursive function f(n) result (factorial_result)
   real, intent (in) :: n
   !integer, parameter :: RegInt_K = selected_int_kind(18)
   real(kind=16) :: factorial_result !real output 

   if (n <= 0) then
      factorial_result = 1.
   else
      factorial_result = n*f(n-1)
   end if
end function f

end module fact

program test_factorial

   use ISO_FORTRAN_ENV 
   use fact
   implicit none
  
   integer :: n,i
   real :: r,ind
   real(kind=8) :: d
   real(kind=16) :: t
      
   write(*,*)" Compiler version: ",compiler_version() 
   write(*,*)" Compiler options: ",compiler_options()
   
   print *, "integer n?"
   read *, n !try n=1800
   do i=0,n 
   ind=real(i)
   r=real(f(ind)) !we promote to reals the result of f()
   d = real(f(ind),KIND=8)
   t = real(f(ind), KIND=16)
   print *, i, r, d, t
   end do
   
end program test_factorial

