module fact
 ! Module wich computes factorial of n, using the recursive function method
  public :: f
  contains

    recursive function f(n) result (factorial_result)
     integer, intent (in) :: n
     real(kind=16) :: factorial_result !real output 

     if (n <= 0) then
        factorial_result = 1
     else
        factorial_result = n*f(n-1)
     end if
    end function f

end module fact

program exp_taylor
   use ISO_FORTRAN_ENV !I insert this module call in each program to show compiler infos
   use fact !call of the factorial recursive function module

  implicit none

  integer :: k,N, ierr
  integer, parameter :: dp = selected_real_kind(13)
  real(kind=8) :: x, sum_k, term, relative_error, abs_error, counter, relative_error_0
  character(100) :: filename

  ! Show compiler infos
   write(*,*)" Compiler version: ",compiler_version() 
   write(*,*)" Compiler options: ",compiler_options()
  
  print *, " Program to calculate the taylor expansion of exp(-x) "
   
  !Input value of x 
  !print *, "Value of x:"
  !read *, x
  
  ! Initialization of global variables
  N = 1000 !maximum number of steps
  abs_error = 1.0e-15
  
  ! Define the output file name
  filename = "exp_taylor_data_2.txt"
  
  ! Open the output file
  open(unit=10, file=trim(filename), status='replace', iostat=ierr, position="append", action="write")
  !open(unit=10, file=trim(filename),  iostat=ierr, position="append", action="write")
  if (ierr /= 0) then
    write(*,*) "Error opening the output file." !I insert these lines to avoid errors in the opening of the output file
    stop
  end if
  
  ! Print table header to the output file
  write(10, *) "x     ", "i(no. of terms)    ", "sum   ", "|sum-exp(-x)|/exp(-x)"
    
  ! Loop over different values of x
  counter = 1
  write(*,*) "x, k, sum_k, exp(-x), relative_error"
  do while (counter <= 150)
    x = counter * 0.1_dp  !0.1 step increment of x
    
    ! Initialization of variables within the x cycle
    sum_k = 0.
    relative_error = 0.
    relative_error_0 = 1.
    term = 0.
  
    ! Main cycle do wich calculate the Taylor series of e^(-x) 
    do k = 0, N
      ! (k-1)-th relative error of the partial sum with respect to the built-in exp function 
      relative_error_0 = abs(sum_k - exp(-x)) / exp(-x)
      term = (-x)**k / real(f(k),KIND=8)
      sum_k = sum_k + term
      
      ! k-th relative error of the partial sum with respect to the built-in exp function 
      relative_error = abs(sum_k - exp(-x)) / exp(-x)
      
	   print *, x, k , sum_k, exp(-x), relative_error 
      
       !if(relative_error < abs_error)  then 	 !Previous conditional exit
        if (relative_error_0 == relative_error) then 
        !My conditional exit is when relative error becomes "constant": 
        !in this way you can find the optimal relative error too
          write(*,*) x, k, sum_k, exp(-x), relative_error
          write(unit=10,fmt=*) x, k -1, sum_k, exp(-x), relative_error
        exit !conditional exit from the cycle
        endif

    end do
    
    counter = counter + 1
  end do

 ! Close the output file
  close(10)
      
end program exp_taylor
