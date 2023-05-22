program mpi_integral
  use mpi
  implicit none
                                                                                                                                               
  integer :: ierr, rank, size, provided
  integer, parameter :: n = 1000 ! 
  !Number of intervals
  real(kind=8) :: a, b, h, local_a, local_b, local_result, result

  ! Initialize MPI     
  
  call MPI_Init_thread(MPI_THREAD_FUNNELED, provided, ierr)                                                                                         
  ! Get the rank and size of the MPI communicator   
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)       

  ! Define the integration limits   
  a = 3.14
  b =  6.28
  ! Calculate local range and number of intervals for each process     
  local_a = a + rank * (b - a) / size
  local_b = local_a + (b - a) / size
  ! Perform local integration on each process
  local_result = integrate(local_a, local_b)
  ! Reduce the local results to obtain the final result on rank 0     
  call MPI_Reduce(local_result, result, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
  ! Print the result on rank 0    
  if (rank == 0) then
    print *, 'Definite integral from ', a, ' to ', b, ': ', result
  end if
  ! Finalize MPI     
  call MPI_Finalize(ierr)



contains

real(kind=8) function func(x)
    real(kind=8), intent(in) :: x
    func =  (1-cos(x))/((x-sin(x))*(x-sin(x))) ! Example 
end function func



! Local integration     
real(kind=8) function integrate(local_aa, local_bb)                                                                                                                    
  real(kind=8), intent(in) :: local_aa, local_bb
  integer :: i
  real(kind=8) :: x, sum
  sum = 0.d0
  h = (b - a) / n
  do i = 0, n-1
    x = local_aa + (i + 0.5) * h
    sum = sum + func(x)
  end do
  integrate = sum * h
end function integrate


end program mpi_integral
