      program rocfft_example
!
      use iso_c_binding
      use omp_lib
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use m_precision
!
      implicit none

      integer(c_size_t), parameter :: N = 32
      integer(c_size_t), parameter :: Nbytes = N*8*2

      type double2
        real(kind = kreal) :: x
        real(kind = kreal) :: y
      end type double2

      type(double2), allocatable, target :: hx(:)
      type(c_ptr) :: dx = c_null_ptr
      type(c_ptr) :: plan = c_null_ptr
      integer(c_size_t), allocatable, target :: lengths(:) 
      integer(c_size_t), parameter :: one = 1
!
      integer(c_size_t), parameter :: N_real = 2*N
      real(kind = kreal), allocatable, target :: x_org(:)
      real(kind = kreal), allocatable, target :: f_org(:)
!
      real(kind = kreal), allocatable, target :: x_real(:)
      real(kind = kreal), allocatable, target :: f_real(:)
!
      integer(c_size_t), allocatable, target :: l_real(:)
!
!
      integer(kind = kint) :: i
      real(kind = kreal) :: start, finish, elapsed
      real(kind = kreal) :: error
      real(kind = kreal), parameter :: error_max = epsilon(error)

      write(*,"(a)",advance="no") "-- Running test 'rocFFT' (Fortran 2003 interfaces) - "

      call rocfftCheck(rocfft_setup())

      allocate(lengths(3))
      lengths(1) = N

      allocate(hx(N))
      hx(:)%x =  1
      hx(:)%y = -1
!
      write(*,*) 'Source'
      write(*,*) 'i, hx(i)%x, hx(i)%y'
      do i = 1, N
        write(*,*) i, hx(i)%x, hx(i)%y
      end do
!
      call hipCheck(hipMalloc(dx,Nbytes))
      call hipCheck(hipMemcpy(dx,c_loc(hx(1)),Nbytes,                   &
     &                        hipMemcpyHostToDevice))

      start = OMP_GET_WTIME()
      call rocfftCheck(rocfft_plan_create(plan,                         &
                                          rocfft_placement_inplace,     &
                                 rocfft_transform_type_complex_forward, &
                                          rocfft_precision_double,      &
                                          one, c_loc(lengths(1)),       &
                                          one, c_null_ptr))

      call rocfftCheck(rocfft_execute(plan, dx,                         &
     &                                c_null_ptr, c_null_ptr))

      call hipCheck(hipDeviceSynchronize())
      elapsed = OMP_GET_WTIME() - start

      call rocfftCheck(rocfft_plan_destroy(plan))

      call hipCheck(hipMemcpy(c_loc(hx(1)),dx,Nbytes,                   &
     &                        hipMemcpyDeviceToHost))
      call hipCheck(hipFree(dx))
!
      write(*,*) 'Converted'
      write(*,*) 'i, hx(i)%x, hx(i)%y'
      do i = 1, N
        write(*,*) i, hx(i)%x, hx(i)%y
      end do
!
! Using the C++ version of this as the "gold".
! first components were \pm 16 and the remaining componenents
! were zero, so the sum of each component pair should be zero
      do i = 1,N
         error = abs(hx(i)%x+hx(i)%y)
         if(error > error_max)then
            write(*,*) "FAILED! Error = ", error,                       &
     &                 "hx(i)%x = ", hx(i)%x, "hx(i)%y = "
         end if
      end do
!
      deallocate(hx)
      deallocate(lengths)
!
!
      allocate(l_real(1))
      l_real(1) = N_real
!
      allocate(f_org(N_real))
      f_org(1:N_real) = 0.0d0
      f_org(10) =   1.0d0

      allocate(f_real(N_real))
      f_real(1:N_real) = f_org(1:N_real)

      allocate(x_real(N_real))
      x_real(1:N_real) = f_real(1:N_real)

      call hipCheck(hipMalloc(dx,Nbytes))
      call hipCheck(hipMemcpy(dx,c_loc(x_real(1)),Nbytes,               &
     &                        hipMemcpyHostToDevice))

      start = OMP_GET_WTIME()
      call rocfftCheck(rocfft_plan_create(plan,                         &
                                          rocfft_placement_inplace,     &
                                  rocfft_transform_type_real_inverse,   &
                                          rocfft_precision_double,      &
                                          one, c_loc(l_real(1)),        &
                                          one, c_null_ptr))

      call rocfftCheck(rocfft_execute(plan, dx,                         &
     &                                c_null_ptr, c_null_ptr))

      call hipCheck(hipDeviceSynchronize())
      elapsed = OMP_GET_WTIME() - start

      call rocfftCheck(rocfft_plan_destroy(plan))

      call hipCheck(hipMemcpy(c_loc(x_real(1)),dx,Nbytes,               &
     &                        hipMemcpyDeviceToHost))
      call hipCheck(hipFree(dx))

      write(*,*) 'Converted'
      write(*,*) 'i, x_real'
      do i = 1, N_real
        write(*,*) i, x_real(i)
      end do
!
      call rocfftCheck(rocfft_cleanup())

      write(*,*) "PASSED!"
      write(*, '("Time for forward transform: ",1pE16.6e3)') elapsed
!
      end program rocfft_example