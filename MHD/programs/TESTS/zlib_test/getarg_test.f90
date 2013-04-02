      program arg_test
!
      use getarg_kemo
!
      implicit none
      integer :: iargc
      integer :: i, count
      character(len=80) :: argc
      count = iargc_kemo()
      do i = 0, count
        call getarg_k(i, argc)
        print *, i, trim(argc)
      end do
      end program