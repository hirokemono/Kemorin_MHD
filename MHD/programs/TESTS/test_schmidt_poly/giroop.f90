      program gaunt
!
      use m_precision
!
      use m_integrals_sph_nonlinear
      use cal_int_sph_nonlinear
!
      implicit none
!
      integer(kind = kint) :: ltr, jmax, np_smp = 1
      integer(kind = kint), allocatable :: idx_gl(:,:)
!
      integer(kind = kint)  :: j1, l, m, j
!
      character(len=kchara), parameter :: file_name='hermony.dat'
      integer(kind = kint), parameter :: iflag_debug = 1, id_file = 9
!
!*  ----  input truncation level ----------
!*
      write (6,*) ' num. of threads'
      read (5,*) np_smp
      write (6,*) ' input truncation number'
      read (5,*) ltr
      jmax = ltr * (ltr+2)
!*
!*  ----------   reset the numbers  --------------
!*
      allocate(idx_gl(jmax,3))
!
      do l = 1, ltr
        do m = -l, l
          j = l*(l+1) + m
          idx_gl(j,1) = j
          idx_gl(j,2) = l
          idx_gl(j,3) = m
        end do
      end do
!
!* ------ caliculate of Gaunt integral with hermmonics ------
!*
      call s_cal_int_sph_nonlinear(iflag_debug, np_smp, ltr, jmax,      &
     &    idx_gl)
!
!*  -----------    open file  ---------------
!*
      call check_gaunt_nl(file_name, id_file, jmax, idx_gl)
!
      end program gaunt
