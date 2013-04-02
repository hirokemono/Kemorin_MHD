!
!     module m_work_4_interpolation
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine verifty_work_4_itp_field(numdir, nsize_itp)
!      subroutine allocate_work_4_itp_field(numdir, nsize_itp)
!      subroutine deallocate_work_4_itp_field
!
!      subroutine verifty_work_4_itp_int(nsize_itp)
!      subroutine allocate_work_4_itp_int(nsize_itp)
!      subroutine deallocate_work_4_itp_int
!
      module m_work_4_interpolation
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: x_inter_org(:)
!
      integer(kind = kint), allocatable :: i_inter_org(:)
!
      integer(kind = kint) :: isize_itp_work = -1
      integer(kind = kint) :: isize_itp_int_work = -1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine verifty_work_4_itp_field(numdir, nsize_itp)
!
      use m_interpolate_table_orgin
!
      integer(kind = kint), intent(in) :: numdir, nsize_itp
!
!
      if (ntot_table_org.ne.0                                           &
     &     .and. isize_itp_work .ge. 0                                  &
     &     .and. isize_itp_work .lt. (numdir*nsize_itp)) then
        call deallocate_work_4_itp_field
      end if
!
      if   (isize_itp_work .le. 0 .and. (numdir*nsize_itp) .gt. 0) then
        call allocate_work_4_itp_field(numdir, nsize_itp)
      end if
!
      end subroutine verifty_work_4_itp_field
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_4_itp_field(numdir, nsize_itp)
!
      integer(kind = kint), intent(in) :: numdir, nsize_itp
!
      allocate(x_inter_org(numdir*nsize_itp) )
      x_inter_org = 0.0d0
!
      isize_itp_work = numdir*nsize_itp
!
      end subroutine allocate_work_4_itp_field
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_4_itp_field
!
      deallocate(x_inter_org)
      isize_itp_work = 0
!
      end subroutine deallocate_work_4_itp_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine verifty_work_4_itp_int(nsize_itp)
!
      use m_interpolate_table_orgin
!
      integer(kind = kint), intent(in) :: nsize_itp
!
!
      if (ntot_table_org.ne.0                                           &
     &     .and. isize_itp_work .ge. 0                                  &
     &     .and. isize_itp_work .lt. nsize_itp) then
        call deallocate_work_4_itp_int
      end if
!
      if   (isize_itp_work .le. 0 .and. nsize_itp .gt. 0) then
        call allocate_work_4_itp_int(nsize_itp)
      end if
!
      end subroutine verifty_work_4_itp_int
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_4_itp_int(nsize_itp)
!
      integer(kind = kint), intent(in) :: nsize_itp
!
      allocate(i_inter_org(nsize_itp) )
      i_inter_org = 0.0d0
!
      isize_itp_int_work = nsize_itp
!
      end subroutine allocate_work_4_itp_int
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_4_itp_int
!
      deallocate(i_inter_org)
      isize_itp_int_work = 0
!
      end subroutine deallocate_work_4_itp_int
!
! ----------------------------------------------------------------------
!
      end module m_work_4_interpolation
