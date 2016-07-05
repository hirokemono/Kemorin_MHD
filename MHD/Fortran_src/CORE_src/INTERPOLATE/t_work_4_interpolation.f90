!
!     module t_work_4_interpolation
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine verifty_work_4_itp_field                             &
!!     &         (numdir, nsize_itp, itp_org, itp_WK)
!!      subroutine alloc_work_4_itp_field(numdir, nsize_itp, itp_WK)
!!      subroutine dealloc_work_4_itp_field(itp_WK)
!!
!!      subroutine verifty_work_4_itp_int(nsize_itp, itp_org, itp_WK)
!!      subroutine alloc_work_4_itp_int(nsize_itp, itp_WK)
!!      subroutine dealloc_work_4_itp_int(itp_WK)
!
      module t_work_4_interpolation
!
      use m_precision
!
      implicit none
!
      type work_4_interoplation
        real(kind = kreal), pointer :: x_inter_org(:)
        integer(kind = kint), pointer :: i_inter_org(:)
      end type work_4_interoplation
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine verifty_work_4_itp_field                               &
     &         (numdir, nsize_itp, itp_org, itp_WK)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: numdir, nsize_itp
      type(interpolate_table_org), intent(in) :: itp_org
      type(work_4_interoplation), intent(inout) ::  itp_WK
!
!
      if(associated(itp_WK%x_inter_org)) then
        if (itp_org%ntot_table_org.ne.0                                 &
     &     .and. size(itp_WK%x_inter_org) .lt. (numdir*nsize_itp)) then
          call dealloc_work_4_itp_field(itp_WK)
          if ((numdir*nsize_itp) .gt. 0) then
            call alloc_work_4_itp_field(numdir, nsize_itp, itp_WK)
          end if
        end if
      else
        call alloc_work_4_itp_field(numdir, nsize_itp, itp_WK)
      end if
!
      end subroutine verifty_work_4_itp_field
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_4_itp_field(numdir, nsize_itp, itp_WK)
!
      integer(kind = kint), intent(in) :: numdir, nsize_itp
      type(work_4_interoplation), intent(inout) ::  itp_WK
!
!
      allocate(itp_WK%x_inter_org(numdir*nsize_itp) )
      itp_WK%x_inter_org = 0.0d0
!
      end subroutine alloc_work_4_itp_field
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_work_4_itp_field(itp_WK)
!
      type(work_4_interoplation), intent(inout) ::  itp_WK
!
      deallocate(itp_WK%x_inter_org)
!
      end subroutine dealloc_work_4_itp_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine verifty_work_4_itp_int(nsize_itp, itp_org, itp_WK)
!
      use t_interpolate_tbl_org
!
      integer(kind = kint), intent(in) :: nsize_itp
      type(interpolate_table_org), intent(in) :: itp_org
      type(work_4_interoplation), intent(inout) ::  itp_WK
!
!
      if(associated(itp_WK%i_inter_org)) then
        if (itp_org%ntot_table_org.ne.0                                 &
     &     .and. size(itp_WK%i_inter_org) .lt. nsize_itp) then
          call dealloc_work_4_itp_int(itp_WK)
          if (nsize_itp .gt. 0) then
            call alloc_work_4_itp_int(nsize_itp, itp_WK)
          end if
        end if
      else
        call alloc_work_4_itp_int(nsize_itp, itp_WK)
      end if
!
      end subroutine verifty_work_4_itp_int
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_4_itp_int(nsize_itp, itp_WK)
!
      integer(kind = kint), intent(in) :: nsize_itp
      type(work_4_interoplation), intent(inout) ::  itp_WK
!
      allocate(itp_WK%i_inter_org(nsize_itp) )
      itp_WK%i_inter_org = 0
!
      end subroutine alloc_work_4_itp_int
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_work_4_itp_int(itp_WK)
!
      type(work_4_interoplation), intent(inout) ::  itp_WK
!
      deallocate(itp_WK%i_inter_org)
!
      end subroutine dealloc_work_4_itp_int
!
! ----------------------------------------------------------------------
!
      end module t_work_4_interpolation
