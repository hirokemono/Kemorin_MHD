!const_bc_infinity_surf.f90
!     module const_bc_infinity_surf
!
!     written by H. Matsui on Dec., 2008
!
!       subroutine allocate_surf_infinity
!       subroutine deallocate_surf_infinity
!
!      subroutine count_num_bc_infinity(num_surf, surf_name,            &
!     &          ngrp_sf_infty)
!      subroutine set_bc_infty_id(num_surf, surf_name,                  &
!     &          ngrp_sf_infty, id_grp_sf_infty)
!
      module const_bc_infinity_surf
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), parameter :: iflag_surf_infty =      0
!
      integer (kind=kint) :: num_bc_infty
      real (kind=kreal), allocatable :: bc_infty_magnitude(:)
      integer (kind=kint), allocatable :: ibc_infty_type(:)
      character (len=kchara), allocatable :: bc_infty_name(:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_infty_surf_ctl
!
      allocate(bc_infty_magnitude(num_bc_infty))
      allocate(ibc_infty_type(num_bc_infty))
      allocate(bc_infty_name(num_bc_infty))
!
      if(num_bc_infty .gt. 0) then
        ibc_infty_type =     0
        bc_infty_magnitude = 0.0d0
      end if
!
      end subroutine allocate_infty_surf_ctl
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_infty_surf_ctl
!
        deallocate(bc_infty_magnitude)
        deallocate(ibc_infty_type)
        deallocate(bc_infty_name)
!
      end subroutine deallocate_infty_surf_ctl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_bc_infinity(num_surf, surf_name,             &
     &          ngrp_sf_infty)
!
      use m_surf_data_list
!
      integer(kind=kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint), intent(inout) :: ngrp_sf_infty
!
      integer (kind = kint) :: igrp, jgrp
!
!
      ngrp_sf_infty = 0
      do igrp = 1, num_surf
!
!  ---  for infinity element
!
        do jgrp = 1, num_bc_infty
          if (surf_name(igrp) .eq. bc_infty_name(jgrp)                  &
     &           .and. ibc_infty_type(jgrp) .eq. iflag_surf_infty) then
              ngrp_sf_infty = ngrp_sf_infty + 1
          end if
        end do
      end do
!
      end subroutine count_num_bc_infinity
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_infty_id(num_surf, surf_name,                   &
     &          ngrp_sf_infty, id_grp_sf_infty)
!
      use m_surf_data_list
!
      integer(kind=kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
      integer (kind=kint), intent(in) :: ngrp_sf_infty
!
      integer (kind=kint), intent(inout)                                &
     &      :: id_grp_sf_infty(ngrp_sf_infty)
!
      integer (kind=kint) :: igrp, jgrp
      integer (kind=kint) :: icou
!
! ---------  boundary condition for temperature
!
      icou = 0
      do igrp = 1, num_surf
!
! ----------- loop for boundary conditions
!
        do jgrp = 1, num_bc_infty
!
! ----------- check surface group
!
          if (surf_name(igrp) .eq. bc_infty_name(jgrp)                  &
     &         .and. ibc_infty_type(jgrp) .eq. iflag_surf_infty) then
            icou = icou + 1
            id_grp_sf_infty(icou) = igrp
          end if
!
        end do
      end do
!
      end subroutine set_bc_infty_id
!
!-----------------------------------------------------------------------
!
      end module const_bc_infinity_surf
