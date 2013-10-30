!
!      module set_surf_scalar_id
!
!      Written by H. Matsui on Sep. 2005
!
!
!      subroutine s_count_num_surf_scalar(num_surf, surf_name,          &
!     &          num_bc_sf, bc_sf_name, ibc_sf_type, ngrp_sf_sgs)
!      subroutine s_set_surf_scalar_id(num_surf, surf_name,             &
!     &          num_bc_sf, bc_sf_name, ibc_sf_type,                    &
!     &          ngrp_sf_sgs, id_grp_sf_sgs)
!
      module set_surf_scalar_id
!
      use m_precision
      use m_boundary_condition_IDs
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_count_num_surf_scalar(num_surf, surf_name,           &
     &          num_bc_sf, bc_sf_name, ibc_sf_type, ngrp_sf_sgs)
!
      integer (kind=kint), intent(in) :: num_surf
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint), intent(in) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
!! 
      integer (kind=kint), intent(inout) :: ngrp_sf_sgs
!
      integer(kind = kint) :: isig_s
      integer(kind = kint) :: i, j
!
!
      ngrp_sf_sgs = 0
!
! ---------  boundary condition for temperature
!
      do i=1, num_surf
!
! ----------- loop for boundary conditions
!
        do j=1, num_bc_sf
!
! ----------- check surface group
!
          if (surf_name(i)==bc_sf_name(j)) then
!
            isig_s = 0
!
! -----------set boundary using SGS case
!
            if (ibc_sf_type(j) .eq. iflag_bc_sgs_s) then
              isig_s = 1
            end if
!
            if (isig_s .eq. 1) then
              ngrp_sf_sgs = ngrp_sf_sgs + 1
            end if
!
          end if
        end do
      end do
!
      end subroutine s_count_num_surf_scalar
!
!-----------------------------------------------------------------------
!
      subroutine s_set_surf_scalar_id(num_surf, surf_name,              &
     &          num_bc_sf, bc_sf_name, ibc_sf_type,                     &
     &          ngrp_sf_sgs, id_grp_sf_sgs)
!
      integer (kind=kint), intent(in) :: num_surf
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint), intent(in) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
! 
      integer(kind=kint), intent(in) :: ngrp_sf_sgs
      integer(kind=kint), intent(inout) :: id_grp_sf_sgs(ngrp_sf_sgs)
!
      integer(kind = kint) :: l_s1, isig_s
      integer(kind = kint) :: i, j
!
!
      l_s1 = 0
!
! ---------  boundary condition for temperature
      do i=1, num_surf
!
! ----------- loop for boundary conditions
         do j=1, num_bc_sf
!
! ----------- check surface group
          if (surf_name(i)==bc_sf_name(j)) then
            isig_s = 0
!
! -----------set boundary using SGS case
            if (ibc_sf_type(j) .eq. iflag_bc_sgs_s) then
              isig_s = 1
            end if
!
            if (isig_s .eq. 1) then
              l_s1 = l_s1 + 1
              id_grp_sf_sgs(l_s1) = i
            end if
!
          end if
        end do
      end do
!
      end subroutine s_set_surf_scalar_id
!
!-----------------------------------------------------------------------
!
      end module set_surf_scalar_id
