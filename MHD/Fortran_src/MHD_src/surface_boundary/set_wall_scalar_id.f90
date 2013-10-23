!
!      module set_wall_scalar_id
!
!
!      Written by H. Matsui on Sep. 2005
!      Modified by H. Matsui on Mar. 2009
!
!      subroutine count_num_wall_surf(num_surf, surf_name,              &
!     &          num_bc_sf, bc_sf_name, ibc_sf_type,                    &
!     &          ngrp_sf_wall, ngrp_sf_spin, ngrp_sf_spout)
!      subroutine s_set_wall_scalar_id(num_surf, surf_name,             &
!     &          num_bc_sf, bc_sf_name, ibc_sf_type,                    &
!     &          ngrp_sf_wall, id_grp_sf_wall,                          &
!     &          ngrp_sf_spin, id_grp_sf_spin,                          &
!     &          ngrp_sf_spout, id_grp_sf_spout)
!
      module set_wall_scalar_id
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
      subroutine count_num_wall_surf(num_surf, surf_name,               &
     &          num_bc_sf, bc_sf_name, ibc_sf_type,                     &
     &          ngrp_sf_wall, ngrp_sf_spin, ngrp_sf_spout)
!
      integer (kind=kint), intent(in) :: num_surf
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint), intent(in) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
! 
! 
      integer(kind=kint), intent(inout) :: ngrp_sf_wall
!
      integer(kind=kint), intent(inout) :: ngrp_sf_spin, ngrp_sf_spout
!
      integer(kind = kint) :: l_w1, l_i1, l_o1
      integer(kind = kint) :: i, j
!
!
      l_w1 = 0
      l_i1 = 0
      l_o1 = 0
!
! ---------  boundary condition for temperature
!
      do i=1, num_surf
!
        do j=1, num_bc_sf
!
         if (surf_name(i)==bc_sf_name(j)) then
!
! -----------set boundary
!
          if (ibc_sf_type(j) .eq. iflag_surf_wall) then
            ngrp_sf_wall = ngrp_sf_wall + 1
!
          else if (ibc_sf_type(j) .eq. iflag_surf_sph_in) then
            ngrp_sf_spin = ngrp_sf_spin + 1
!
          else if (ibc_sf_type(j) .eq. iflag_surf_sph_out) then
            ngrp_sf_spout = ngrp_sf_spout + 1
          end if
!
         end if
        end do
      end do
!
      end subroutine count_num_wall_surf
!
!-----------------------------------------------------------------------
!
      subroutine s_set_wall_scalar_id(num_surf, surf_name,              &
     &          num_bc_sf, bc_sf_name, ibc_sf_type,                     &
     &          ngrp_sf_wall, id_grp_sf_wall,                           &
     &          ngrp_sf_spin, id_grp_sf_spin,                           &
     &          ngrp_sf_spout, id_grp_sf_spout)
!
      integer(kind = kint), intent(in) :: num_surf
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint), intent(in) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
! 
! 
      integer(kind=kint), intent(in) :: ngrp_sf_wall
      integer(kind=kint), intent(inout) :: id_grp_sf_wall(ngrp_sf_wall)
!
      integer(kind=kint), intent(in) :: ngrp_sf_spin
      integer(kind=kint), intent(inout)                                 &
     &      :: id_grp_sf_spin(ngrp_sf_spin)
! 
      integer(kind=kint), intent(in) :: ngrp_sf_spout
      integer(kind=kint), intent(inout)                                 &
     &      :: id_grp_sf_spout(ngrp_sf_spout)
!
      integer(kind = kint) :: l_w1, l_i1, l_o1
      integer(kind = kint) :: i, j
!
!
      l_w1 = 0
      l_i1 = 0
      l_o1 = 0
!
! ---------  boundary condition for temperature
!
      do i=1, num_surf
        do j=1, num_bc_sf
!
          if (surf_name(i)==bc_sf_name(j)) then
!
! -----------set boundary
!
            if (ibc_sf_type(j) .eq. iflag_surf_wall) then
              l_w1 = l_w1 + 1
              id_grp_sf_wall(l_w1) = i
!
            else if (ibc_sf_type(j) .eq. iflag_surf_sph_in) then
              l_i1 = l_i1 + 1
              id_grp_sf_spin(l_i1) = i
!
            else if (ibc_sf_type(j) .eq. iflag_surf_sph_out) then
              l_o1 = l_o1 + 1
              id_grp_sf_spout(l_o1) = i
            end if
!
          end if
        end do
      end do
!
      end subroutine s_set_wall_scalar_id
!
!-----------------------------------------------------------------------
!
      end module set_wall_scalar_id
