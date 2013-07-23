!
!      module set_sf_grad_scalar_id
!
!      Written by H. Matsui on Sep. 2005
!      Modified by H. Matsui on Feb., 2009
!
!      subroutine count_num_surf_grad_scalar                            &
!     &          (num_surf, surf_istack, surf_name,                     &
!     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,        &
!     &           ngrp_sf_fix, nele_sf_fix, ngrp_sf_lead)
!      subroutine s_set_surf_grad_scalar_id                             &
!     &          (num_surf, surf_istack, surf_name,                     &
!     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,        &
!     &           field_name, ngrp_sf_fix, id_grp_sf_fix,               &
!     &           nele_sf_fix, ist_sf_fix, sf_apt_fix,                  &
!     &           ngrp_sf_lead, id_grp_sf_lead)
!
      module set_sf_grad_scalar_id
!
      use set_surface_bc
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
      subroutine count_num_surf_grad_scalar                             &
     &          (num_surf, surf_istack, surf_name,                      &
     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,         &
     &           field_name, ngrp_sf_fix, nele_sf_fix, ngrp_sf_lead)
!
      integer(kind=kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint), intent(in) :: num_bc_sf
      real (kind=kreal), intent(in) :: bc_sf_mag(num_bc_sf)
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
      character(len=kchara), intent(in) :: field_name
! 
! 
      integer (kind=kint), intent(inout) :: ngrp_sf_fix, nele_sf_fix
      integer (kind=kint), intent(inout) :: ngrp_sf_lead
!
      integer(kind = kint) :: i, j
!
      ngrp_sf_fix =  0
      nele_sf_fix =  0
      ngrp_sf_lead = 0
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
! -----------set boundary using SGS case
!
            if (ibc_sf_type(j) .eq. iflag_fixed_grad_s) then
              ngrp_sf_fix = ngrp_sf_fix + 1
              nele_sf_fix = nele_sf_fix                                 &
     &                   + surf_istack(i) - surf_istack(i-1)
            else if (ibc_sf_type(j) .eq. (-iflag_fixed_grad_s)) then
              call count_surf_group_from_data(i, ngrp_sf_fix,           &
     &            nele_sf_fix, field_name, num_surf, surf_istack,       &
     &            surf_name)
            else if (ibc_sf_type(j) .eq. iflag_lead_grad_s) then
              ngrp_sf_lead = ngrp_sf_lead + 1
            end if
!
          end if
        end do
      end do
!
      end subroutine count_num_surf_grad_scalar
!
!-----------------------------------------------------------------------
!
      subroutine s_set_surf_grad_scalar_id                              &
     &          (num_surf, surf_istack, surf_name,                      &
     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,         &
     &           field_name, ngrp_sf_fix, id_grp_sf_fix,                &
     &           nele_sf_fix, ist_sf_fix, sf_apt_fix,                   &
     &           ngrp_sf_lead, id_grp_sf_lead)
!
      integer (kind=kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint), intent(in) :: num_bc_sf
      real (kind=kreal), intent(in) :: bc_sf_mag(num_bc_sf)
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
!
      character (len=kchara), intent(in) :: field_name
!
      integer(kind=kint), intent(in) :: ngrp_sf_fix, nele_sf_fix
      integer(kind=kint), intent(inout) :: id_grp_sf_fix(ngrp_sf_fix)
      integer(kind=kint), intent(inout) :: ist_sf_fix(0:ngrp_sf_fix)
      real (kind=kreal), intent(inout) :: sf_apt_fix(nele_sf_fix)
! 
      integer(kind=kint), intent(in) :: ngrp_sf_lead
      integer(kind=kint), intent(inout) :: id_grp_sf_lead(ngrp_sf_lead)
!
      integer(kind = kint) :: l_f1, l_l1
      integer(kind = kint) :: i, j
!
!
      l_f1 = 0
      l_l1 = 0
!
! ---------  boundary condition for temperature
!
      do i=1, num_surf
!
        do j=1, num_bc_sf
!
          if (surf_name(i)==bc_sf_name(j)) then
!
! -----------set boundary using SGS case
!
            if (ibc_sf_type(j) .eq. iflag_fixed_grad_s) then
              call set_surf_group_from_ctl(num_surf, surf_istack,       &
     &            ngrp_sf_fix, nele_sf_fix, l_f1, i, id_grp_sf_fix,     &
     &            ist_sf_fix, sf_apt_fix, bc_sf_mag(j))
!
            else if (ibc_sf_type(j) .eq. (-iflag_fixed_grad_s)) then
              call  set_surf_group_from_data(num_surf, surf_name,       &
     &            ngrp_sf_fix, nele_sf_fix, l_f1, i, id_grp_sf_fix,     &
     &            ist_sf_fix, sf_apt_fix, field_name)
!
            else if (ibc_sf_type(j) .eq. iflag_lead_grad_s) then
              l_l1 = l_l1 + 1
              id_grp_sf_lead(l_l1) = i
            end if
!
          end if
        end do
      end do
!
      end subroutine s_set_surf_grad_scalar_id
!
!-----------------------------------------------------------------------
!
      end module set_sf_grad_scalar_id
