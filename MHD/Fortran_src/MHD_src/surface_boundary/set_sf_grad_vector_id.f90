!
!      module set_sf_grad_vector_id
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine count_num_sf_grad_vector                              &
!     &          (sf_grp, num_bc_sf, bc_sf_name, ibc_sf_type,           &
!     &           field_name_x, field_name_y, field_name_z,             &
!     &           sf_bc_grad, sf_bc_lead)
!      subroutine s_set_sf_grad_vector_id(sf_grp,                       &
!     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,        &
!     &           field_name_x, field_name_y, field_name_z,             &
!     &           sf_bc_grad, sf_bc_lead)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(scaler_surf_flux_bc_type), intent(inout) :: sf_bc_grad(3)
!
      module set_sf_grad_vector_id
!
      use m_precision
      use m_boundary_condition_IDs
!
      use t_group_data
      use t_surface_bc_data
      use set_surface_bc
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine count_num_sf_grad_vector                               &
     &          (sf_grp, num_bc_sf, bc_sf_name, ibc_sf_type,            &
     &           field_name_x, field_name_y, field_name_z,              &
     &           sf_bc_grad, sf_bc_lead)
!
      type(surface_group_data), intent(in) :: sf_grp
!
      integer (kind=kint) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
      character(len=kchara), intent(in) :: field_name_x
      character(len=kchara), intent(in) :: field_name_y
      character(len=kchara), intent(in) :: field_name_z
!
      type(scaler_surf_flux_bc_type), intent(inout) :: sf_bc_grad(3)
      type(scaler_surf_bc_data_type),  intent(inout) :: sf_bc_lead(3)
!
      integer(kind = kint) :: i, j, nd
      integer(kind=kint) :: ngrp_sf_fix(3), nele_sf_fix(3)
      integer(kind=kint) :: ngrp_sf_lead(3)
      character(len=kchara) :: field_name
!
!
      ngrp_sf_fix(1:3) =  0
      nele_sf_fix(1:3) =  0
      ngrp_sf_lead(1:3) = 0
!
      do i=1, sf_grp%num_grp
        if (num_bc_sf .gt. 0) then
!
! ----------- loop for boundary conditions
          do j=1, num_bc_sf
!
! ----------- check surface group
            if (sf_grp%grp_name(i) .eq. bc_sf_name(j)) then
!
! -----------set boundary from control file
              do nd = 1, 3
                if ( ibc_sf_type(j) .eq. (iflag_fixed_grad + nd) ) then
                  ngrp_sf_fix(nd) = ngrp_sf_fix(nd) + 1
                  nele_sf_fix(nd) = nele_sf_fix(nd)                     &
     &                             + sf_grp%istack_grp(i)               &
     &                             - sf_grp%istack_grp(i-1)
!
! -----------set boundary from data file
                else if (ibc_sf_type(j) .eq. -(iflag_fixed_grad + nd))  &
     &              then
                  if      (nd .eq. 1) then
                    field_name = field_name_x
                  else if (nd .eq. 2) then
                    field_name = field_name_y
                  else if (nd .eq. 3) then
                    field_name = field_name_z
                  end if
!
                  call count_surf_group_from_data(i, ngrp_sf_fix(nd),   &
     &                nele_sf_fix(nd), field_name, sf_grp%num_grp,      &
     &                sf_grp%istack_grp, sf_grp%grp_name )
!
! -----------set boundary from data file
!
                else if ( ibc_sf_type(j)==(nd+iflag_lead_grad) ) then
                  ngrp_sf_lead(nd) = ngrp_sf_lead(nd) + 1
                end if
!
              end do
            end if
!
          end do
        end if
      end do
!
      do j = 1, 3
        sf_bc_grad(j)%ngrp_sf_fix_fx = ngrp_sf_fix(j)
        sf_bc_grad(j)%nitem_sf_fix_fx = nele_sf_fix(3)
        sf_bc_lead(j)%ngrp_sf_dat = ngrp_sf_lead(j)
      end do
!
      end subroutine count_num_sf_grad_vector
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_sf_grad_vector_id(sf_grp,                        &
     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,         &
     &           field_name_x, field_name_y, field_name_z,              &
     &           sf_bc_grad, sf_bc_lead)
!
      type(surface_group_data), intent(in) :: sf_grp
!
      integer (kind=kint) :: num_bc_sf
      real (kind=kreal), intent(in) :: bc_sf_mag(num_bc_sf)
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
      character(len=kchara), intent(in) :: field_name_x
      character(len=kchara), intent(in) :: field_name_y
      character(len=kchara), intent(in) :: field_name_z
! 
      type(scaler_surf_flux_bc_type), intent(inout) :: sf_bc_grad(3)
      type(scaler_surf_bc_data_type),  intent(inout) :: sf_bc_lead(3)
!
!
      integer(kind = kint) :: i, j, nd
      integer(kind = kint) :: l_f1(3), l_l1(3)
      character (len=kchara) :: field_name
!
!
      l_f1(1:3) = 0
      l_l1(1:3) = 0
!
      do i=1, sf_grp%num_grp
!
! ----------- loop for boundary conditions
        do j=1, num_bc_sf
!
! ----------- check surface group
          if (sf_grp%grp_name(i) .eq. bc_sf_name(j)) then
!
! -----------set boundary from control file
!
            do nd = 1, 3
              if ( ibc_sf_type(j) .eq. (iflag_fixed_grad+nd) ) then
                call set_surf_group_from_ctl                            &
     &             (sf_grp%num_grp, sf_grp%istack_grp,                  &
     &              sf_bc_grad(nd)%ngrp_sf_fix_fx,                      &
     &              sf_bc_grad(nd)%nitem_sf_fix_fx, l_f1(nd), i,        &
     &              sf_bc_grad(nd)%id_grp_sf_fix_fx,                    &
     &              sf_bc_grad(nd)%ist_ele_sf_fix_fx,                   &
     &              sf_bc_grad(nd)%sf_apt_fix_fx, bc_sf_mag(j))
!
! -----------set boundary from data file
              else if ( ibc_sf_type(j) .eq. -(iflag_fixed_grad+nd) ) then
                if      (nd .eq. 1) then
                  field_name = field_name_x
                else if (nd .eq. 2) then
                  field_name = field_name_y
                else if (nd .eq. 3) then
                  field_name = field_name_z
                end if
!
                call set_surf_group_from_data(sf_grp,                   &
     &              sf_bc_grad(nd)%ngrp_sf_fix_fx,                      &
     &              sf_bc_grad(nd)%nitem_sf_fix_fx, l_f1(nd), i,        &
     &              sf_bc_grad(nd)%id_grp_sf_fix_fx,                    &
     &              sf_bc_grad(nd)%ist_ele_sf_fix_fx,                   &
     &              sf_bc_grad(nd)%sf_apt_fix_fx, field_name)
!
! -----------lead boundary values
              else if ( ibc_sf_type(j) .eq. (nd+iflag_lead_grad) ) then
                l_l1(nd) = l_l1(nd) + 1
                sf_bc_lead(nd)%id_grp_sf_dat(l_l1(nd)) = i
              end if
            end do
!
          end if
!
        end do
      end do
!
      end subroutine s_set_sf_grad_vector_id
!
!-----------------------------------------------------------------------
!
      end module set_sf_grad_vector_id
