!
!      module set_surf_vector_id_type
!        (module set_surf_vector_id)
!        (module set_surf_scalar_id)
!
!      Written by H. Matsui on Feb., 2009
!
!      subroutine s_set_surf_vector_id_type(sf_grp, inod_stack_sf_grp,  &
!     &          num_bc_sf, bc_sf_name, ibc_sf_type, sgs_sf, norm_sf)
!      subroutine set_sf_grad_vector_id_type                            &
!     &          (sf_grp,  num_bc_sf, bc_sf_name, ibc_sf_type,          &
!     &           sf_bc_grad, sf_bc_lead)
!
!      subroutine set_sf_grad_scalar_id_type                            &
!     &          (sf_grp, num_bc_sf, bc_sf_name, ibc_sf_type,           &
!     &           ngrp_sf_fix, id_grp_sf_fix, ist_sf_fix,               &
!     &           ngrp_sf_lead, id_grp_sf_lead)
!
      module set_surf_vector_id_type
!
      use m_precision
      use m_boundary_condition_IDs
!
      use t_group_data
      use t_surface_bc_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine s_set_surf_vector_id_type(sf_grp, inod_stack_sf_grp,   &
     &          num_bc_sf, bc_sf_name, ibc_sf_type, sgs_sf, norm_sf)
!
      type(surface_group_data), intent(in) :: sf_grp
!
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_stack_sf_grp(0:sf_grp%num_grp)
!
      integer (kind=kint) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: sgs_sf(3)
      type(scaler_surf_flux_bc_type),  intent(inout) :: norm_sf
!
      integer(kind = kint) :: i, j, nd
      integer(kind = kint) :: l_s1(3), isig_s(3)
      integer(kind = kint) :: l_10
!
!
      l_s1(1:3) = 0
      l_10 = 0
!
      do i=1, sf_grp%num_grp
!
! ----------- loop for boundary conditions
        do j=1, num_bc_sf
!
! ----------- check surface group
         if (sf_grp%grp_name(i) .eq. bc_sf_name(j)) then
           isig_s(1:3) = 0
!
! -----------set boundary from control file
!
           do nd = 1, 3
!
! -----------set boundary from control file
              if ( ibc_sf_type(j)==(iflag_bc_sgs+nd) ) then
                isig_s(nd) = 1
              end if
            end do
!
! -----------set boundary from control file
!
            if ( abs(ibc_sf_type(j)) .eq. iflag_fixed_norm) then
              l_10 = l_10 + 1
              norm_sf%id_grp_sf_fix_fx(l_10) = i
!
              norm_sf%ist_ele_sf_fix_fx(l_10)                           &
     &             = norm_sf%ist_ele_sf_fix_fx(l_10-1)                  &
     &                 + inod_stack_sf_grp(i) - inod_stack_sf_grp(i-1)
              isig_s(1:3) = 1
            end if
!
            do nd = 1, 3
              if ( isig_s(nd).eq.1 ) then
                l_s1(nd) = l_s1(nd) + 1
                sgs_sf(nd)%id_grp_sf_dat(l_s1(nd)) = i
              end if
            end do
!
          end if
!
        end do
      end do
!
      end subroutine s_set_surf_vector_id_type
!
!-----------------------------------------------------------------------
!
      subroutine set_sf_grad_vector_id_type                             &
     &          (sf_grp,  num_bc_sf, bc_sf_name, ibc_sf_type,           &
     &           sf_bc_grad, sf_bc_lead)
!
      type(surface_group_data), intent(in) :: sf_grp
!
      integer (kind=kint) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
! 
      type(scaler_surf_flux_bc_type), intent(inout) :: sf_bc_grad(3)
      type(scaler_surf_bc_data_type),  intent(inout) :: sf_bc_lead(3)
!
      integer(kind = kint) :: i, j, nd, i_dest
      integer(kind = kint) :: l_f1(3), l_l1(3)
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
! -----------set fixed boundary
!
            do nd = 1, 3
              if ( abs( ibc_sf_type(j) ) .eq. (nd+iflag_fixed_grad) )   &
     &           then
                l_f1(nd) = l_f1(nd) + 1
                i_dest = l_f1(nd)
                sf_bc_grad(nd)%id_grp_sf_fix_fx(i_dest) = i
!
                sf_bc_grad(nd)%ist_ele_sf_fix_fx(i_dest)                &
     &              = sf_bc_grad(nd)%ist_ele_sf_fix_fx(i_dest-1)        &
     &               + sf_grp%istack_grp(i) - sf_grp%istack_grp(i-1)
!
! -----------lead boundary values
              else if ( ibc_sf_type(j)==(nd+iflag_lead_grad) ) then
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
      end subroutine set_sf_grad_vector_id_type
!
!-----------------------------------------------------------------------
!
      subroutine set_sf_grad_scalar_id_type                             &
     &          (sf_grp, num_bc_sf, bc_sf_name, ibc_sf_type,            &
     &           ngrp_sf_fix, id_grp_sf_fix, ist_sf_fix,                &
     &           ngrp_sf_lead, id_grp_sf_lead)
!
      type(surface_group_data), intent(in) :: sf_grp
!
      integer (kind=kint), intent(in) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
!
      integer(kind=kint), intent(in) :: ngrp_sf_fix
      integer(kind=kint), intent(inout) :: id_grp_sf_fix(ngrp_sf_fix)
      integer(kind=kint), intent(inout) :: ist_sf_fix(0:ngrp_sf_fix)
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
      do i=1, sf_grp%num_grp
        do j=1, num_bc_sf
          if (sf_grp%grp_name(i) .eq. bc_sf_name(j)) then
!
! -----------set boundary using SGS case
!
            if (abs(ibc_sf_type(j)) .eq. iflag_fixed_grad_s) then
              l_f1 = l_f1 + 1
              id_grp_sf_fix(l_f1) = i
              ist_sf_fix(l_f1) = ist_sf_fix(l_f1-1)                     &
     &                          + sf_grp%istack_grp(i)                  &
     &                          - sf_grp%istack_grp(i-1)
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
      end subroutine set_sf_grad_scalar_id_type
!
!-----------------------------------------------------------------------
!
      end module set_surf_vector_id_type
