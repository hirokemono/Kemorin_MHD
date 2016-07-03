!
!      module set_surf_vector_id
!
!      Written by H. Matsui on Sep. 2005
!
!!      subroutine s_count_num_surf_vector(IO_bc, sf_grp, sf_grp_nod,   &
!!     &           num_bc_sf, bc_sf_name, ibc_sf_type,                  &
!!     &           field_name, sgs_sf, norm_sf)
!!      subroutine s_set_surf_vector_id                                 &
!!     &          (IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,&
!!     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,       &
!!     &           field_name, sgs_sf, norm_sf)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(surface_group_geometry), intent(in) :: sf_grp_v
!!        type(scaler_surf_bc_data_type),  intent(inout) :: sgs_sf(3)
!!        type(scaler_surf_flux_bc_type),  intent(inout) :: norm_sf
!
      module set_surf_vector_id
!
      use m_precision
      use m_boundary_condition_IDs
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_bc_data
      use t_boundary_field_IO
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
      subroutine s_count_num_surf_vector(IO_bc, sf_grp, sf_grp_nod,     &
     &           num_bc_sf, bc_sf_name, ibc_sf_type,                    &
     &           field_name, sgs_sf, norm_sf)
!
      type(IO_boundary), intent(in) :: IO_bc
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
!
      integer (kind=kint) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
      character (len=kchara), intent(in) :: field_name
! 
      type(scaler_surf_bc_data_type),  intent(inout) :: sgs_sf(3)
      type(scaler_surf_flux_bc_type),  intent(inout) :: norm_sf
!
      integer(kind = kint) :: ngrp_sf_sgs(3)
      integer(kind = kint) :: ngrp_sf_dat_n, nnod_sf_dat_n
      integer(kind = kint) :: isig_s(3)
      integer(kind = kint) :: i, j, nd
!
!
      ngrp_sf_sgs(1:3) = 0
      ngrp_sf_dat_n = 0
      nnod_sf_dat_n = 0
!
      do i = 1, sf_grp%num_grp
!
        if (num_bc_sf .gt. 0) then
!
! ----------- loop for boundary conditions
          do j=1, num_bc_sf
!
! ----------- check surface group
            if (sf_grp%grp_name(i) .eq. bc_sf_name(j)) then
              isig_s(1:3) = 0
!
! -----------set boundary from control file
              do nd = 1, 3
!
! -----------set boundary from control file
                if ( ibc_sf_type(j) .eq. (iflag_bc_sgs+nd) ) then
                  isig_s(nd) = 1
                end if
              end do
!
! -----------set boundary from control file
              if (ibc_sf_type(j) .eq. iflag_fixed_norm) then
                ngrp_sf_dat_n = ngrp_sf_dat_n + 1
                nnod_sf_dat_n = nnod_sf_dat_n                           &
     &                         + sf_grp_nod%inod_stack_sf_grp(i)        &
     &                         - sf_grp_nod%inod_stack_sf_grp(i-1)
                isig_s(1:3) = 1
              else if (ibc_sf_type(j) .eq. -iflag_fixed_norm) then
                call count_surf_nod_group_from_data                     &
     &             (IO_bc, i, ngrp_sf_dat_n,                            &
     &              nnod_sf_dat_n, field_name, sf_grp%num_grp,          &
     &              sf_grp_nod%inod_stack_sf_grp, sf_grp%grp_name)
                isig_s(1:3) = 1
              end if
!
              ngrp_sf_sgs(1:3) = ngrp_sf_sgs(1:3) + isig_s(1:3)
            end if
!
          end do
        end if
      end do
!
      norm_sf%ngrp_sf_fix_fx =  ngrp_sf_dat_n
      norm_sf%nitem_sf_fix_fx = nnod_sf_dat_n
      do j = 1, 3
        sgs_sf(j)%ngrp_sf_dat = ngrp_sf_sgs(j)
      end do
!
      end subroutine s_count_num_surf_vector
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_surf_vector_id                                   &
     &          (IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,  &
     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,         &
     &           field_name, sgs_sf, norm_sf)
!
      use t_surface_group_geometry
!
      type(IO_boundary), intent(in) :: IO_bc
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
      integer (kind=kint) :: num_bc_sf
      real (kind=kreal), intent(in) :: bc_sf_mag(num_bc_sf)
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
      character (len=kchara), intent(in) :: field_name
!
      type(scaler_surf_bc_data_type),  intent(inout) :: sgs_sf(3)
      type(scaler_surf_flux_bc_type),  intent(inout) :: norm_sf
!
      integer(kind = kint) :: i, j, nd
      integer(kind = kint) :: l_f1(3), l_s1(3), isig_s(3)
      integer(kind = kint) :: l_10
!
!
      l_f1(1:3) = 0
      l_s1(1:3) = 0
      l_10 = 0
!
      do i = 1, sf_grp%num_grp
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
              if ( ibc_sf_type(j) .eq. (iflag_bc_sgs+nd) ) then
                isig_s(nd) = 1
              end if
            end do
!
! -----------set boundary from control file
!
            if (ibc_sf_type(j) .eq. iflag_fixed_norm) then
              call set_sf_nod_grp_from_ctl(l_10, i,                     &
     &           sf_grp%num_grp, sf_grp_nod%inod_stack_sf_grp,          &
     &            norm_sf%ngrp_sf_fix_fx, norm_sf%nitem_sf_fix_fx,      &
     &            norm_sf%id_grp_sf_fix_fx, norm_sf%ist_ele_sf_fix_fx,  &
     &            norm_sf%sf_apt_fix_fx, bc_sf_mag(j))
              isig_s(1:3) = 1
            else if (ibc_sf_type(j) .eq. -iflag_fixed_norm) then
              call set_sf_nod_grp_from_data(IO_bc, l_10, i,             &
     &            node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,        &
     &            norm_sf%ngrp_sf_fix_fx, norm_sf%nitem_sf_fix_fx,      &
     &            norm_sf%id_grp_sf_fix_fx, norm_sf%ist_ele_sf_fix_fx,  &
     &            norm_sf%sf_apt_fix_fx, field_name )
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
      end subroutine s_set_surf_vector_id
!
!-----------------------------------------------------------------------
!
      end module set_surf_vector_id
