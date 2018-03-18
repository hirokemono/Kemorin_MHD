!
!      module set_merged_groups
!
!!      subroutine allocate_flags_merged_grp(merged)
!!      subroutine deallocate_flags_merged_grp
!!
!!      subroutine count_merged_node_group(num_pe, merge_tbl,           &
!!     &          subdomain, sub_nod_grp, merged_grp)
!!      subroutine set_merged_node_group(num_pe, merge_tbl,             &
!!     &          subdomain, sub_nod_grp, merged_grp)
!
!!      subroutine count_merged_element_group(num_pe, merge_tbl,        &
!!     &          subdomain, sub_ele_grp, merged_grp)
!!      subroutine set_merged_element_group(num_pe, merge_tbl,          &
!!     &          subdomain, sub_ele_grp, merged_grp)
!
!!      subroutine count_merged_surface_group(num_pe, merge_tbl,        &
!!     &          subdomain, sub_surf_grp, merged_grp)
!!      subroutine set_merged_surface_group(num_pe, merge_tbl,          &
!!     &          subdomain, sub_surf_grp, merged_grp)
!!        type(merged_stacks), intent(in) :: merge_tbl
!!        type(mesh_geometry), intent(in) :: subdomain(num_pe)
!!        type(group_data), intent(in) :: sub_nod_grp(num_pe)
!!        type(group_data), intent(in) :: sub_ele_grp(num_pe)
!!        type(surface_group_data), intent(in) :: sub_surf_grp(num_pe)
!!        type(mesh_groups), intent(inout) :: merged_grp
!
!      Written by H. Matsui on july, 2005
!
      module set_merged_groups
!
      use m_precision
!
      use m_geometry_constants
      use t_mesh_data
      use t_group_data
!
      implicit none
!
      integer (kind = kint), allocatable, private :: iflag_nod_grp(:)
      integer (kind = kint), allocatable, private :: iflag_ele_grp(:)
      integer (kind = kint), allocatable, private :: iflag_sf_grp(:,:)
!
      private :: mark_node_by_global_address
      private :: mark_ele_by_global_address
      private :: mark_surf_by_global_address
!
!------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_flags_merged_grp(merged)
!
      type(mesh_geometry), intent(in) :: merged
!
      allocate( iflag_nod_grp(merged%node%numnod) )
      allocate( iflag_ele_grp(merged%ele%numele) )
      allocate( iflag_sf_grp(merged%ele%numele,nsurf_4_ele) )
!
      end subroutine allocate_flags_merged_grp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_flags_merged_grp
!
      deallocate(iflag_nod_grp)
      deallocate(iflag_ele_grp)
      deallocate(iflag_sf_grp )
!
      end subroutine deallocate_flags_merged_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_merged_node_group(num_pe, merge_tbl,             &
     &          subdomain, sub_nod_grp, merged_grp)
!
      use t_merged_geometry_data
!
      integer(kind = kint), intent(in)  :: num_pe
      type(merged_stacks), intent(in) :: merge_tbl
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(group_data), intent(in) :: sub_nod_grp(num_pe)
!
      type(mesh_groups), intent(inout) :: merged_grp
!
      integer(kind = kint) :: inod, jgrp, icou
!
!
      do jgrp = 1, merged_grp%nod_grp%num_grp
        call mark_node_by_global_address                                &
     &     (merged_grp%nod_grp%grp_name(jgrp),                          &
     &      num_pe, subdomain, sub_nod_grp)
!
        icou = merged_grp%nod_grp%istack_grp(jgrp-1)
        do inod = 1, merge_tbl%nnod_merged
          icou = icou + iflag_nod_grp(inod)
        end do
        merged_grp%nod_grp%istack_grp(jgrp) = icou
!
      end do
      merged_grp%nod_grp%num_item                                       &
     &     = merged_grp%nod_grp%istack_grp(merged_grp%nod_grp%num_grp)
!
      end subroutine count_merged_node_group
!
!-----------------------------------------------------------------------
!
      subroutine set_merged_node_group(num_pe, merge_tbl,              &
     &          subdomain, sub_nod_grp, merged_grp)
!
      use t_merged_geometry_data
!
      integer(kind = kint), intent(in)  :: num_pe
      type(merged_stacks), intent(in) :: merge_tbl
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(group_data), intent(in) :: sub_nod_grp(num_pe)
!
      type(mesh_groups), intent(inout) :: merged_grp
!
      integer(kind = kint) :: inod, jgrp, icou
!
!
      do jgrp = 1, merged_grp%nod_grp%num_grp
        call mark_node_by_global_address                                &
     &     (merged_grp%nod_grp%grp_name(jgrp),                          &
     &      num_pe, subdomain, sub_nod_grp)
!
        icou = merged_grp%nod_grp%istack_grp(jgrp-1)
        do inod = 1, merge_tbl%nnod_merged
          if ( iflag_nod_grp(inod) .gt. 0 ) then
            icou = icou + 1
            merged_grp%nod_grp%item_grp(icou) = inod
          end if
        end do
      end do
!
      end subroutine set_merged_node_group
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_merged_element_group(num_pe, merge_tbl,          &
     &          subdomain, sub_ele_grp, merged_grp)
!
      use t_merged_geometry_data
!
      integer(kind = kint), intent(in) :: num_pe
      type(merged_stacks), intent(in) :: merge_tbl
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(group_data), intent(in) :: sub_ele_grp(num_pe)
!
      type(mesh_groups), intent(inout) :: merged_grp
!
      integer(kind = kint) :: iele, jgrp, icou
!
!
      do jgrp = 1, merged_grp%ele_grp%num_grp
        call mark_ele_by_global_address                                 &
     &     (merged_grp%ele_grp%grp_name(jgrp),                          &
     &      num_pe, subdomain, sub_ele_grp)
!
        icou = merged_grp%ele_grp%istack_grp(jgrp-1)
        do iele = 1, merge_tbl%nele_merged
          icou = icou + iflag_ele_grp(iele)
        end do
        merged_grp%ele_grp%istack_grp(jgrp) = icou
!
      end do
      merged_grp%ele_grp%num_item                                       &
     &     = merged_grp%ele_grp%istack_grp(merged_grp%ele_grp%num_grp)
!
      end subroutine count_merged_element_group
!
!-----------------------------------------------------------------------
!
      subroutine set_merged_element_group(num_pe, merge_tbl,            &
     &          subdomain, sub_ele_grp, merged_grp)
!
      use t_merged_geometry_data
!
      integer(kind = kint), intent(in) :: num_pe
      type(merged_stacks), intent(in) :: merge_tbl
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(group_data), intent(in) :: sub_ele_grp(num_pe)
!
      type(mesh_groups), intent(inout) :: merged_grp
!
      integer(kind = kint) :: iele, jgrp, icou
!
!
      do jgrp = 1, merged_grp%ele_grp%num_grp
        call mark_ele_by_global_address                                 &
     &     (merged_grp%ele_grp%grp_name(jgrp),                          &
     &      num_pe, subdomain, sub_ele_grp)
!
        icou = merged_grp%ele_grp%istack_grp(jgrp-1)
        do iele = 1, merge_tbl%nele_merged
          if ( iflag_ele_grp(iele) .gt. 0 ) then
            icou = icou + 1
            merged_grp%ele_grp%item_grp(icou) = iele
          end if
        end do
      end do
!
      end subroutine set_merged_element_group
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_merged_surface_group(num_pe, merge_tbl,          &
     &          subdomain, sub_surf_grp, merged_grp)
!
      use t_merged_geometry_data
!
      integer(kind = kint), intent(in)  :: num_pe
      type(merged_stacks), intent(in) :: merge_tbl
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(surface_group_data), intent(in) :: sub_surf_grp(num_pe)
!
      type(mesh_groups), intent(inout) :: merged_grp
!
      integer (kind = kint) :: icou, jgrp, iele, isurf
!
!
      do jgrp = 1, merged_grp%surf_grp%num_grp
        call mark_surf_by_global_address                                &
     &     (merged_grp%surf_grp%grp_name(jgrp),                         &
     &      num_pe, subdomain, sub_surf_grp)
!
        icou = merged_grp%surf_grp%istack_grp(jgrp-1)
        do iele = 1, merge_tbl%nele_merged
          do isurf = 1, nsurf_4_ele
            icou = icou + iflag_sf_grp(iele,isurf)
          end do
        end do
        merged_grp%surf_grp%istack_grp(jgrp) = icou
!
      end do
      merged_grp%surf_grp%num_item                                      &
     &    = merged_grp%surf_grp%istack_grp(merged_grp%surf_grp%num_grp)
!
!
      end subroutine count_merged_surface_group
!
!-----------------------------------------------------------------------
!
      subroutine set_merged_surface_group(num_pe, merge_tbl,            &
     &          subdomain, sub_surf_grp, merged_grp)
!
      use t_merged_geometry_data
!
      integer(kind = kint), intent(in)  :: num_pe
      type(merged_stacks), intent(in) :: merge_tbl
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(surface_group_data), intent(in) :: sub_surf_grp(num_pe)
!
      type(mesh_groups), intent(inout) :: merged_grp
!
      integer (kind = kint) :: icou, jgrp, iele, isurf
!
!
!
      do jgrp = 1, merged_grp%surf_grp%num_grp
        call mark_surf_by_global_address                                &
     &     (merged_grp%surf_grp%grp_name(jgrp),                         &
     &      num_pe, subdomain, sub_surf_grp)
!
        icou = merged_grp%surf_grp%istack_grp(jgrp-1)
        do iele = 1, merge_tbl%nele_merged
          do isurf = 1, nsurf_4_ele
            if ( iflag_sf_grp(iele,isurf).gt.0 ) then
              icou = icou + 1
              merged_grp%surf_grp%item_sf_grp(1,icou) = iele
              merged_grp%surf_grp%item_sf_grp(2,icou) = isurf
            end if
          end do
        end do
        merged_grp%surf_grp%istack_grp(jgrp) = icou
!
      end do
      merged_grp%surf_grp%num_item                                      &
     &    = merged_grp%surf_grp%istack_grp(merged_grp%surf_grp%num_grp)
!
!
      end subroutine set_merged_surface_group
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mark_node_by_global_address(target_name,               &
     &          num_pe, subdomain, sub_nod_grp)
!
      character(len=kchara), intent(in) :: target_name
!
      integer(kind = kint), intent(in)  :: num_pe
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(group_data), intent(in) :: sub_nod_grp(num_pe)
!
      integer(kind = kint) :: ip, igrp, kst, ked, k, ii
      integer(kind = kint_gl) :: inod_g
!
!
      iflag_nod_grp = 0
!
      do ip = 1, num_pe
        do igrp = 1, sub_nod_grp(ip)%num_grp
          if ( sub_nod_grp(ip)%grp_name(igrp) .eq. target_name)  then
            kst = sub_nod_grp(ip)%istack_grp(igrp-1)+1
            ked = sub_nod_grp(ip)%istack_grp(igrp  )
            do k = kst, ked
              ii = sub_nod_grp(ip)%item_grp(k)
              inod_g = subdomain(ip)%node%inod_global(ii)
              iflag_nod_grp(inod_g) = 1
            end do
          end if
        end do
      end do
!
      end subroutine mark_node_by_global_address
!
!-----------------------------------------------------------------------
!
      subroutine mark_ele_by_global_address(target_name,                &
     &          num_pe, subdomain, sub_ele_grp)
!
      character(len=kchara), intent(in) :: target_name
!
      integer(kind = kint), intent(in) :: num_pe
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(group_data), intent(in) :: sub_ele_grp(num_pe)
!
      integer(kind = kint) :: ip, igrp, kst, ked, k, ii
      integer(kind = kint_gl) :: iele_g
!
!
      iflag_ele_grp = 0
!
      do ip = 1, num_pe
        do igrp = 1, sub_ele_grp(ip)%num_grp
          if ( sub_ele_grp(ip)%grp_name(igrp) .eq. target_name)  then
            kst = sub_ele_grp(ip)%istack_grp(igrp-1)+1
            ked = sub_ele_grp(ip)%istack_grp(igrp  )
            do k = kst, ked
              ii = sub_ele_grp(ip)%item_grp(k)
              iele_g = subdomain(ip)%ele%iele_global(ii)
              iflag_ele_grp(iele_g) = 1
            end do
          end if
        end do
      end do
!
      end subroutine mark_ele_by_global_address
!
!-----------------------------------------------------------------------
!
      subroutine mark_surf_by_global_address(target_name,               &
     &          num_pe, subdomain, sub_surf_grp)
!
      character(len=kchara), intent(in) :: target_name
!
      integer(kind = kint), intent(in)  :: num_pe
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(surface_group_data), intent(in) :: sub_surf_grp(num_pe)
!
!
      integer(kind = kint) :: ip, igrp, kst, ked, k, ii, isurf
      integer(kind = kint_gl) :: iele_g
!
!
      iflag_sf_grp = 0
!
      do ip = 1, num_pe
        do igrp = 1, sub_surf_grp(ip)%num_grp
          if (sub_surf_grp(ip)%grp_name(igrp) .eq. target_name) then
            kst = sub_surf_grp(ip)%istack_grp(igrp-1) + 1
            ked = sub_surf_grp(ip)%istack_grp(igrp  )
            do k = kst, ked
              ii =  sub_surf_grp(ip)%item_sf_grp(1,k)
              iele_g =  subdomain(ip)%ele%iele_global(ii)
              isurf = sub_surf_grp(ip)%item_sf_grp(2,k)
              iflag_sf_grp(iele_g,isurf)                                &
     &              = iflag_sf_grp(iele_g,isurf) + 1
            end do
          end if
        end do
      end do
!
      end subroutine mark_surf_by_global_address
!
!-----------------------------------------------------------------------
!
      end module set_merged_groups
