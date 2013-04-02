!
!      module set_merged_groups
!
!      subroutine allocate_flags_merged_grp
!      subroutine deallocate_flags_merged_grp
!
!      subroutine count_merged_node_group
!      subroutine set_merged_node_group
!
!      subroutine count_merged_element_group
!      subroutine set_merged_element_group
!
!      subroutine count_merged_surface_group
!      subroutine set_merged_surface_group
!
!      Written by H. Matsui on july, 2005
!
      module set_merged_groups
!
      use m_precision
!
      use m_geometry_constants
      use m_geometry_data_4_merge
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
      subroutine allocate_flags_merged_grp
!
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
      subroutine count_merged_node_group
!
      integer(kind = kint) :: inod, jgrp, icou
!
!
      do jgrp = 1, merged_grp%nod_grp%num_grp
        call mark_node_by_global_address                                &
     &      (merged_grp%nod_grp%grp_name(jgrp) )
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
      subroutine set_merged_node_group
!
      integer(kind = kint) :: inod, jgrp, icou
!
!
      do jgrp = 1, merged_grp%nod_grp%num_grp
        call mark_node_by_global_address                                &
     &      (merged_grp%nod_grp%grp_name(jgrp) )
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
      subroutine count_merged_element_group
!
      integer(kind = kint) :: iele, jgrp, icou
!
!
      do jgrp = 1, merged_grp%ele_grp%num_grp
        call mark_ele_by_global_address                                 &
     &      (merged_grp%ele_grp%grp_name(jgrp))
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
      subroutine set_merged_element_group
!
      integer(kind = kint) :: iele, jgrp, icou
!
!
      do jgrp = 1, merged_grp%ele_grp%num_grp
        call mark_ele_by_global_address                                 &
     &      (merged_grp%ele_grp%grp_name(jgrp))
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
      subroutine count_merged_surface_group
!
      integer (kind = kint) :: icou, jgrp, iele, isurf
!
!
      do jgrp = 1, merged_grp%surf_grp%num_grp
        call mark_surf_by_global_address                                &
     &      (merged_grp%surf_grp%grp_name(jgrp))
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
      subroutine set_merged_surface_group
!
      integer (kind = kint) :: icou, jgrp, iele, isurf
!
!
!
      do jgrp = 1, merged_grp%surf_grp%num_grp
        call mark_surf_by_global_address                                &
     &      (merged_grp%surf_grp%grp_name(jgrp))
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
      subroutine mark_node_by_global_address(target_name)
!
      character(len=kchara), intent(in) :: target_name
!
      integer(kind = kint) :: ip, igrp, kst, ked, k, ii, inod
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
              inod = subdomain(ip)%node%inod_global(ii)
              iflag_nod_grp(inod) = 1
            end do
          end if
        end do
      end do
!
      end subroutine mark_node_by_global_address
!
!-----------------------------------------------------------------------
!
      subroutine mark_ele_by_global_address(target_name)
!
      character(len=kchara), intent(in) :: target_name
!
      integer(kind = kint) :: ip, igrp, kst, ked, k, ii, iele
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
              iele = subdomain(ip)%ele%iele_global(ii)
              iflag_ele_grp(iele) = 1
            end do
          end if
        end do
      end do
!
      end subroutine mark_ele_by_global_address
!
!-----------------------------------------------------------------------
!
      subroutine mark_surf_by_global_address(target_name)
!
      character(len=kchara), intent(in) :: target_name
!
!
      integer(kind = kint) :: ip, igrp, kst, ked, k, ii, iele, isurf
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
              iele =  subdomain(ip)%ele%iele_global(ii)
              isurf = sub_surf_grp(ip)%item_sf_grp(2,k)
              iflag_sf_grp(iele,isurf) = iflag_sf_grp(iele,isurf) + 1
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
