!
!      module set_geometry_to_merge
!
!      Written by H. Matsui
!      Modified by H. Matsui on Dec., 2006
!
!      subroutine set_geometry_data_2_merge
!      subroutine set_geometry_data_w_overlap
!
      module set_geometry_to_merge
!
      use m_precision
!
      use set_read_geometry_2_merge
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_geometry_data_2_merge
!
       use m_geometry_data_4_merge
       use set_read_boundary_2_merge
!
       integer (kind = kint) :: ip
!
! ========================
! * PES loops 
! ========================
      call allocate_ioverlap_nod(merged)
      call allocate_ioverlap_ele(merged)
!
      merge_tbl%nnod_merged = 0
      merge_tbl%nele_merged = 0
      do ip = 1, mgd_mesh1%num_pe
        call copy_read_nodal_data_2_merge                               &
     &     (ip, mgd_mesh1%subdomain(ip), merge_tbl, merged)
        call copy_read_ele_data_2_merge                                 &
     &     (ip, mgd_mesh1%subdomain(ip), merge_tbl, merged)
      end do
!
      end subroutine set_geometry_data_2_merge
!
!  ---------------------------------------------------------------------
!
      subroutine set_geometry_data_w_overlap
!
      use m_geometry_data_4_merge
      use set_read_boundary_2_merge
!
      integer (kind = kint) :: ip
!
!
! ========================
! * PES loops 
! ========================
      merge_tbl%nnod_merged = 0
      merge_tbl%nele_merged = 0
      do ip =1, mgd_mesh1%num_pe
        call copy_read_nodal_data_w_overlap                             &
     &     (ip, mgd_mesh1%subdomain(ip), merge_tbl, merged)
        call copy_read_ele_data_w_overlap                               &
     &     (ip, mgd_mesh1%subdomain(ip), merge_tbl, merged)
!
!   convert node and element ID
!
        call cvt_ele_connect_w_overlap                                  &
     &     (ip, mgd_mesh1%subdomain(ip), merge_tbl, merged)
!
        call cvt_group_4_overlap(mgd_mesh1%sub_nod_grp(ip),             &
     &      merge_tbl%istack_nod(ip-1))
        call cvt_group_4_overlap(mgd_mesh1%sub_ele_grp(ip),             &
     &      merge_tbl%istack_ele(ip-1))
        call cvt_surf_grp_4_overlap(mgd_mesh1%sub_surf_grp(ip),         &
     &      merge_tbl%istack_ele(ip-1))
      end do
!
      end subroutine set_geometry_data_w_overlap
!
!  ---------------------------------------------------------------------
!
      end module set_geometry_to_merge
