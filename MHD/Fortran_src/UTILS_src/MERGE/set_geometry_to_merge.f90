!
!      module set_geometry_to_merge
!
!      Written by H. Matsui
!      Modified by H. Matsui on Dec., 2006
!
!!      subroutine set_geometry_data_2_merge(mgd_mesh)
!!      subroutine set_geometry_data_w_overlap(mgd_mesh)
!!        type(merged_mesh), intent(inout) :: mgd_mesh
!
      module set_geometry_to_merge
!
      use m_precision
!
       use t_mesh_data_4_merge
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
      subroutine set_geometry_data_2_merge(mgd_mesh)
!
       use set_read_boundary_2_merge
!
      type(merged_mesh), intent(inout) :: mgd_mesh
!
       integer (kind = kint) :: ip
!
! ========================
! * PES loops 
! ========================
      call allocate_ioverlap_nod(mgd_mesh%merged)
      call allocate_ioverlap_ele(mgd_mesh%merged)
!
      mgd_mesh%merge_tbl%nnod_merged = 0
      mgd_mesh%merge_tbl%nele_merged = 0
      do ip = 1, mgd_mesh%num_pe
        call copy_read_nodal_data_2_merge                               &
     &     (ip, mgd_mesh%subdomain(ip), mgd_mesh%merge_tbl,             &
     &      mgd_mesh%merged)
        call copy_read_ele_data_2_merge                                 &
     &     (ip, mgd_mesh%subdomain(ip), mgd_mesh%merge_tbl,             &
     &      mgd_mesh%merged)
      end do
!
      end subroutine set_geometry_data_2_merge
!
!  ---------------------------------------------------------------------
!
      subroutine set_geometry_data_w_overlap(mgd_mesh)
!
      use set_read_boundary_2_merge
!
      type(merged_mesh), intent(inout) :: mgd_mesh
!
      integer (kind = kint) :: ip
!
!
! ========================
! * PES loops 
! ========================
      mgd_mesh%merge_tbl%nnod_merged = 0
      mgd_mesh%merge_tbl%nele_merged = 0
      do ip =1, mgd_mesh%num_pe
        call copy_read_nodal_data_w_overlap                             &
     &     (ip, mgd_mesh%subdomain(ip), mgd_mesh%merge_tbl,             &
     &      mgd_mesh%merged)
        call copy_read_ele_data_w_overlap                               &
     &     (ip, mgd_mesh%subdomain(ip), mgd_mesh%merge_tbl,             &
     &      mgd_mesh%merged)
!
!   convert node and element ID
!
        call cvt_ele_connect_w_overlap                                  &
     &     (ip, mgd_mesh%subdomain(ip), mgd_mesh%merge_tbl,             &
     &      mgd_mesh%merged)
!
        call cvt_group_4_overlap(mgd_mesh%sub_nod_grp(ip),              &
     &      mgd_mesh%merge_tbl%istack_nod(ip-1))
        call cvt_group_4_overlap(mgd_mesh%sub_ele_grp(ip),              &
     &      mgd_mesh%merge_tbl%istack_ele(ip-1))
        call cvt_surf_grp_4_overlap(mgd_mesh%sub_surf_grp(ip),          &
     &      mgd_mesh%merge_tbl%istack_ele(ip-1))
      end do
!
      end subroutine set_geometry_data_w_overlap
!
!  ---------------------------------------------------------------------
!
      end module set_geometry_to_merge
