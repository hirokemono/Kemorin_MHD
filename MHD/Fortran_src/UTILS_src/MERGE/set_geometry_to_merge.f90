!
!      module set_geometry_to_merge
!
!      Written by H. Matsui
!      Modified by H. Matsui on Dec., 2006
!
!!      subroutine set_geometry_data_2_merge(mgd_mesh)
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
      end module set_geometry_to_merge
