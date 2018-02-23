!
!      module const_merged_surf_data
!
!     Written by H. Matsui on Jan., 2007
!
!!      subroutine s_const_merged_surf_data(mgd_mesh)
!!        type(merged_mesh), intent(inout) :: mgd_mesh
!
      module const_merged_surf_data
!
      use m_precision
!
      use m_surf_geometry_4_merge
!
      implicit    none
!
      private :: count_nsurf_4_each_domain
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_const_merged_surf_data(mgd_mesh)
!
      use t_mesh_data_4_merge
      use const_surface_data
!
      type(merged_mesh), intent(in) :: mgd_mesh
!
!   set hash data for suface elements using sum of local node ID
!
      call const_isolated_surface_t_data                                &
     &   (mgd_mesh%merged%node, mgd_mesh%merged%ele,                    &
     &    merged_surf)
!
!   count number of element for each domain
!
      call allocate_num_surface_merge(mgd_mesh%num_pe)
      call count_nsurf_4_each_domain                                    &
     &   (mgd_mesh%num_pe, mgd_mesh%merge_tbl)
!
      end subroutine s_const_merged_surf_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_nsurf_4_each_domain(num_pe, merge_tbl)
!
      use t_merged_geometry_data
!
      integer(kind = kint), intent(in) :: num_pe
      type(merged_stacks), intent(in) :: merge_tbl
!
      integer(kind = kint) :: ip, iref, ist, isurf, inod
!
!
      do ip = 1, num_pe
        iref = merge_tbl%istack_nod(ip)
        ist =  istack_surfpe(ip-1) + 1
        do isurf = ist, merged_surf%numsurf
          inod = merged_surf%ie_surf(isurf,1)
          if (inod .gt. iref) exit
          istack_surfpe(ip) = isurf
        end do
      end do
!
      end subroutine count_nsurf_4_each_domain
!
!------------------------------------------------------------------
!
      end module const_merged_surf_data
