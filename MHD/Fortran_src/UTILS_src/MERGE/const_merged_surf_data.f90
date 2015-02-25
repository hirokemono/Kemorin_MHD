!
!      module const_merged_surf_data
!
!     Written by H. Matsui on Jan., 2007
!
!      subroutine s_const_merged_surf_data
!
      module const_merged_surf_data
!
      use m_precision
!
      use m_geometry_data_4_merge
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
      subroutine s_const_merged_surf_data
!
      use m_surface_hash
      use const_surface_type_data
!
!   set hash data for suface elements using sum of local node ID
!
      call const_isolated_surface_t_data(merged%node, merged%ele,       &
     &    merged_surf)
!
!   count number of element for each domain
!
      call allocate_num_surface_merge
      call count_nsurf_4_each_domain
!
      end subroutine s_const_merged_surf_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_nsurf_4_each_domain
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
