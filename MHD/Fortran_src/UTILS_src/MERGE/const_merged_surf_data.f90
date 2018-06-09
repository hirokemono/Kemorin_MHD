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
!      subroutine s_const_merged_surf_data(mgd_mesh)
!
!      call construct_surface_data                                       &
!     &   (mgd_mesh%merged%node, mgd_mesh%merged%ele,                    &
!     &    mgd_mesh%merged_surf)
!
!   count number of element for each domain
!
!      call alloc_num_surface_merge(mgd_mesh)
!      call count_nsurf_4_each_domain                                    &
!     &   (mgd_mesh%num_pe, mgd_mesh%merge_tbl,                          &
!     &    mgd_mesh%merged_surf, mgd_mesh%istack_surfpe)
!      write(*,*) 'count_nsurf_4_each_domain end'
!
!      end subroutine s_const_merged_surf_data
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_nsurf_4_each_domain                              &
     &         (num_pe, merge_tbl, merged_surf, istack_surfpe)
!
      use t_merged_geometry_data
      use t_surface_data
!
      integer(kind = kint), intent(in) :: num_pe
      type(merged_stacks), intent(in) :: merge_tbl
      type(surface_data), intent(in) :: merged_surf
!
      integer(kind = kint), intent(inout) :: istack_surfpe(0:num_pe)
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
