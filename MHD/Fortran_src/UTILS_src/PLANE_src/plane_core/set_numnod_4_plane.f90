!
!     module set_numnod_4_plane
!
!      Written by H. Matsui
!
!!      subroutine s_set_numnod_4_plane(merge_tbl)
!!        type(merged_stacks), intent(inout) :: merge_tbl
!
      module set_numnod_4_plane
!
      implicit    none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine s_set_numnod_4_plane(merge_tbl)
!
      use t_merged_geometry_data
      use m_size_4_plane
!
      type(merged_stacks), intent(inout) :: merge_tbl
!
!
       merge_tbl%inter_nod_m = nx_all*ny_all*nz_all
       merge_tbl%inter_ele_m = nx_all*ny_all*(nz_all-1)
!
       write(*,*) 'numnod_initial', merge_tbl%inter_nod_m
!
      end subroutine s_set_numnod_4_plane
!
!  --------------------------------------------------------------------
!
      end module set_numnod_4_plane
