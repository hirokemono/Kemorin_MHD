!
!     module set_numnod_4_plane
!
      module set_numnod_4_plane
!
!      Written by H. Matsui
!
      implicit    none
!
!      subroutine s_set_numnod_4_plane
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine s_set_numnod_4_plane
!
      use m_geometry_data_4_merge
      use m_size_4_plane
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
