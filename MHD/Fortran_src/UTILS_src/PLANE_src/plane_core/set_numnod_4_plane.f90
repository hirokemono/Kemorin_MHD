!
!     module set_numnod_4_plane
!
!      Written by H. Matsui
!
!!      subroutine s_set_numnod_4_plane(c_size, merge_tbl)
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
      subroutine s_set_numnod_4_plane(c_size, merge_tbl)
!
      use t_merged_geometry_data
      use t_size_of_cube
!
      type(size_of_cube), intent(in) :: c_size
      type(merged_stacks), intent(inout) :: merge_tbl
!
!
       merge_tbl%inter_nod_m = c_size%nx_all * c_size%ny_all            &
     &                        * c_size%nz_all
       merge_tbl%inter_ele_m = c_size%nx_all * c_size%ny_all            &
     &                        * (c_size%nz_all - 1)
!
       write(*,*) 'numnod_initial', merge_tbl%inter_nod_m
!
      end subroutine s_set_numnod_4_plane
!
!  --------------------------------------------------------------------
!
      end module set_numnod_4_plane
