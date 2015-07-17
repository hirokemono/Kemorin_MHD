!set_group_data_4_IO.f90
!     module set_group_data_4_IO
!
!      written by H. Matsui on Dec., 2006
!
!      subroutine copy_group_data_from_IO
!      subroutine copy_group_data_to_IO
!
      module set_group_data_4_IO
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_group_data_from_IO
!
      use m_group_data
      use set_group_types_4_IO
!
!
      call set_nod_grp_type_from_IO(nod_grp1)
      call set_ele_grp_type_from_IO(ele_grp1)
      call set_surf_grp_type_from_IO(sf_grp1)
!
      end subroutine copy_group_data_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_group_data_to_IO
!
      use m_group_data
      use set_group_types_4_IO
!
!
      call set_node_grp_type_to_IO(nod_grp1)
      call set_ele_grp_type_to_IO(ele_grp1)
      call set_surface_grp_type_to_IO(sf_grp1)
!
      call deallocate_grp_type(nod_grp1)
      call deallocate_grp_type(ele_grp1)
      call deallocate_sf_grp_type(sf_grp1)
!
      end subroutine copy_group_data_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_group_data_4_IO
