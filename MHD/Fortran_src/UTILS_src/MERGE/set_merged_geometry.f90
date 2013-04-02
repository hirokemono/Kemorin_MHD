!
!      module set_merged_geometry
!
!      Written by H. Matsui
!
!      subroutine set_merged_mesh_and_group
!      subroutine set_merged_node_and_element
!      subroutine set_overlapped_mesh_and_group
!
      module set_merged_geometry
!
      use m_precision
!
      use m_geometry_data_4_merge
      use set_geometry_to_merge
      use count_number_with_overlap
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_merged_mesh_and_group
!
      use const_merged_groups
!
!
!      write(*,*) 'allocate_number_of_mesh'
      call allocate_number_of_mesh
      call allocate_subdomain_grp_stack
!
!     count number of node for each domain
!
!       write(*,*) 'count_number_w_overlap'
       call count_number_w_overlap
!
!     array allocation
!
!      write(*,*) 'allocate_geometry_data_4_merge'
      call allocate_geometry_data_4_merge
!
!  set mesh_information
!
!      write(*,*) 'set_geometry_data_2_merge'
      call set_geometry_data_2_merge
!
!      write(*,*) 'count_num_group_w_overlap'
      call count_num_group_w_overlap
!
!      write(*,*) 'const_merged_mesh_groups'
      call const_merged_mesh_groups
!
      end subroutine set_merged_mesh_and_group
!
!  ---------------------------------------------------------------------
!
      subroutine set_merged_node_and_element
!
!
!      write(*,*) 'allocate_number_of_mesh'
      call allocate_number_of_mesh
      call allocate_subdomain_grp_stack
!
!     count number of node for each domain
!
!       write(*,*) 'count_number_w_overlap'
       call count_number_w_overlap
!
!     array allocation
!
!      write(*,*) 'allocate_geometry_data_4_merge'
      call allocate_geometry_data_4_merge
!
!  set mesh_information
!
!       write(*,*) 'set_geometry_data_2_merge'
       call set_geometry_data_2_merge
!
!
      end subroutine set_merged_node_and_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_overlapped_mesh_and_group
!
      use const_overlap_groups
!
!
!       write(*,*) 'allocate_number_of_mesh'
      call allocate_number_of_mesh
      call allocate_subdomain_grp_stack
!
!     count number of node for each domain
!
       write(*,*) 'count_number_w_overlap'
       call count_number_w_overlap
!
!     array allocation
!
       write(*,*) 'allocate_geometry_data_4_merge'
      call allocate_geometry_data_4_merge
!
!  set mesh_information
!
       write(*,*) 'set_geometry_data_w_overlap'
       call set_geometry_data_w_overlap
!
!
       call count_num_group_w_overlap
!
       write(*,*) 'const_merged_overlapped_groups'
       call const_merged_overlapped_groups
!
      end subroutine set_overlapped_mesh_and_group
!
!  ---------------------------------------------------------------------
!
      end module set_merged_geometry
