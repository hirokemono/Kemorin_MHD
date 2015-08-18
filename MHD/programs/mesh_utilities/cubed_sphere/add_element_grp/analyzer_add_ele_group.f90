!analyzer_add_ele_group.f90
!      module analyzer_add_ele_group
!     Written by H. Matsui on Mar., 2008
!     Modified by H. Matsui on June., 2013
!
!      subroutine initialize_add_egrp
!
!
      module  analyzer_add_ele_group
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine  initialize_add_egrp
!
      use calypso_mpi
      use m_geometry_data
      use m_control_data_add_ele_grp
      use m_add_ele_grp_parameter
      use m_element_group
      use m_read_mesh_data
      use m_work_4_add_egrp_sph
      use load_mesh_data
      use const_mesh_info
!
      use set_control_add_2d_egrp
      use set_ele_grp2_by_2d
      use set_new_2d_element_group
!
!
!
      call read_control_add_elegrp
      call s_set_control_add_2d_egrp
!
!  read global mesh
!
      mesh_file_head = original_mesh_head
      call input_mesh(my_rank)
      call const_nod_ele_infos
!
      call alloc_r_ele_cubed_sph(ele1%numele)
      call set_rele_cubed_sph(node1%numnod, ele1%numele, ele1%ie,       &
     &    node1%rr, ele1%r_ele)
!
      call allocate_work_4_add_egrp_sph(ele1%numele)
      call count_new_2d_element_group
!
      if (iflag_debug.eq.1) write(*,*) 'set_new_2d_ele_group'
      call set_new_2d_ele_group(ele_grp1)
!
      call deallocate_work_4_add_egrp_sph
!
       if (iflag_debug.eq.1) write(*,*) 'output_mesh'
      mesh_file_head = modified_mesh_head
      call output_mesh(my_rank)
!
      call dealloc_r_ele_cubed_sph
!
      end subroutine initialize_add_egrp
!
!   --------------------------------------------------------------------
!
      end module analyzer_add_ele_group
