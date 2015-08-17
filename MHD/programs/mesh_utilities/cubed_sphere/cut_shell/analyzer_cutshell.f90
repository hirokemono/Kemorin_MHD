!analyzer_cutshell.f90
!     Written by H. Matsui on Oct., 2007
!
!      subroutine initialize_cutshell
!      subroutine analyze_cutshell
!
      module  analyzer_cutshell
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_mesh_data
      use t_phys_data
!
      implicit none
!
      integer(kind = kint), parameter, private :: my_rank = izero
!
      type(mesh_data), save :: cutted_fem
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------

      subroutine  initialize_cutshell
!
      use m_control_data_4_cutshell
      use m_geometry_data
      use m_read_mesh_data
      use load_mesh_data
      use set_control_cut_shell
      use const_mesh_info
      use cal_mesh_position_type
      use const_cutshell_mesh
!
!
      call read_control_data_4_cutshell
      call s_set_control_4_cutshell
!
!  read global mesh
!
      mesh_file_head = original_mesh_head
      call input_mesh(my_rank)
!
      call deallocate_element_geometry
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
      call set_local_element_info
      call set_spherical_position_type(node1)
!
      end subroutine  initialize_cutshell
!
!   --------------------------------------------------------------------
!
      subroutine analyze_cutshell
!
      use m_read_mesh_data
      use const_cutshell_mesh
      use load_mesh_type_data
!
!
      call s_const_reduced_geometry(cutted_fem%mesh, cutted_fem%group)
!
      cutted_fem%mesh%nod_comm%num_neib = 0
      call allocate_type_comm_tbl_num(cutted_fem%mesh%nod_comm)
      call allocate_type_comm_tbl_item(cutted_fem%mesh%nod_comm)
!
      mesh_file_head = modified_mesh_head
      call output_mesh_type(my_rank, cutted_fem)
!
      end subroutine analyze_cutshell
!
!   --------------------------------------------------------------------
!
      end module analyzer_cutshell
