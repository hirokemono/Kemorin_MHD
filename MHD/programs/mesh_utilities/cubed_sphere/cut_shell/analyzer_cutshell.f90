

      module  analyzer_cutshell
!
!     Written by H. Matsui on Oct., 2007
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      integer(kind = kint), parameter, private :: my_rank = izero
!
!      subroutine initialize_cutshell
!      subroutine analyze_cutshell
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
      use cal_mesh_position
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
      call set_spherical_position
!
      end subroutine  initialize_cutshell
!
!   --------------------------------------------------------------------
!
      subroutine analyze_cutshell
!
      use m_read_mesh_data
      use m_2nd_geometry_data
      use const_cutshell_mesh
      use load_2nd_mesh_data
!
!
      call s_const_reduced_geometry
!
      comm_2nd%num_neib = 0
      call allocate_type_comm_tbl_num(comm_2nd)
      call allocate_type_comm_tbl_item(comm_2nd)
!
      mesh_file_head = modified_mesh_head
      call output_2nd_mesh(my_rank)
      call deallocate_2nd_mesh
!
      end subroutine analyze_cutshell
!
!   --------------------------------------------------------------------
!
      end module analyzer_cutshell
