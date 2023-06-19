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
      use t_control_data_4_cutshell
      use t_file_IO_parameter
!
      implicit none
!
      integer, parameter, private :: my_rank = izero
!
      type(ctl_data_cutshell), save :: cutshell_ctl1
      type(mesh_data), save :: original_fem
      type(mesh_data), save :: cutted_fem
!
      integer(kind = kint) :: iflag_reduce_type1 = 1
      type(field_IO_params), save ::  original_mesh_file1
      type(field_IO_params), save ::  modified_mesh_file1
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------

      subroutine  initialize_cutshell
!
      use load_mesh_data
      use set_control_cut_shell
      use cal_mesh_position
      use const_cutshell_mesh
!
      integer(kind = kint) :: ierr
!
!
      call read_control_data_4_cutshell(cutshell_ctl1)
      if(cutshell_ctl1%i_cutshell_ctl .ne. 1)                            &
     &                   stop 'control file is broken'
!
      call s_set_control_4_cutshell(cutshell_ctl1, iflag_reduce_type1,  &
     &    original_mesh_file1, modified_mesh_file1)
!
!  read global mesh
!
      call input_mesh(original_mesh_file1, my_rank,                     &
     &    original_fem%mesh, original_fem%group, ierr)
      if(ierr .ne. 0) stop 'Mesh data is wrong!'
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_spherical_position'
      call set_spherical_position(original_fem%mesh%node)
!
      end subroutine  initialize_cutshell
!
!   --------------------------------------------------------------------
!
      subroutine analyze_cutshell
!
      use const_cutshell_mesh
      use load_mesh_data
!
!
      call s_const_reduced_geometry                                     &
     &   (iflag_reduce_type1, original_fem%mesh, original_fem%group,    &
     &    cutted_fem%mesh, cutted_fem%group)
!
      cutted_fem%mesh%nod_comm%num_neib = 0
      call alloc_comm_table_num(cutted_fem%mesh%nod_comm)
      call alloc_comm_table_item(cutted_fem%mesh%nod_comm)
!
      call output_mesh(modified_mesh_file1, my_rank,                    &
     &                 cutted_fem%mesh, cutted_fem%group)
      call dealloc_mesh_data(cutted_fem%mesh, cutted_fem%group)
!
      end subroutine analyze_cutshell
!
!   --------------------------------------------------------------------
!
      end module analyzer_cutshell
