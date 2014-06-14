!analyzer_test_table.f90
!
!      module analyzer_test_table
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine init_analyzer
!      subroutine analyze
!
!..................................................
!
      module analyzer_test_table
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
!
      use m_ctl_params_4_gen_table
      use m_2nd_geometry_data
!
      use input_control_itp_mesh
      use const_mesh_info
      use set_size_4_smp_types
!
      integer(kind = kint) :: ierr
!
!     ---------------------
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_itp_mesh'
      call s_input_control_itp_mesh(ierr)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'set_local_element_info'
      call set_local_element_info
!
!     --------------------- 
!
      if (my_rank .lt. ndomain_org) then
        if (iflag_debug.eq.1) write(*,*) 'set_nod_and_ele_infos'
        call set_nod_and_ele_infos
      end if
!
!     --------------------- 
!
      if (my_rank .lt. ndomain_dest) then
        call count_size_4_smp_mesh_type(node_2nd, ele_2nd)
        if(i_debug.eq.iflag_full_msg) call check_smp_size_2nd(my_rank)
      end if
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_interpolated_geometry
      use m_2nd_geometry_data
      use mesh_interpolation
!
!
       if (iflag_debug.eq.1) write(*,*) 'allocate_interpolate_geometry'
      call allocate_interpolate_geometry(node_2nd%numnod)
!
       if (iflag_debug.eq.1) write(*,*) 'interpolation_4_mesh_test'
      call interpolation_4_mesh_test
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_table
