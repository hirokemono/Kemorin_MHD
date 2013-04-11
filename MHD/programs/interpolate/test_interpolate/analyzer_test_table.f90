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
      use m_parallel_var_dof
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
      use m_2nd_geometry_param
!
      use input_control_itp_mesh
      use const_mesh_info
      use set_smp_size_4_2nd
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
      call s_input_control_itp_mesh
!
      call time_prog_barrier
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
      call time_prog_barrier
!
!     --------------------- 
!
      if (my_rank .lt. ndomain_dest) then
        call s_count_smp_size_4_2nd
        if(i_debug.eq.iflag_full_msg) call check_smp_size_2nd(my_rank)
      end if
!
      call time_prog_barrier
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_interpolated_geometry
      use m_2nd_geometry_param
      use mesh_interpolation
!
!
       if (iflag_debug.eq.1) write(*,*) 'allocate_interpolate_geometry'
      call allocate_interpolate_geometry(nnod_2nd)
!
       if (iflag_debug.eq.1) write(*,*) 'interpolation_4_mesh_test'
      call interpolation_4_mesh_test
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_table
