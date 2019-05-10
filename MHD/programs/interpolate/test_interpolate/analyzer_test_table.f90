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
      use t_step_parameter
      use t_mesh_data
      use t_interpolate_table
      use t_IO_step_parameter
      use t_ctl_data_gen_table
      use t_ctl_params_4_gen_table
!
      implicit none
!
      type(ctl_data_gen_table), save :: gtbl_ctl1
      type(ctl_params_4_gen_table), save :: gen_itp_p1
!
      type(mesh_data), save :: org_femmesh
      type(mesh_data), save :: new_femmesh
!
      type(interpolate_table), save :: itp_test
!
      type(time_step_param), save :: t_ITP
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use input_control_interpolate
      use const_mesh_information
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
      call s_input_control_interpolate(gen_itp_p1, gtbl_ctl1,           &
     &    org_femmesh, new_femmesh, itp_test, t_ITP, ierr)
!
!     --------------------- 
!
      if (my_rank .lt. gen_itp_p1%ndomain_dest) then
        call count_size_4_smp_mesh_type                                 &
     &     (new_femmesh%mesh%node, new_femmesh%mesh%ele)
        if(i_debug.eq.iflag_full_msg) then
          call check_mesh_smp_size(my_rank, new_femmesh%mesh)
        end if
      end if
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use mesh_interpolation
!
!
      if (iflag_debug.eq.1) write(*,*) 'interpolation_4_mesh_test'
      call interpolation_4_mesh_test                                    &
     &   (org_femmesh%mesh, new_femmesh%mesh, itp_test)
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_test_table
