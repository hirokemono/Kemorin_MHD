!analyzer_sph_bc_temp.f90
!
!      module analyzer_sph_bc_temp
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initilize_bc_temp
!      subroutine analyze_bc_temp
!
!..................................................
!
      module analyzer_sph_bc_temp
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initilize_bc_temp
!
      use m_ctl_data_test_bc_temp
      use m_ctl_params_test_bc_temp
      use load_mesh_data
      use const_mesh_info
!
!
!     ----- read control data
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_4_bc_temp'
      call read_control_4_bc_temp
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_params_sph_bc_temp'
      call set_ctl_params_sph_bc_temp
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh_1st'
      call input_mesh_1st(my_rank)
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_mesh_informations'
      call const_mesh_informations(my_rank)
!
       end subroutine initilize_bc_temp
!
! ----------------------------------------------------------------------
!
      subroutine analyze_bc_temp
!
      use m_group_data
      use const_sph_boundary_temp
!
      if (iflag_debug.gt.0) write(*,*) 'const_sph_temp_bc'
      call const_sph_temp_bc(nod_grp1)
!
      end subroutine analyze_bc_temp
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_bc_temp
