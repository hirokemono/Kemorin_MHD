!analyzer_zm_transform.f90
!      module analyzer_zm_transform
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
      module analyzer_zm_transform
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
      use m_work_time
      use m_SPH_transforms
      use t_ctl_params_sph_trans
!
      use FEM_analyzer_sph_trans
      use SPH_analyzer_sph_trans
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_zm_trans
!
      use t_SPH_mesh_field_data
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
!
! ----   read controls
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans(spt_ctl1)
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans(spt_ctl1, t_STR, SPH_TRNS,        &
     &                                FEM_STR1, SPH_STR1, VIZ_D_STR1)
      call set_ctl_data_4_zm_trans(spt_ctl1, SPH_STR1)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (SPH_STR1%FEM_mesh_flags, SPH_STR1%sph_file_param, SPH_TRNS,   &
     &    FEM_STR1%geofem, FEM_STR1%mesh_file_IO)
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_sph_trans'
      call FEM_initialize_sph_trans                                     &
     &   (t_STR%init_d, t_STR%ucd_step, FEM_STR1)
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_init_sph_zm_trans'
      call SPH_initialize_sph_trans(SPH_TRNS, SPH_STR1)
!
!    Set field IOP array by spectr fields
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans(SPH_TRNS%fld, SPH_STR1%fld_IO)
!
      end subroutine init_zm_trans
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zm_trans
!
      integer(kind = kint) :: i_step
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
!   Input field data
        call FEM_analyze_sph_trans(i_step, t_STR%ucd_step, FEM_STR1)
!
!   Spherical transform
        call SPH_analyze_sph_zm_trans(i_step, FEM_STR1%geofem,          &
     &      FEM_STR1%field, SPH_TRNS, SPH_STR1)
      end do
!
      call FEM_finalize_sph_trans(t_STR%ucd_step, FEM_STR1)
      call output_elapsed_times
!
      end subroutine analyze_zm_trans
!
! ----------------------------------------------------------------------
!
      end module analyzer_zm_transform
