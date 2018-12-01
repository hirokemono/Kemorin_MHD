!analyzer_sph_transform.f90
!      module analyzer_sph_transform
!
!      subroutine initialize_sph_transform
!      subroutine analyze_sph_transform
!
!      modified by H. Matsui on Jan., 2008
!
!      Input indexing data prefix:: sph_file_prefix
!
!      Input field  data prefix:: field_file_prefix
!      Input spectr data prefix:: restart_file_prefix
!
!
      module analyzer_sph_transform
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
      use m_SPH_transforms
      use m_spheric_data_transform
      use t_ctl_params_sph_trans
!
      use calypso_mpi
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
      subroutine initialize_sph_transform
!
      use parallel_load_data_4_sph
!
!
      call init_elapse_time_by_TOTAL
!
!     --------------------- 
!
! ----   read controls
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans(spt_ctl1)
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_4_sph_transform'
      call set_control_4_sph_transform                                  &
     &   (spt_ctl1, t_STR, viz_step_STR, files_STR,                     &
     &    SPH_TRNS%fld, d_gauss_trans, field_STR, WK_sph_TRNS)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh(files_STR%FEM_mesh_flags,         &
     &    SPH_TRNS%sph, SPH_TRNS%comms, SPH_TRNS%groups,                &
     &    femmesh_STR, elemesh_STR, files_STR%mesh_file_IO,             &
     &    gen_sph_TRNS)
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_sph_trans'
      call FEM_initialize_sph_trans                                     &
     &   (files_STR%ucd_file_IO, time_IO_TRNS)
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_sph_trans'
      call SPH_initialize_sph_trans(SPH_TRNS)
!
!    Set field IOP array by spectr fields
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans(SPH_TRNS%fld, sph_trns_IO)
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_transform end'
!
      end subroutine initialize_sph_transform
!
! ----------------------------------------------------------------------
!
      subroutine analyze_sph_transform
!
      integer(kind = kint) :: i_step, visval
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
!
!   Input field data
        call FEM_analyze_sph_trans                                      &
     &     (i_step, files_STR%org_ucd_file_IO, time_IO_TRNS, visval)
!
!   Spherical transform
        call SPH_analyze_sph_trans                                      &
     &     (i_step, files_STR%sph_file_IO, SPH_TRNS, sph_trns_IO)
      end do
!
      call FEM_finalize_sph_trans                                       &
     &   (files_STR%org_ucd_file_IO, m_ucd_SPH_TRNS)
!
      call output_elapsed_times
!
      end subroutine analyze_sph_transform
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_transform
