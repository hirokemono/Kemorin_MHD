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
      use m_ctl_params_sph_trans
      use m_sph_global_parameter
      use m_sph_1d_global_index
      use m_sph_mesh_1d_connect
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
      use m_ctl_data_4_sph_trans
      use parallel_load_data_4_sph
!
!
      num_elapsed = 30
      call allocate_elapsed_times
!
!     --------------------- 
!
! ----   read controls
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_4_sph_transform'
      call set_control_4_sph_transform                                  &
     &   (t_STR, mesh_file_STR, ucd_SPH_TRNS, rj_fld_trans,             &
     &    d_gauss_trans, field_STR, WK_sph_TRNS)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh(sph_mesh_trans%sph,               &
     &    sph_mesh_trans%sph_comms, sph_mesh_trans%sph_grps,            &
     &    femmesh_STR%mesh, femmesh_STR%group, elemesh_STR,             &
     &    mesh_file_STR, sph_dbc, sph_lcp, stk_lc1d, sph_gl1d, stbl)
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_sph_trans'
      call FEM_initialize_sph_trans(field_file_param, time_IO_TRNS)
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_sph_trans'
      call SPH_initialize_sph_trans(sph_mesh_trans, rj_fld_trans)
!
!    Set field IOP array by spectr fields
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans(field_file_param,                &
     &    rj_fld_trans, sph_trns_IO)
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
        call FEM_analyze_sph_trans(i_step, time_IO_TRNS, visval)
!
!   Spherical transform
        call SPH_analyze_sph_trans                                      &
     &     (i_step, sph_mesh_trans, rj_fld_trans, sph_trns_IO)
      end do
!
      call FEM_finalize_sph_trans(ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
      call output_elapsed_times
!
      end subroutine analyze_sph_transform
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_transform
