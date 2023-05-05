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
      use t_ctl_params_sph_trans
      use t_mesh_SR
!
      use calypso_mpi
      use FEM_analyzer_sph_trans
      use SPH_analyzer_sph_trans
!
      implicit none
!
      character (len = kchara), parameter, private                      &
     &                :: fname_sph_trns_ctl = 'ctl_sph_transform'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_transform
!
      use t_SPH_mesh_field_data
      use input_controls_sph_trans
!
!     ---------------------
!
      call init_elapse_time_by_TOTAL
!
! ----   read controls
      if (iflag_debug.gt.0) write(*,*) 'set_control_sph_fwd_trans'
      call input_control_sph_fwd_trans(fname_sph_trns_ctl, spt_ctl1,    &
     &    t_STR, SPH_TRNS, FEM_STR1, SPH_STR1)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (SPH_STR1%FEM_mesh_flags, SPH_STR1%sph_file_param, SPH_TRNS,   &
     &    FEM_STR1%geofem, FEM_STR1%mesh_file_IO)
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_sph_trans'
      call FEM_initialize_sph_trans(t_STR%init_d, t_STR%ucd_step,       &
     &                              FEM_STR1, m_SR5)
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_sph_trans'
      call SPH_initialize_sph_trans(SPH_TRNS, SPH_STR1,                 &
     &                              m_SR5%SR_sig, m_SR5%SR_r)
!
!    Set field IOP array by spectr fields
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans(SPH_TRNS%fld, SPH_STR1%fld_IO)
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'initialize_sph_transform end'
!
      end subroutine initialize_sph_transform
!
! ----------------------------------------------------------------------
!
      subroutine analyze_sph_transform
!
      integer(kind = kint) :: i_step
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
!
!   Input field data
        call FEM_analyze_sph_trans(i_step, t_STR%ucd_step,              &
     &                             FEM_STR1, m_SR5)
!
!   Spherical transform
        call SPH_analyze_sph_trans                                      &
     &     (i_step, FEM_STR1%geofem, FEM_STR1%field, SPH_TRNS,          &
     &      SPH_STR1, m_SR5%SR_sig, m_SR5%SR_r)
      end do
!
      call FEM_finalize_sph_trans(t_STR%ucd_step, FEM_STR1)
      call output_elapsed_times
!
      end subroutine analyze_sph_transform
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_transform
