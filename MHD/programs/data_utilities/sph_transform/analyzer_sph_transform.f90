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
      use calypso_mpi
      use m_work_time
      use t_field_data_IO
!
      use m_SPH_transforms
      use FEM_analyzer_sph_trans
      use SPH_analyzer_sph_trans
!
      implicit none
!
      type(field_IO), save, private :: sph_trns_IO
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
      use m_ctl_params_sph_trans
      use m_spheric_parameter
      use m_sph_spectr_data
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
      call set_control_4_sph_transform(ucd_SPH_TRNS, rj_fld1)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh(sph1, comms_sph1, sph_grps1,      &
     &    femmesh_STR%mesh, femmesh_STR%group, elemesh_STR)
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_sph_trans'
      call FEM_initialize_sph_trans
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_sph_trans'
      call SPH_initialize_sph_trans
!
!    Set field IOP array by spectr fields
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans(sph1%sph_rj, sph_trns_IO)
!
      end subroutine initialize_sph_transform
!
! ----------------------------------------------------------------------
!
      subroutine analyze_sph_transform
!
      use m_t_step_parameter
!
      integer(kind = kint) :: i_step, visval
!
!
      do i_step = i_step_init, i_step_number
!
!   Input field data
        call FEM_analyze_sph_trans(i_step, visval)
!
!   Spherical transform
        call SPH_analyze_sph_trans(i_step, sph_trns_IO)
      end do
!
      call FEM_finalize_sph_trans(ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
        end subroutine analyze_sph_transform
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_transform
