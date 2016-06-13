!analyzer_zm_sph_field.f90
!      module analyzer_zm_sph_field
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine init_zm_sph_field
!      subroutine analyze_zm_sph_field
!
      module analyzer_zm_sph_field
!
      use m_precision
      use calypso_mpi
!
      use m_spheric_data_transform
      use m_SPH_transforms
      use m_work_time
!
      use SPH_analyzer_sph_trans
      use SPH_analyzer_back_trans
      use FEM_analyzer_sph_trans
      use FEM_analyzer_back_trans
      use visualizer_all
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_zm_sph_field
!
      use m_ctl_data_4_sph_trans
      use m_ctl_params_sph_trans
      use parallel_load_data_4_sph
!
!
      num_elapsed = 30
      call allocate_elapsed_times
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans                                   &
     &   (ucd_SPH_TRNS, rj_fld_trans, d_gauss_trans)
      call set_ctl_data_4_pick_zm
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh(sph_mesh_trans%sph,               &
     &    sph_mesh_trans%sph_comms, sph_mesh_trans%sph_grps,            &
     &    femmesh_STR%mesh, femmesh_STR%group, elemesh_STR)
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans(ele_4_nod_SPH_TRANS,               &
     &    jac_STR_l, jac_STR_q, ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_sph_trans'
      call SPH_initialize_sph_trans(sph_mesh_trans, rj_fld_trans)
!
!    Set field IOP array by spectr fields
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans                                  &
     &   (sph_mesh_trans%sph%sph_rj, rj_fld_trans, sph_trns_IO)
!
!  -------------------------------
!
      call init_visualize(femmesh_STR%mesh, femmesh_STR%group,          &
     &    elemesh_STR, field_STR)
!
      end subroutine init_zm_sph_field
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zm_sph_field
!
      use m_control_params_2nd_files
      use m_t_step_parameter
      use m_ctl_params_sph_trans
      use sph_rtp_zonal_rms_data
      use coordinate_convert_4_sph
!
      integer(kind=kint ) :: visval, i_step
      integer(kind=kint ) :: istep_psf, istep_iso
      integer(kind=kint ) :: istep_pvr, istep_fline
!
!
      do i_step = i_step_init, i_step_number
!
!   Input field data
        call FEM_analyze_sph_trans(i_step, visval)
!
!   Take zonal RMS
        if (iflag_debug.gt.0) write(*,*) 'zonal_mean_all_rtp_field'
        call overwrite_nodal_xyz_2_sph                                  &
     &     (femmesh_STR%mesh%node, field_STR)
        call zonal_mean_all_rtp_field(sph_mesh_trans%sph%sph_rtp,       &
     &     femmesh_STR%mesh%node, field_STR)
!
        call set_ucd_file_prefix(zonal_udt_head, ucd_SPH_TRNS)
        call FEM_analyze_back_trans(ucd_SPH_TRNS, i_step,               &
     &      istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
        if(visval .eq. 0) then
          call visualize_all                                            &
     &       (istep_psf, istep_iso, istep_pvr, istep_fline,             &
     &        femmesh_STR%mesh, femmesh_STR%group, elemesh_STR,         &
     &        field_STR, ele_4_nod_SPH_TRANS, jac_STR_q)
        end if
      end do
!
      call FEM_finalize_sph_trans(ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
      end subroutine analyze_zm_sph_field
!
! ----------------------------------------------------------------------
!
      end module analyzer_zm_sph_field
