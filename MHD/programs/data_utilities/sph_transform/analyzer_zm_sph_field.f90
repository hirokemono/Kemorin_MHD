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
      use m_spheric_global_ranks
!
      use SPH_analyzer_sph_trans
      use SPH_analyzer_back_trans_old
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
      num_elapsed = 68
      call allocate_elapsed_times
!
      elapse_labels(12) = 'Visualizatio time         '
!
      elapse_labels(60) = 'Sectioning initialization.    '
      elapse_labels(61) = 'Isosurfaceing initialization.    '
      elapse_labels(62) = 'Volume rendering initialization.    '
      elapse_labels(63) = 'fieldline initialization.    '
!
      elapse_labels(65) = 'Sectioning.    '
      elapse_labels(66) = 'Isosurfaceing.    '
      elapse_labels(67) = 'Volume rendering.    '
      elapse_labels(68) = 'fieldline.    '
!
      elapse_labels(num_elapsed) = 'Communication time        '
!
!     ---------------------
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans                                   &
     &   (t_STR, mesh_file_STR, ucd_SPH_TRNS, rj_fld_trans,             &
     &    d_gauss_trans, field_STR, WK_sph_TRNS)
      call set_ctl_data_4_pick_zm
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh(sph_mesh_trans%sph,               &
     &    sph_mesh_trans%sph_comms, sph_mesh_trans%sph_grps,            &
     &    femmesh_STR%mesh, femmesh_STR%group, elemesh_STR,             &
     &    mesh_file_STR, gen_sph1)
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans(viz_step_STR, ele_4_nod_SPH_TRANS, &
     &    jacobians_STR, ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_sph_trans'
      call SPH_initialize_sph_trans(sph_mesh_trans, rj_fld_trans)
!
!    Set field IOP array by spectr fields
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans(field_file_param,                &
     &    rj_fld_trans, sph_trns_IO)
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
      use m_ctl_params_sph_trans
      use sph_rtp_zonal_rms_data
      use coordinate_convert_4_sph
!
      integer(kind=kint ) :: visval, i_step
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
!
!   Input field data
        call FEM_analyze_sph_trans(i_step, time_IO_TRNS, visval)
!
!   Take zonal RMS
        if (iflag_debug.gt.0) write(*,*) 'zonal_mean_all_rtp_field'
        call overwrite_nodal_xyz_2_sph                                  &
     &     (femmesh_STR%mesh%node, field_STR)
        call zonal_mean_all_rtp_field(sph_mesh_trans%sph%sph_rtp,       &
     &     femmesh_STR%mesh%node, field_STR)
!
        call set_ucd_file_prefix(zonal_udt_head, ucd_SPH_TRNS)
        call FEM_analyze_back_trans(time_IO_TRNS, ucd_SPH_TRNS, i_step, &
     &      viz_step_STR, visval)
!
        if(visval .eq. 0) then
          call visualize_all(viz_step_STR, t_STR%time_d,                &
     &        femmesh_STR%mesh, femmesh_STR%group, elemesh_STR,         &
     &        field_STR, ele_4_nod_SPH_TRANS, jacobians_STR)
        end if
      end do
!
      call FEM_finalize_sph_trans(ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
      call output_elapsed_times
!
      end subroutine analyze_zm_sph_field
!
! ----------------------------------------------------------------------
!
      end module analyzer_zm_sph_field
