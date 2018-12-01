!analyzer_zonal_rms_sph.f90
!      module analyzer_zonal_rms_sph
!..................................................
!
!      modified by H. Matsui on June, 2012
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_zonal_rms_sph
!
      use m_precision
      use calypso_mpi
!
      use m_SPH_transforms
      use m_work_time
      use m_spheric_data_transform
!
      use SPH_analyzer_sph_trans
      use SPH_analyzer_back_trans_old
      use FEM_analyzer_sph_trans
      use FEM_analyzer_back_trans
      use t_visualizer
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
      use t_ctl_params_sph_trans
      use parallel_load_data_4_sph
      use m_elapsed_labels_4_VIZ
!
!     --------------------- 
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call append_COMM_TIME_to_elapsed
!
!     --------------------- 
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans(spt_ctl1)
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans                                   &
     &   (spt_ctl1, t_STR, viz_step_STR, files_STR,                     &
     &    SPH_TRNS%fld, d_gauss_trans, field_STR, WK_sph_TRNS)
      call set_ctl_data_4_pick_zm                                       &
     &   (spt_ctl1, files_STR%zm_source_file_param)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh(files_STR%FEM_mesh_flags,         &
     &    SPH_TRNS%sph, SPH_TRNS%comms, SPH_TRNS%groups,                &
     &    femmesh_STR, elemesh_STR, files_STR%mesh_file_IO,             &
     &    gen_sph_TRNS)
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans                                    &
     &   (files_STR%ucd_file_IO, viz_step_STR, ele_4_nod_SPH_TRANS,     &
     &    jacobians_STR, ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_sph_trans'
      call SPH_initialize_sph_trans(SPH_TRNS)
!
!    Set field IOP array by spectr fields
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans(SPH_TRNS%fld, sph_trns_IO)
!
!  -------------------------------
!
      call init_visualize(femmesh_STR, elemesh_STR, field_STR,          &
     &    spt_ctl1%viz_ctls, vizs_TRNS)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use t_ctl_params_sph_trans
      use sph_rtp_zonal_rms_data
      use coordinate_convert_4_sph
!
      integer(kind=kint ) :: visval, i_step
!
!
      do i_step = t_STR%init_d%i_time_step, t_STR%finish_d%i_end_step
!
!   Input field data
        call FEM_analyze_sph_trans                                      &
     &     (i_step, files_STR%org_ucd_file_IO, time_IO_TRNS, visval)
!
!   Take zonal RMS
        if (iflag_debug.gt.0) write(*,*) 'zonal_rms_all_rtp_field'
        call overwrite_nodal_xyz_2_sph                                  &
     &    (femmesh_STR%mesh%node, field_STR)
        call zonal_rms_all_rtp_field (SPH_TRNS%sph%sph_rtp,             &
     &      femmesh_STR%mesh%node, field_STR)
!
        call FEM_analyze_back_trans                                     &
     &     (files_STR%zonal_ucd_param, time_IO_TRNS, ucd_SPH_TRNS,      &
     &      i_step, viz_step_STR, visval)
!
        if(visval .eq. 0) then
          call visualize_all(viz_step_STR, t_STR%time_d,                &
     &        femmesh_STR, elemesh_STR, field_STR, ele_4_nod_SPH_TRANS, &
     &        jacobians_STR, vizs_TRNS)
        end if
      end do
!
      call FEM_finalize_sph_trans                                       &
     &   (files_STR%org_ucd_file_IO, m_ucd_SPH_TRNS)
!
      call copy_COMM_TIME_to_elaps
      call output_elapsed_times
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_zonal_rms_sph
