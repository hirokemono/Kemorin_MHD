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
!
      use t_field_data_IO
!
      use SPH_analyzer_sph_trans
      use SPH_analyzer_back_trans
      use FEM_analyzer_sph_trans
      use FEM_analyzer_back_trans
      use visualizer_all
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
      subroutine init_analyzer
!
      use m_ctl_data_4_sph_trans
      use m_ctl_params_sph_trans
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_group_data_sph_specr
      use m_sph_spectr_data
      use parallel_load_data_4_sph
!
!     --------------------- 
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
      call s_set_ctl_data_4_sph_trans(ucd_SPH_TRNS, rj_fld1)
      call set_ctl_data_4_pick_zm
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (sph1%sph_params, sph1%sph_rtp, sph1%sph_rtm, sph1%sph_rlm, sph1%sph_rj,            &
     &    comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1, bc_rtp_grp1,       &
     &    radial_rtp_grp1, theta_rtp_grp1, zonal_rtp_grp,               &
     &    radial_rj_grp1, sphere_rj_grp1,                               &
     &    femmesh_STR%mesh, femmesh_STR%group, elemesh_STR)
!
!    Initialize FEM grid
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans(ele_4_nod_SPH_TRANS,               &
     &    jac_STR_l, jac_STR_q, ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
!    Initialization for spherical tranform
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_sph_trans'
      call SPH_initialize_sph_trans
!
!    Set field IOP array by spectr fields
      if (iflag_debug.gt.0) write(*,*) 'SPH_to_FEM_bridge_sph_trans'
      call SPH_to_FEM_bridge_sph_trans(sph1%sph_rj, sph_trns_IO)
!
!  -------------------------------
!
      call init_visualize(femmesh_STR%mesh, femmesh_STR%group,          &
     &    elemesh_STR, field_STR)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_control_params_2nd_files
      use m_spheric_parameter
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
        if (iflag_debug.gt.0) write(*,*) 'zonal_rms_all_rtp_field'
        call overwrite_nodal_xyz_2_sph                                  &
     &    (femmesh_STR%mesh%node, field_STR)
        call zonal_rms_all_rtp_field                                    &
     &     (sph1%sph_rtp, femmesh_STR%mesh%node, field_STR)
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
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_zonal_rms_sph
