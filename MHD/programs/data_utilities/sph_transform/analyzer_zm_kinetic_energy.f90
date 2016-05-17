!analyzer_zm_kinetic_energy.f90
!      module analyzer_zm_kinetic_energy
!..................................................
!
!      modified by H. Matsui on Jan., 2008
!
!      subroutine init_zm_kinetic_energy
!      subroutine analyze_zm_kinetic_energy
!
      module analyzer_zm_kinetic_energy
!
      use m_precision
      use calypso_mpi
      use m_SPH_transforms
      use m_work_time
!
      use t_field_data_IO
!
      use SPH_analyzer_back_trans
      use FEM_analyzer_back_trans
      use SPH_analyzer_zm_energies
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
      subroutine init_zm_kinetic_energy
!
      use m_ctl_data_4_sph_trans
      use m_ctl_params_sph_trans
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_group_data_sph_specr
      use m_sph_spectr_data
      use parallel_load_data_4_sph
!
!
      num_elapsed = 30
      call allocate_elapsed_times
!
!   ----  read controls
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_data_sph_trans'
      call read_control_data_sph_trans
      call set_ctl_data_4_zm_energies
!
      if (iflag_debug.gt.0) write(*,*) 's_set_ctl_data_4_sph_trans'
      call s_set_ctl_data_4_sph_trans(ucd_SPH_TRNS, rj_fld1)
!
!  ------    set spectr grids
      if (iflag_debug.gt.0) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (sph_param1, sph_rtp1, sph_rtm1, sph_rlm1, sph1%sph_rj,            &
     &    comm_rtp1, comm_rtm1, comm_rlm1, comm_rj1, bc_rtp_grp1,       &
     &    radial_rtp_grp1, theta_rtp_grp1, zonal_rtp_grp,               &
     &    radial_rj_grp1, sphere_rj_grp1,                               &
     &    femmesh_STR%mesh, femmesh_STR%group, elemesh_STR)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'FEM_initialize_back_trans'
      call FEM_initialize_back_trans(ele_4_nod_SPH_TRANS,               &
     &    jac_STR_l, jac_STR_q, ucd_SPH_TRNS, m_ucd_SPH_TRNS)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'SPH_initialize_back_trans'
      call SPH_initialize_back_trans(sph_trns_IO)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_visualize'
      call init_visualize(femmesh_STR%mesh, femmesh_STR%group,          &
     &    elemesh_STR, field_STR)
!
      end subroutine init_zm_kinetic_energy
!
! ----------------------------------------------------------------------
!
      subroutine analyze_zm_kinetic_energy
!
      use m_t_step_parameter
!
      integer(kind=kint ) :: visval, i_step
      integer(kind=kint ) :: istep_psf, istep_iso
      integer(kind=kint ) :: istep_pvr, istep_fline
!
!
      do i_step = i_step_init, i_step_number
        if (iflag_debug.gt.0) write(*,*) 'step ', i_step, 'start...'
!
        call SPH_analyze_zm_energies(i_step, visval, sph_trns_IO)
!
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
      end subroutine analyze_zm_kinetic_energy
!
! ----------------------------------------------------------------------
!
      end module analyzer_zm_kinetic_energy
