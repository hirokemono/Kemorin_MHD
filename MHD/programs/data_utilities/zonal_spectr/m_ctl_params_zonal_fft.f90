!m_ctl_params_zonal_fft.f90
!      module m_ctl_params_zonal_fft
!
      module m_ctl_params_zonal_fft
!
!        programmed by H.Matsui on Oct., 2007
!
      use m_precision
      implicit  none
!
!
!
      integer(kind = kint) :: istep_start, istep_end, istep_int
      integer(kind = kint) :: ifrag_trans_vect
!
      character(len = kchara) :: ene_spec_head = 'zonal_ene_spec'
      character(len = kchara) :: tave_ene_spec_head = 'zonal_ene_spec'
!
!      subroutine s_set_ctl_data_4_zonal_fft
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_ctl_data_4_zonal_fft
!
      use m_machine_parameter
      use m_read_mesh_data
      use m_ucd_data
      use m_field_data_IO
      use m_node_id_spherical_IO
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_ctl_data_4_zonal_fft
      use set_control_platform_data
!
!
      np_smp = num_smp_ctl
!
      call set_control_mesh_def
      call set_control_sph_mesh
!
      call set_control_restart_file_def
      call set_control_ucd_file_def
!
      if(i_spectr_header .gt. 0) then
        phys_file_head = spectr_file_head_ctl
      end if
!
      if(i_z_e_sp_file .gt. 0) then
        ene_spec_head = ene_spec_head_ctl
      end if
!
      if(i_tz_esp_file .gt. 0) then
        tave_ene_spec_head = tave_ene_spec_head_ctl
      end if
!
!
      ifrag_trans_vect = 0
      if(i_spectr_header .gt. 0) then
        phys_file_head = spectr_file_head_ctl
        if (     vector_trans_ctl .eq. 'spherical'                      &
     &    .or.   vector_trans_ctl .eq. 'Spherical'                      &
     &    .or.   vector_trans_ctl .eq. 'SPHERICAL'                      &
     &    .or.   vector_trans_ctl .eq. 'sph'                            &
     &    .or.   vector_trans_ctl .eq. 'SPH'                            &
     &    .or.   vector_trans_ctl .eq. 'rtp'                            &
     &    .or.   vector_trans_ctl .eq. 'RTP') then
          ifrag_trans_vect = 1
        else if (vector_trans_ctl .eq. 'cylindrical'                    &
     &    .or.   vector_trans_ctl .eq. 'Cylindrical'                    &
     &    .or.   vector_trans_ctl .eq. 'CYLINDRICAL'                    &
     &    .or.   vector_trans_ctl .eq. 'cyl'                            &
     &    .or.   vector_trans_ctl .eq. 'CYL'                            &
     &    .or.   vector_trans_ctl .eq. 'spz'                            &
     &    .or.   vector_trans_ctl .eq. 'SPZ') then
          ifrag_trans_vect = 2
        end if
      end if
!
      if (i_i_step_init .gt. 0) then
        istep_start = i_step_init_ctl
      end if
!
      if (i_i_step_number .gt. 0) then
        istep_end =   i_step_number_ctl
      else
        istep_end =   istep_start
      end if
!
      if (i_i_step_ucd .gt. 0) then
        istep_int =   i_step_ucd_ctl
      else
        istep_int =   1
      end if
!
!
      end subroutine s_set_ctl_data_4_zonal_fft
!
! -----------------------------------------------------------------------
!
      end module m_ctl_params_zonal_fft
