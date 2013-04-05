!>@file   set_control_sph_data_MHD.f90
!!@brief  module set_control_sph_data_MHD
!!
!!@author H. Matsui
!!@date Programmed on Sep., 2009
!
!>@brief  Set control parameters for spherical hermonics dynamo from IO
!!
!!@verbatim
!!     subroutine s_set_control_sph_data_MHD
!!     subroutine set_ctl_params_dynamobench
!!     subroutine check_SPH_MHD_dependencies
!!@endverbatim
!
      module set_control_sph_data_MHD
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_sph_data_MHD
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_node_phys_data
      use m_ctl_data_4_platforms
      use m_ctl_data_4_fields
      use m_ctl_data_mhd_forces
      use m_ctl_data_mhd_evo_scheme
      use m_int_4_sph_coriolis_IO
      use m_node_id_spherical_IO
      use m_control_params_sph_MHD
      use m_physical_property
      use m_work_4_sph_trans
      use m_file_format_switch
!
      use m_field_data_IO
!
      use set_control_sph_data
      use set_phys_name_4_sph_trans
      use FFT_selector
      use add_nodal_fields_4_MHD
      use add_sph_MHD_fields_2_ctl
!
!
!   overwrite restart header for magnetic field extension
!
      if( (iflag_org_sph_rj_head*iflag_org_rst_head) .gt. 0) then
        phys_file_head = org_rst_header
      end if
!
!   set physical values
!
      if(i_num_nod_phys.eq.0) then
        call parallel_abort(90, 'Set field for simulation')
      end if
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'original number of field ', num_nod_phys_ctl
!
      if ( num_nod_phys_ctl .ne. 0 ) then
!
!     add terms for potentials
!
        call add_field_name_4_mhd
        call add_field_name_4_sph_mhd
        if (iflag_debug.eq.1) write(*,*)                                &
     &    'num_nod_phys_ctl after modified ', num_nod_phys_ctl
!
!    set nodal data
!
        if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data'
        call s_set_control_sph_data(ierr)
!
        if (iflag_debug.gt.0) write(*,*) 'copy_sph_name_rj_to_rtp'
        call copy_sph_name_rj_to_rtp
      end if
!
!
      if(i_sph_transform_mode .gt. 0) then
        if(     sph_transform_mode_ctl .eq. 'radius_in'                 &
     &    .or.  sph_transform_mode_ctl .eq. 'Radius_in'                 &
     &    .or.  sph_transform_mode_ctl .eq. 'RADIUS_IN') then
          id_lagendre_transfer = iflag_lag_krloop_inner
        else if(sph_transform_mode_ctl .eq. 'radius_out'                &
     &    .or.  sph_transform_mode_ctl .eq. 'Radius_out'                &
     &    .or.  sph_transform_mode_ctl .eq. 'RADIUS_OUT') then
          id_lagendre_transfer = iflag_lag_krloop_outer
        else if(sph_transform_mode_ctl .eq. 'long_loop'                 &
     &    .or.  sph_transform_mode_ctl .eq. 'Long_loop'                 &
     &    .or.  sph_transform_mode_ctl .eq. 'LONG_LOOP') then
          id_lagendre_transfer = iflag_lag_largest_loop
        end if
      end if
!
      if(i_FFT_package .gt. 0) then
        if(     FFT_library_ctl .eq. 'ispack'                           &
     &    .or.  FFT_library_ctl .eq. 'ISPACK') then
          iflag_FFT = iflag_ISPACK
        else if(FFT_library_ctl .eq. 'fftpack'                          &
     &    .or.  FFT_library_ctl .eq. 'FFTPACK') then
          iflag_FFT = iflag_FFTPACK
        else if(FFT_library_ctl .eq. 'fftw'                             &
     &    .or.  FFT_library_ctl .eq. 'FFTW') then
          iflag_FFT = iflag_FFTW
        end if
      end if
!
      if (iflag_4_coriolis .gt. 0) then
        iflag_sph_coriolis_file                                         &
     &         = max(i_sph_coriolis_file,i_coriolis_tri_int_name)
        if(i_sph_coriolis_file .gt. 0) then
          sph_cor_file_name = sph_cor_file_name_ctl
           call choose_file_format(sph_cor_file_fmt_ctl,                &
     &         i_sph_coriolis_fmt, ifmt_cor_int_file)
        end if
        if(i_coriolis_tri_int_name .gt. 0) then
          sph_cor_file_name = coriolis_tri_int_name_ctl
          call choose_file_format(coriolis_file_fmt_ctl,                &
     &        i_coriolis_file_fmt, ifmt_cor_int_file)
        end if
!
        if(angular(1).ne.zero .or. angular(2).ne.zero) then
          iflag_tilted_coriolis = 1
        end if
!
      end if
!
!
      end subroutine s_set_control_sph_data_MHD
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_dynamobench
!
      use m_ctl_data_4_pickup_sph
      use m_field_at_mid_equator
!
!
      if(i_nphi_mid_eq .gt. 0) then
        mphi_mid_eq = nphi_mid_eq_ctl
      else
        mphi_mid_eq = -1
      end if
!
      end subroutine set_ctl_params_dynamobench
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_SPH_MHD_dependencies
!
      use m_sph_spectr_data
      use check_dependency_for_MHD
!
!
      call check_dependencies(num_phys_rj, phys_name_rj)
!
      end subroutine check_SPH_MHD_dependencies
!
! -----------------------------------------------------------------------
!
      end module set_control_sph_data_MHD
