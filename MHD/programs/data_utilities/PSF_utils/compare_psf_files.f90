!>@file   compare_psf_files.f90
!!@brief  module compare_psf_files
!!
!!@date  Programmed by H.Matsui in Jan., 2021
!
!>@brief control data for cross sections
!
      program compare_psf_files
!
      use m_precision
      use t_ctl_data_psf_compare
!
      use t_file_IO_parameter
      use t_control_params_4_psf
!
      type(psf_compare_control), save :: psf_cmp_ctl1
!
      character(len=kchara), parameter :: default_psf_prefix = 'psf'
      type(field_IO_params), save:: psf1_file_param
      type(field_IO_params), save :: psf2_file_param
!
      call read_control_file_psf_compare(0, psf_cmp_ctl1)
!
      call set_merged_psf_file_ctl(default_psf_prefix,                  &
     &    psf_cmp_ctl1%first_psf%psf_file_prefix_ctl,                   &
     &    psf_cmp_ctl1%first_psf%psf_file_format_ctl,                   &
     &    psf1_file_param)
      call set_merged_psf_file_ctl(default_psf_prefix,                  &
     &    psf_cmp_ctl1%second_psf%psf_file_prefix_ctl,                  &
     &    psf_cmp_ctl1%second_psf%psf_file_format_ctl,                  &
     &    psf2_file_param)
!
      write(*,*) 'psf1_file_param%file_prefix: ',                       &
     &          trim(psf1_file_param%file_prefix)
      write(*,*) 'psf1_file_param%iflag_format: ',                      &
     &          psf1_file_param%iflag_format,                           &
     &    trim(psf_cmp_ctl1%first_psf%psf_file_format_ctl%charavalue)
      write(*,*) 'psf2_file_param%file_prefix: ',                       &
     &          trim(psf2_file_param%file_prefix)
      write(*,*) 'psf2_file_param%iflag_format: ',                      &
     &          psf2_file_param%iflag_format,                           &
     &    trim(psf_cmp_ctl1%first_psf%psf_file_format_ctl%charavalue)
      write(*,*) 'i_step_surface_ctl: ',                                &
     &          psf_cmp_ctl1%i_step_surface_ctl
!
      call reset_ctl_data_psf_compare(psf_cmp_ctl1)
!
      end program compare_psf_files
