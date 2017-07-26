!>@file   sph_SGS_MHD_rst_IO_control.f90
!!@brief  module sph_SGS_MHD_rst_IO_control
!!
!!@author H. Matsui
!!@date Programmed in 2009
!!@n    Modified in June, 2015
!
!>@brief  I/O routines for restart data
!!
!!@verbatim
!!      subroutine output_sph_SGS_MHD_rst_control                       &
!!     &         (MHD_files, time_d, rj_fld, rst_step,                  &
!!     &          i_step_sgs_coefs, SGS_param, dynamic_SPH)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(time_data), intent(in) :: time_d
!!        type(phys_data), intent(in) :: rj_fld
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!!
!!      subroutine read_alloc_sph_rst_SGS_snap(i_step, rj_file_param,   &
!!     &          MHD_files, sph_rj, ipol, rj_fld, rst_step, time_d,    &
!!     &          i_step_sgs_coefs, SGS_param, dynamic_SPH)
!!        type(field_IO_params), intent(in) :: rj_file_param
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(time_data), intent(inout) :: time_d
!!        type(SGS_model_control_params), intent(inout) :: SGS_param
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!
!!      subroutine sst_initial_Csim_control                             &
!!     &         (MHD_files, MHD_step, SGS_par, dynamic_SPH)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!@endverbatim
!!
!!@n @param i_step  time step
!
      module sph_SGS_MHD_rst_IO_control
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_file_format_switch
!
      use t_time_data
      use t_IO_step_parameter
      use t_phys_address
      use t_phys_data
      use t_MHD_file_parameter
      use t_file_IO_parameter
      use t_time_data
!
      use sph_mhd_rst_IO_control
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine output_sph_SGS_MHD_rst_control                         &
     &         (MHD_files, time_d, rj_fld, rst_step,                    &
     &          i_step_sgs_coefs, SGS_param, dynamic_SPH)
!
      use t_sph_filtering
      use set_sph_restart_IO
      use sgs_ini_model_coefs_IO
!
      integer(kind=kint), intent(in) :: i_step_sgs_coefs
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(time_data), intent(in) :: time_d
      type(phys_data), intent(in) :: rj_fld
      type(IO_step_param), intent(in) :: rst_step
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!
!
      call output_sph_restart_control                                   &
     &   (MHD_files%fst_file_IO, time_d, rj_fld, rst_step)
!
      if(SGS_param%iflag_dynamic .gt. 0) then
        call write_SPH_Csim_file                                        &
     &     (i_step_sgs_coefs, MHD_files%Csim_file_IO,                   &
     &     rst_step, time_d, dynamic_SPH)
      end if
!
      end subroutine output_sph_SGS_MHD_rst_control
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_rst_SGS_snap(i_step, rj_file_param,     &
     &          MHD_files, sph_rj, ipol, rj_fld, rst_step, time_d,      &
     &          i_step_sgs_coefs, SGS_param, dynamic_SPH)
!
      use t_spheric_rj_data
      use t_SGS_control_parameter
      use t_sph_filtering
      use sgs_ini_model_coefs_IO
      use set_sph_restart_IO
      use r_interpolate_sph_data
!
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: rj_file_param
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(IO_step_param), intent(inout) :: rst_step
      type(time_data), intent(inout) :: time_d
      type(SGS_model_control_params), intent(inout) :: SGS_param
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      integer(kind = kint), intent(inout) :: i_step_sgs_coefs
!
!
      call read_alloc_sph_rst_4_snap                                    &
     &   (i_step, rj_file_param, MHD_files%fst_file_IO, sph_rj,         &
     &    ipol, rj_fld, rst_step, time_d)
!
      if(SGS_param%iflag_dynamic .gt. 0) then
        call read_alloc_SPH_Csim_file                                   &
     &     (MHD_files%Csim_file_IO, time_d, rst_step,                   &
     &      i_step_sgs_coefs, SGS_param, dynamic_SPH%wk_sgs)
        if(iflag_debug .gt. 0) write(*,*) 'iflag_rst_sgs_coef_code',    &
     &                        SGS_param%iflag_rst_sgs_coef_code
        if(SGS_param%iflag_rst_sgs_coef_code .eq. 0) then
          SGS_param%stab_weight = one
        end if
      end if
!
      end subroutine read_alloc_sph_rst_SGS_snap
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sst_initial_Csim_control                               &
     &         (MHD_files, MHD_step, SGS_par, dynamic_SPH)
!
      use m_machine_parameter
      use m_initial_field_control
!
      use t_MHD_step_parameter
      use t_spheric_parameter
      use t_SGS_control_parameter
      use t_sph_filtering
!
      use set_sph_restart_IO
      use sph_mhd_rst_IO_control
      use sgs_ini_model_coefs_IO
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(MHD_step_param), intent(inout) :: MHD_step
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. 0) return
      if (iflag_restart .eq. i_rst_by_file) then
        call read_alloc_SPH_Csim_file(MHD_files%Csim_file_IO,           &
     &      MHD_step%init_d, MHD_step%rst_step,                         &
     &      SGS_par%i_step_sgs_coefs, SGS_par%model_p,                  &
     &      dynamic_SPH%wk_sgs)
      else
        SGS_par%model_p%iflag_rst_sgs_coef_code = 0
        call write_SPH_Csim_file                                        &
     &     (SGS_par%i_step_sgs_coefs, MHD_files%Csim_file_IO,           &
     &      MHD_step%rst_step, MHD_step%init_d, dynamic_SPH)
      end if
      if(iflag_debug .gt. 0) write(*,*) 'iflag_rst_sgs_coef_code',      &
     &                        SGS_par%model_p%iflag_rst_sgs_coef_code
!
      end subroutine sst_initial_Csim_control
!
!-----------------------------------------------------------------------
!
      end module sph_SGS_MHD_rst_IO_control
