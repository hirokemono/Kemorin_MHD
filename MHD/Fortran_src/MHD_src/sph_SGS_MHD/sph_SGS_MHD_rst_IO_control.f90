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
!!      subroutine output_sph_SGS_MHD_rst_control(i_step, MHD_files,    &
!!     &          time_d, rst_step, SPH_SGS, SPH_MHD, sph_fst_IO)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(time_data), intent(in) :: time_d
!!        type(phys_data), intent(in) :: rj_fld
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!
!!      subroutine read_alloc_sph_rst_SGS_snap(i_step, rj_file_param,   &
!!     &          MHD_files, rst_step, time_d, SPH_MHD, SPH_SGS)
!!        type(field_IO_params), intent(in) :: rj_file_param
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(IO_step_param), intent(inout) :: rst_step
!!        type(time_data), intent(inout) :: time_d
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!
!!      subroutine set_initial_Csim_control                             &
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
      use t_SPH_SGS_structure
      use t_SPH_mesh_field_data
      use t_phys_address
      use t_phys_data
      use t_MHD_file_parameter
      use t_file_IO_parameter
      use t_field_data_IO
      use t_time_data
!
      use sph_mhd_rst_IO_control
!
      implicit  none
!
      private :: write_sph_rst_Csim, read_alloc_sph_rst_Csim_snap
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine output_sph_SGS_MHD_rst_control(i_step, MHD_files,      &
     &          time_d, rst_step, SPH_SGS, SPH_MHD, sph_fst_IO)
!
      use t_SPH_SGS_structure
      use t_sph_filtering
      use SPH_SGS_ini_model_coefs_IO
!
      integer(kind=kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: rst_step
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!
      type(field_IO), intent(inout) :: sph_fst_IO
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!
!
      call output_sph_restart_control(i_step, MHD_files%fst_file_IO,    &
     &    time_d, SPH_MHD%fld, rst_step, sph_fst_IO)
      call write_sph_rst_Csim(i_step, MHD_files,                        &
     &    rst_step, time_d, SPH_SGS%SGS_par, SPH_SGS%dynamic)
!
      end subroutine output_sph_SGS_MHD_rst_control
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_rst_SGS_snap(i_step, rj_file_param,     &
     &          MHD_files, rst_step, time_d, SPH_MHD, SPH_SGS)
!
      use t_SGS_control_parameter
      use t_sph_filtering
      use t_SPH_SGS_structure
      use SPH_SGS_ini_model_coefs_IO
      use r_interpolate_sph_data
!
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: rj_file_param
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(IO_step_param), intent(inout) :: rst_step
      type(time_data), intent(inout) :: time_d
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!
!
      call read_alloc_sph_rst_4_snap                                    &
     &   (i_step, rj_file_param, MHD_files%fst_file_IO, rst_step,       &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld, time_d)
!
      call read_alloc_sph_rst_Csim_snap(i_step, MHD_files,              &
     &    rst_step, time_d, SPH_SGS%SGS_par, SPH_SGS%dynamic)
!
      end subroutine read_alloc_sph_rst_SGS_snap
!
! -----------------------------------------------------------------------
!
      subroutine set_initial_Csim_control                               &
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
      use sph_mhd_rst_IO_control
      use SPH_SGS_ini_model_coefs_IO
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(in) :: MHD_step
!
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. 0) return
      if (iflag_restart .eq. i_rst_by_file) then
        call read_alloc_SPH_Csim_file(MHD_step%init_d%i_time_step,      &
     &      MHD_files%Csim_file_IO, MHD_step%init_d, MHD_step%rst_step, &
     &      SGS_par%i_step_sgs_coefs, SGS_par%model_p, dynamic_SPH)
      else
        SGS_par%model_p%iflag_rst_sgs_coef_code = 0
        call write_SPH_Csim_file(MHD_step%init_d%i_time_step,           &
     &      SGS_par%i_step_sgs_coefs, MHD_files%Csim_file_IO,           &
     &      MHD_step%rst_step, MHD_step%init_d, dynamic_SPH)
      end if
      if(iflag_debug .gt. 0) write(*,*) 'iflag_rst_sgs_coef_code',      &
     &                        SGS_par%model_p%iflag_rst_sgs_coef_code
!
      call init_SPH_Csim_file(dynamic_SPH)
!
      end subroutine set_initial_Csim_control
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_rst_Csim(i_step, MHD_files,                  &
     &          rst_step, time_d, SGS_par, dynamic_SPH)
!
      use t_sph_filtering
      use SPH_SGS_ini_model_coefs_IO
!
      integer(kind=kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: rst_step
      type(SGS_paremeters), intent(in) :: SGS_par
!
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      if(SGS_par%model_p%iflag_dynamic .gt. 0) then
        call write_SPH_Csim_file                                        &
     &     (i_step, SGS_par%i_step_sgs_coefs, MHD_files%Csim_file_IO,   &
     &      rst_step, time_d, dynamic_SPH)
      end if
!
      end subroutine write_sph_rst_Csim
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_rst_Csim_snap(i_step, MHD_files,        &
     &          rst_step, time_d, SGS_par, dynamic_SPH)
!
      use t_SGS_control_parameter
      use t_sph_filtering
      use SPH_SGS_ini_model_coefs_IO
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(IO_step_param), intent(inout) :: rst_step
      type(time_data), intent(inout) :: time_d
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      if(SGS_par%model_p%iflag_dynamic .gt. 0) then
        call read_alloc_SPH_Csim_file                                   &
     &     (i_step, MHD_files%Csim_file_IO, time_d, rst_step,           &
     &      SGS_par%i_step_sgs_coefs, SGS_par%model_p, dynamic_SPH)
        if(iflag_debug .gt. 0) write(*,*) 'iflag_rst_sgs_coef_code',    &
     &                        SGS_par%model_p%iflag_rst_sgs_coef_code
        if(SGS_par%model_p%iflag_rst_sgs_coef_code .eq. 0) then
          SGS_par%model_p%stab_weight = one
        end if
      end if
!
      end subroutine read_alloc_sph_rst_Csim_snap
!
! -----------------------------------------------------------------------
!
      end module sph_SGS_MHD_rst_IO_control
