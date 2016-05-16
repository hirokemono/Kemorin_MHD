!>@file   sph_mhd_rst_IO_control.f90
!!@brief  module sph_mhd_rst_IO_control
!!
!!@author H. Matsui
!!@date Programmed in 2009
!!@n    Modified in June, 2015
!
!>@brief  I/O routines for restart data
!!
!!@verbatim
!!      subroutine set_ctl_restart_4_sph_mhd
!!      subroutine set_rst_file_by_orignal_mesh
!!
!!      subroutine init_output_sph_restart_file(rj_fld)
!!      subroutine output_sph_restart_control(rj_fld)
!!      subroutine output_sph_rst_by_elaps(rj_fld)
!!        type(phys_data), intent(in) :: rj_fld
!!
!!      subroutine read_alloc_sph_restart_data
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine init_radial_sph_interpolation
!!      subroutine read_alloc_sph_rst_4_snap(i_step, sph_rj, rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine output_spectr_4_snap(i_step, sph_rj, rj_fld)
!!        type(phys_data), intent(in) :: rj_fld
!!      subroutine read_alloc_sph_rst_2_modify(i_step, sph_rj, rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@n @param i_step  time step
!
      module sph_mhd_rst_IO_control
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_control_parameter
      use m_t_step_parameter
      use m_file_format_switch
!
      use t_phys_data
      use t_field_data_IO
!
      use field_IO_select
!
      implicit  none
!
      type(field_IO), save, private :: sph_fst_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_restart_4_sph_mhd
!
      use set_control_platform_data
!
!
      call set_control_restart_file_def(sph_fst_IO)
!
      end subroutine set_ctl_restart_4_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine set_rst_file_by_orignal_mesh
!
      use m_control_params_2nd_files
!
      if( (iflag_org_sph_rj_head*iflag_org_rst) .eq. 0) return
      sph_fst_IO%file_prefix = org_rst_header
!
      end subroutine set_rst_file_by_orignal_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_output_sph_restart_file(rj_fld)
!
      use m_initial_field_control
      use set_sph_restart_IO
!
      type(phys_data), intent(in) :: rj_fld
!
!
      i_step_MHD = i_step_init
      time   =     time_init
!
      call set_sph_restart_num_to_IO(rj_fld, sph_fst_IO)
!
      end subroutine init_output_sph_restart_file
!
! -----------------------------------------------------------------------
!
      subroutine output_sph_restart_control(rj_fld)
!
      use set_sph_restart_IO
!
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint) :: istep_fld
!
!
      if ( mod(istep_max_dt,i_step_output_rst) .ne. 0 ) return
!
      istep_fld = istep_max_dt/i_step_output_rst
!
      call set_sph_restart_data_to_IO(rj_fld, sph_fst_IO)
      call sel_write_step_SPH_field_file                                &
     &   (nprocs, my_rank, istep_fld, sph_fst_IO)
!
      end subroutine output_sph_restart_control
!
! -----------------------------------------------------------------------
!
      subroutine output_sph_rst_by_elaps(rj_fld)
!
      use set_sph_restart_IO
!
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint), parameter :: negaone = -1
!
!
      call set_sph_restart_data_to_IO(rj_fld, sph_fst_IO)
      call sel_write_step_SPH_field_file                                &
     &   (nprocs, my_rank, negaone, sph_fst_IO)
!
      end subroutine output_sph_rst_by_elaps
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_restart_data(rj_fld)
!
      use set_sph_restart_IO
!
      integer(kind = kint) :: istep_fld
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (i_step_init .eq. -1) then
        call sel_read_alloc_step_SPH_file                               &
     &     (nprocs, my_rank, i_step_init, sph_fst_IO)
      else
        istep_fld = i_step_init / i_step_output_rst
        call sel_read_alloc_step_SPH_file                               &
     &     (nprocs, my_rank, istep_fld, sph_fst_IO)
      end if
!
      call set_sph_restart_from_IO(sph_fst_IO, rj_fld)
!
      call dealloc_phys_data_IO(sph_fst_IO)
      call dealloc_phys_name_IO(sph_fst_IO)
!
      end subroutine read_alloc_sph_restart_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_radial_sph_interpolation(sph_params, sph_rj)
!
      use m_control_params_2nd_files
      use m_node_id_spherical_IO
      use t_spheric_parameter
      use r_interpolate_sph_data
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rj_grid), intent(inout) ::  sph_rj
!
!
      if(iflag_org_sph_rj_head .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*) 'input_old_rj_sph_trans'
        call input_old_rj_sph_trans                                     &
     &     (my_rank, sph_params%l_truncation, sph_rj)
      end if
!
      call copy_cmb_icb_radial_point                                    &
     &   (sph_params%nlayer_ICB, sph_params%nlayer_CMB)
!
      end subroutine init_radial_sph_interpolation
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_rst_4_snap(i_step, sph_rj, rj_fld)
!
      use m_control_params_2nd_files
      use m_node_id_spherical_IO
      use t_spheric_rj_data
      use set_sph_restart_IO
      use r_interpolate_sph_data
!
      integer(kind = kint), intent(in) :: i_step
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: istep_fld
!
!
      istep_fld = i_step / i_step_output_rst
      call sel_read_alloc_step_SPH_file                                 &
     &   (nprocs, my_rank, istep_fld, sph_fst_IO)
!
      if(iflag_org_sph_rj_head .eq. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_sph_restart_from_IO'
        call set_sph_restart_from_IO(sph_fst_IO, rj_fld)
        time = time_init
      else
        if (iflag_debug.gt.0)                                           &
     &            write(*,*) 'r_interpolate_sph_rst_from_IO'
        call r_interpolate_sph_rst_from_IO(sph_fst_IO, sph_rj, rj_fld)
      end if
!
      call dealloc_phys_data_IO(sph_fst_IO)
      call dealloc_phys_name_IO(sph_fst_IO)
!
      end subroutine read_alloc_sph_rst_4_snap
!
! -----------------------------------------------------------------------
!
      subroutine output_spectr_4_snap(i_step, sph_rj, rj_fld)
!
      use t_spheric_rj_data
      use m_t_int_parameter
      use m_control_params_sph_data
      use copy_rj_phys_data_4_IO
      use copy_time_steps_4_restart
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: i_step
!
      integer(kind = kint) :: istep_fld
!
!
      if( (iflag_sph_spec_output*i_step_output_ucd) .eq. 0) return
      if(mod(i_step,i_step_output_ucd) .eq. 0) return
!
      istep_fld = i_step / i_step_output_ucd
      call copy_time_steps_to_restart
      call copy_rj_viz_phys_name_to_IO                                  &
     &   (sph_rj%nnod_rj, rj_fld, sph_fst_IO)
      call alloc_phys_data_IO(sph_fst_IO)
      call copy_rj_viz_phys_data_to_IO                                  &
     &   (sph_rj%nnod_rj, rj_fld, sph_fst_IO)
!
      call set_spectr_prefix_fmt_2_fld_IO(sph_fst_IO)
      call sel_write_step_SPH_field_file                                &
     &   (nprocs, my_rank, istep_fld, sph_fst_IO)
!
      call dealloc_phys_data_IO(sph_fst_IO)
      call dealloc_phys_name_IO(sph_fst_IO)
!
      end subroutine output_spectr_4_snap
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_rst_2_modify(i_step, sph_rj, rj_fld)
!
      use m_control_params_2nd_files
      use t_spheric_rj_data
!
      integer(kind = kint), intent(in) :: i_step
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ifmt_rst_file_tmp
      character(len=kchara) :: restart_tmp_prefix
!
!
      ifmt_rst_file_tmp =  sph_fst_IO%iflag_file_fmt
      restart_tmp_prefix = sph_fst_IO%file_prefix
      sph_fst_IO%file_prefix = org_rst_header
      call set_field_file_fmt_prefix                                    &
     &   (ifmt_rst_file_tmp, org_rst_header, sph_fst_IO)
      call read_alloc_sph_rst_4_snap(i_step, sph_rj, rj_fld)
!
      call set_field_file_fmt_prefix                                    &
     &   (ifmt_rst_file_tmp, restart_tmp_prefix, sph_fst_IO)
!
      end subroutine read_alloc_sph_rst_2_modify
!
! -----------------------------------------------------------------------
!
      end module sph_mhd_rst_IO_control
