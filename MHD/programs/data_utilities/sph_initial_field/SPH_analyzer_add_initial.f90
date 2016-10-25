!!@brief  module SPH_analyzer_add_initial
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Jan.., 2014
!
!>@brief  Main spectrum method loop to generate initial field
!!@n      Initial field definision is in  const_sph_initial_spectr.f90
!!
!!@verbatim
!!      subroutine initialize_add_sph_initial
!!@endverbatim
!
      module SPH_analyzer_add_initial
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_work_time
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
!
      implicit none
!
      private :: SPH_add_initial_field
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_add_sph_initial
!
      use m_spheric_parameter
      use m_ctl_data_sph_MHD_noviz
      use m_sph_spectr_data
      use set_control_sph_mhd
      use m_rms_4_sph_spectr
      use init_sph_MHD_elapsed_label
      use input_control_sph_MHD
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      total_start = MPI_WTIME()
      call set_sph_MHD_elapsed_label
!
!   Load parameter file
!
      call start_eleps_time(1)
      call start_eleps_time(4)
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sph_MHD_noviz'
      call read_control_4_sph_MHD_noviz
!
      if (iflag_debug.eq.1) write(*,*) 'input_control_4_SPH_make_init'
      call input_control_4_SPH_make_init                                &
     &   (sph1, comms_sph1, sph_grps1, rj_fld1, pwr1)
      call end_eleps_time(4)
!
!    precondition elaps start
!
      call start_eleps_time(2)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_add_initial_field'
      call SPH_add_initial_field
!
      call end_eleps_time(2)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_add_sph_initial
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_add_initial_field
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_physical_property
      use m_boundary_params_sph_MHD
!
      use set_control_sph_mhd
      use set_sph_phys_address
      use const_sph_initial_spectr
      use set_reference_sph_mhd
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use sph_transforms_4_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use set_initial_sph_dynamo
      use sph_mhd_rst_IO_control
!
!
!   Allocate spectr field data
!
      call set_sph_sprctr_data_address                                  &
     &   (sph1%sph_rj, ipol, idpdr, itor, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd'
      call init_r_infos_sph_mhd(sph_grps1, ipol, sph1,                  &
     &    omega_sph1, ref_temp1, rj_fld1)
!
! ---------------------------------
!
     if(iflag_debug.gt.0) write(*,*)' read_alloc_sph_restart_data'
     call read_alloc_sph_restart_data(rj_fld1)
     istep_max_dt = i_step_init
!
     if(iflag_debug.gt.0) write(*,*)' sph_initial_spectrum'
     call sph_initial_spectrum(ipol, itor, rj_fld1)
!
      end subroutine SPH_add_initial_field
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_add_initial
