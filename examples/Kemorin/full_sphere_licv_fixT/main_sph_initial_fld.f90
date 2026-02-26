!>@file   main_sph_initial_fld.f90
!!@brief  program sph_make_initial
!!
!!@author H. Matsui
!!@date Programmed by H. Okuda in 2000
!!@n    Modified by H. Matsui in May, 2003 (ver 2.0)
!!@n    Connect to vizs  by H. Matsui in July 2006 (ver 2.0)
!
!>@brief  Main program to generate initial field
!!@n       Define initial field at const_sph_initial_spectr.f90
!
     program sph_make_initial
!
      use m_precision
      use calypso_mpi
!
      use t_spherical_MHD
!
      implicit none
!
!>      File name for control file
      character(len=kchara), parameter :: MHD_ctl_name =  'control_MHD'
!
!
      call calypso_MPI_init
      call initialize_const_sph_initial(MHD_ctl_name)
      call calypso_MPI_finalize
!
      stop
!
! ----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine initialize_const_sph_initial(control_file_name)
!
      use t_ctl_data_MHD
      use input_control_sph_MHD
!
      character(len=kchara), intent(in) :: control_file_name
!
!>      Control struture for MHD simulation
      type(spherical_MHD), save :: SMHDs
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save :: MHD_ctl1
!
!
!   Load parameter file
!
      if(iflag_debug.eq.1) write(*,*) 'input_control_4_SPH_MHD_nosnap'
      call input_control_4_SPH_MHD_nosnap(control_file_name,            &
     &    SMHDs%MHD_files, MHD_ctl1, SMHDs%MHD_step, SMHDs%SPH_model,   &
     &    SMHDs%SPH_WK, SMHDs%SPH_MHD)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_const_initial_field'
      call SPH_const_initial_field(SMHDs%MHD_files, SMHDs%MHD_step,     &
     &    SMHDs%SPH_model, SMHDs%SPH_MHD, SMHDs%SPH_WK)
!
      end subroutine initialize_const_sph_initial
!
! ----------------------------------------------------------------------
!
      subroutine SPH_const_initial_field(MHD_files, MHD_step,           &
     &                                   SPH_model, SPH_MHD, SPH_WK)
!
      use init_radial_infos_sph_mhd
      use init_sph_radius_variations
      use radial_reference_field_IO
      use check_dependency_for_MHD
      use input_control_sph_MHD
      use schmidt_poly_on_rtm_grid
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
!   Allocate spectr field data
!
      call set_sph_MHD_sprctr_data(SPH_model%MHD_prop, SPH_MHD)
!
      call alloc_schmidt_normalize(SPH_MHD%sph%sph_rlm%nidx_rlm(2),     &
     &    SPH_MHD%sph%sph_rj%nidx_rj(2), SPH_WK%trans_p%leg)
      call copy_sph_normalization_2_rlm(SPH_MHD%sph%sph_rlm,            &
     &    SPH_WK%trans_p%leg%g_sph_rlm)
      call copy_sph_normalization_2_rj(SPH_MHD%sph%sph_rj,              &
     &    SPH_WK%trans_p%leg%g_sph_rj)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(SPH_MHD%ipol, SPH_MHD%sph,          &
     &    SPH_WK%r_2nd, SPH_WK%r_n2e_3rd, SPH_WK%r_e2n_1st,             &
     &    SPH_model%omega_sph, SPH_model%MHD_prop)
!
      call init_radial_reference_data(SPH_MHD%sph%sph_rj, SPH_MHD%ipol, &
     &    SPH_model%MHD_prop, SPH_model%refs)
      call init_radius_variations_sph_mhd(SPH_MHD%sph, SPH_WK%r_2nd,    &
     &    SPH_model%MHD_prop, SPH_model%refs)
!
      if (iflag_debug.gt.0) write(*,*) 'init_bc_infos_sph_mhd_evo'
      call init_bc_infos_sph_mhd_evo(SPH_model%bc_IO, SPH_MHD%groups,   &
     &    SPH_model%MHD_BC, SPH_MHD%ipol, SPH_MHD%sph, SPH_WK%r_2nd,    &
     &    SPH_model%MHD_prop, SPH_model%refs%ref_field,                 &
     &    SPH_model%sph_MHD_bc)
!
      call init_reference_fields                                        &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_WK%r_2nd, SPH_model%refs,      &
     &    SPH_MHD%fld, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)

! ---------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_spectrum'
      call sph_initial_spectrum(MHD_files%fst_file_IO, SPH_model%refs,  &
     &    SPH_model%sph_MHD_bc, SPH_MHD, MHD_step)
!
      end subroutine SPH_const_initial_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sph_initial_spectrum(fst_file_IO, refs, sph_MHD_bc,    &
     &                                SPH_MHD, MHD_step)
!
      use t_phys_address
      use t_field_data_IO
      use m_initial_field_control
      use m_t_step_parameter
      use sph_mhd_rst_IO_control
      use set_sph_restart_IO
!
      type(radial_reference_field), intent(in) :: refs
      type(field_IO_params), intent(in) :: fst_file_IO
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(MHD_step_param), intent(inout) :: MHD_step
!
!>      Structure of restart IO data
      type(field_IO), save :: sph_fst_IO
!
!
!  Set initial velocity if velocity is exist
      call set_initial_velocity(SPH_MHD%ipol, SPH_MHD%fld)
!  Set initial temperature if temperature is exist
      call set_initial_temp(sph_MHD_bc%sph_bc_T, sph_MHD_bc%bcs_T,      &
     &    refs, SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
!  Copy initial field to restart IO data
      call set_sph_restart_num_to_IO(SPH_MHD%fld, sph_fst_IO)
!
      call output_sph_restart_control                                   &
     &   (MHD_step%time_d%i_time_step, fst_file_IO,                     &
     &    MHD_step%time_d, SPH_MHD%fld, MHD_step%rst_step, sph_fst_IO)
!
      end subroutine sph_initial_spectrum
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_velocity(ipol, rj_fld)
!
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol%base%i_velo .le. izero) return
!$omp parallel workshare
      rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_velo  ) = zero
      rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_velo+1) = zero
      rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_velo+2) = zero
!$omp end parallel workshare
!
      end subroutine set_initial_velocity
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temp(sph_bc_T, bcs_T, refs, sph,           &
     &                            ipol, rj_fld)
!
      use t_radial_reference_field
      use sel_init_reftemp_sph_shell
      use set_initial_sph_scalars
      use spherical_indices_picker
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      type(radial_reference_field), intent(in) :: refs
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      integer ( kind = kint) :: inod, jj, k
      real(kind = kreal) :: source_IC = 3.0d0
      real(kind = kreal) :: source_OC = 3.0d0
!
!
!      if(sph%sph_rj%idx_rj_degree_zero .gt. 0) then
!        write(*,*) 'radial_id radius temperature grad_T source'
!        do k = 1, refs%ref_field%n_point
!          write(*,*) k-1, refs%ref_field%d_fld(k,refs%iref_radius),  &
!     &                refs%ref_field%d_fld(k,refs%iref_base%i_temp),  &
!     &         refs%ref_field%d_fld(k,refs%iref_grad%i_grad_temp),  &
!     &         refs%ref_field%d_fld(k,refs%iref_base%i_heat_source)
!        end do
!      end if
!
      if(ipol%base%i_temp .gt. izero) then
!$omp parallel workshare
        rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_temp       ) = zero
!$omp end parallel workshare
      end if
      if(ipol%base%i_heat_source .gt. izero) then
!$omp parallel workshare
        rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_heat_source) = zero
!$omp end parallel workshare
      end if
!
      call s_sel_init_reftemp_sph_shell                                 &
     &   (sph, sph_bc_T, bcs_T, one, rj_fld%n_point,                    &
     &    rj_fld%d_fld(1,ipol%base%i_temp),                             &
     &    rj_fld%d_fld(1,ipol%base%i_heat_source),                      &
     &    source_IC, source_OC)
      call initital_sph_noise_temp(sph, sph_bc_T, rj_fld%n_point,       &
     &                             rj_fld%d_fld(1,ipol%base%i_temp))
!
!
!
      jj = find_local_sph_mode_address(sph, 0, 0)
      if(jj .eq. 0) return
      do k = 1, sph%sph_rj%nidx_rj(1)
        inod = local_sph_data_address(sph, k, jj)
        write(*,*) k, refs%ref_field%d_fld(k+1,refs%iref_radius),       &
     &             refs%ref_field%d_fld(k+1,refs%iref_base%i_temp),     &
     &             rj_fld%d_fld(inod,ipol%base%i_temp)
      end do
!
      do k = 1, sph%sph_rj%nidx_rj(1)
        inod = local_sph_data_address(sph, k, jj)
        write(*,*) k, refs%ref_field%d_fld(k+1,refs%iref_radius),       &
     &         refs%ref_field%d_fld(k+1,refs%iref_base%i_heat_source),  &
     &         rj_fld%d_fld(inod,ipol%base%i_heat_source)
      end do
!
      end subroutine set_initial_temp
!
!-----------------------------------------------------------------------
!
      end program sph_make_initial
