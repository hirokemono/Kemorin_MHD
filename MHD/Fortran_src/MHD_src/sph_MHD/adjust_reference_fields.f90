!>@file   adjust_reference_fields.f90
!!@brief  module adjust_reference_fields
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2015
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine init_reference_fields(sph_params, sph_rj)
!!      subroutine adjust_press_by_average_on_CMB                       &
!!     &         (kr_in, kr_out, sph_rj, rj_fld)
!!      subroutine sync_temp_by_per_temp_sph(reftemp_rj, sph_rj, rj_fld)
!!        d_rj(inod,ipol%i_temp):        T => \Theta = T - T0
!!        d_rj(inod,ipol%i_par_temp):    \Theta = T - T0
!!        d_rj(inod,ipol%i_grad_t):      T => d \Theta / dr
!!        d_rj(inod,ipol%i_grad_part_t): d \Theta / dr
!!      subroutine trans_per_temp_to_temp_sph                           &
!!     &         (reftemp_rj, sph_rj, rj_fld)
!!        d_rj(inod,ipol%i_temp):        \Theta = T - T0 => T
!!        d_rj(inod,ipol%i_par_temp):    \Theta = T - T0
!!        d_rj(inod,ipol%i_grad_t):      d \Theta / dr   => dT / dr
!!        d_rj(inod,ipol%i_grad_part_t): d \Theta / dr
!!
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!@endverbatim
!
      module adjust_reference_fields
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_reference_fields(sph_params, sph_rj)
!
      use m_sph_spectr_data
      use m_boundary_params_sph_MHD
!
      use set_reference_sph_mhd
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
!
!      Set reference temperature and adjust boundary conditions
!
      if(iflag_debug .gt. 0) write(*,*) 'set_ref_temp_sph_mhd'
      call allocate_reft_rj_data(sph_rj)
      call set_ref_temp_sph_mhd(sph_rj%nidx_rj,                         &
     &    sph_params%radius_ICB, sph_params%radius_CMB,                 &
     &    sph_rj%ar_1d_rj, sph_bc_T, reftemp_rj)
      call adjust_sph_temp_bc_by_reftemp                                &
     &   (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(2),                 &
     &    reftemp_rj, sph_bc_T)
!
      end subroutine init_reference_fields
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine adjust_press_by_average_on_CMB                         &
     &         (kr_in, kr_out, sph_rj, rj_fld)
!
      use m_sph_phys_address
!
      use set_reference_sph_mhd
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call adjust_by_ave_pressure_on_CMB(kr_in, kr_out,                 &
     &    sph_rj%idx_rj_degree_zero, rj_fld%n_point, sph_rj%nidx_rj,    &
     &    rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine adjust_press_by_average_on_CMB
!
! -----------------------------------------------------------------------
!
      subroutine sync_temp_by_per_temp_sph(reftemp_rj, sph_rj, rj_fld)
!
      use m_sph_phys_address
!
      use set_reference_sph_mhd
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind=kreal), intent(in)                                      &
     &               :: reftemp_rj(sph_rj%nidx_rj(1),0:1)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call chenge_temp_to_per_temp_sph(sph_rj%idx_rj_degree_zero,       &
     &    rj_fld%n_point, sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,        &
     &    reftemp_rj, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine sync_temp_by_per_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine trans_per_temp_to_temp_sph                             &
     &         (reftemp_rj, sph_rj, rj_fld)
!
      use m_sph_phys_address
!
      use set_reference_sph_mhd
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind=kreal), intent(in)                                      &
     &        :: reftemp_rj(sph_rj%nidx_rj(1),0:1)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call transfer_per_temp_to_temp_sph(sph_rj%idx_rj_degree_zero,     &
     &    rj_fld%n_point, sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,        &
     &    reftemp_rj, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine trans_per_temp_to_temp_sph
!
! -----------------------------------------------------------------------
!
      end module adjust_reference_fields
