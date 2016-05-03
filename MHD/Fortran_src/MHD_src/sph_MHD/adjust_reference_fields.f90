!>@file   adjust_reference_fields.f90
!!@brief  module adjust_reference_fields
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2015
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine adjust_press_by_average_on_CMB(kr_in, kr_out, rj_fld)
!!      subroutine sync_temp_by_per_temp_sph(reftemp_rj, rj_fld)
!!        d_rj(inod,ipol%i_temp):        T => \Theta = T - T0
!!        d_rj(inod,ipol%i_par_temp):    \Theta = T - T0
!!        d_rj(inod,ipol%i_grad_t):      T => d \Theta / dr
!!        d_rj(inod,ipol%i_grad_part_t): d \Theta / dr
!!      subroutine trans_per_temp_to_temp_sph(reftemp_rj, rj_fld)
!!        d_rj(inod,ipol%i_temp):        \Theta = T - T0 => T
!!        d_rj(inod,ipol%i_par_temp):    \Theta = T - T0
!!        d_rj(inod,ipol%i_grad_t):      d \Theta / dr   => dT / dr
!!        d_rj(inod,ipol%i_grad_part_t): d \Theta / dr
!!@endverbatim
!
      module adjust_reference_fields
!
      use m_precision
      use m_machine_parameter
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
      subroutine init_reference_fields
!
      use m_sph_spectr_data
      use m_boundary_params_sph_MHD
      use m_spheric_parameter
!
      use set_reference_sph_mhd
!
!      Set reference temperature and adjust boundary conditions
!
      if(iflag_debug .gt. 0) write(*,*) 'set_ref_temp_sph_mhd'
      call allocate_reft_rj_data(sph_rj1)
      call set_ref_temp_sph_mhd(sph_rj1%nidx_rj, r_ICB, r_CMB,          &
     &    sph_rj1%ar_1d_rj, sph_bc_T, reftemp_rj)
      call adjust_sph_temp_bc_by_reftemp                                &
     &   (sph_rj1%idx_rj_degree_zero, sph_rj1%nidx_rj(2),               &
     &    reftemp_rj, sph_bc_T)
!
      end subroutine init_reference_fields
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine adjust_press_by_average_on_CMB(kr_in, kr_out, rj_fld)
!
      use m_spheric_parameter
      use m_sph_phys_address
!
      use set_reference_sph_mhd
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(phys_data), intent(inout) :: rj_fld
!
!
      call adjust_by_ave_pressure_on_CMB(kr_in, kr_out,                 &
     &    sph_rj1%idx_rj_degree_zero, rj_fld%n_point, sph_rj1%nidx_rj,  &
     &    rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine adjust_press_by_average_on_CMB
!
! -----------------------------------------------------------------------
!
      subroutine sync_temp_by_per_temp_sph(reftemp_rj, rj_fld)
!
      use m_spheric_parameter
      use m_sph_phys_address
!
      use set_reference_sph_mhd
!
      real(kind=kreal), intent(in)                                      &
     &               :: reftemp_rj(sph_rj1%nidx_rj(1),0:1)
      type(phys_data), intent(inout) :: rj_fld
!
!
      call chenge_temp_to_per_temp_sph(sph_rj1%idx_rj_degree_zero,      &
     &    rj_fld%n_point, sph_rj1%nidx_rj, sph_rj1%radius_1d_rj_r,      &
     &    reftemp_rj, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine sync_temp_by_per_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine trans_per_temp_to_temp_sph(reftemp_rj, rj_fld)
!
      use m_spheric_parameter
      use m_sph_phys_address
!
      use set_reference_sph_mhd
!
      real(kind=kreal), intent(in)                                      &
     &        :: reftemp_rj(sph_rj1%nidx_rj(1),0:1)
      type(phys_data), intent(inout) :: rj_fld
!
!
      call transfer_per_temp_to_temp_sph(sph_rj1%idx_rj_degree_zero,    &
     &    rj_fld%n_point, sph_rj1%nidx_rj, sph_rj1%radius_1d_rj_r,      &
     &    reftemp_rj, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine trans_per_temp_to_temp_sph
!
! -----------------------------------------------------------------------
!
      end module adjust_reference_fields
