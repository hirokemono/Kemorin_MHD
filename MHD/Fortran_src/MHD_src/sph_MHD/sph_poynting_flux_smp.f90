!>@file   sph_poynting_flux_smp.f90
!!@brief  module sph_poynting_flux_smp
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!
!>@brief Evaluate poynting flux for nodal field
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_rtp_electric_field_smp
!!      subroutine cal_rtp_poynting_flux_smp
!!      subroutine cal_rtp_magnetic_streaching
!!@endverbatim
!
      module sph_poynting_flux_smp
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_physical_property
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_rtp_electric_field_smp
!
      use poynting_flux_smp
!
!
      call cal_electric_field_smp(np_smp, nnod_rtp, inod_rtp_smp_stack, &
     &    coef_d_magne, d_rtp(1,irtp%i_current),                        &
     &    d_rtp(1,irtp%i_vp_induct), d_rtp(1,irtp%i_electric))
!
      end subroutine cal_rtp_electric_field_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_rtp_poynting_flux_smp
!
      use poynting_flux_smp
!
!
      call cal_poynting_flux_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,  &
     &     coef_d_magne, d_rtp(1,irtp%i_current),                       &
     &     d_rtp(1,irtp%i_vp_induct), d_rtp(1,irtp%i_magne),            &
     &     d_rtp(1,irtp%i_poynting))
!
      end subroutine cal_rtp_poynting_flux_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rtp_magnetic_streaching
!
      use poynting_flux_smp
      use m_work_4_sph_trans
!
      call cal_rtp_magnetic_streach(nnod_rtp, nidx_rtp, a_r_1d_rtp_r,   &
     &   cot_theta_1d_rtp, d_rtp(1,irtp%i_magne), d_rtp(1,irtp%i_velo), &
     &   d_rtp(1,irtp%i_grad_vx), d_rtp(1,irtp%i_grad_vy),              &
     &   d_rtp(1,irtp%i_grad_vz), d_rtp(1,irtp%i_mag_stretch) )
!
      end subroutine cal_rtp_magnetic_streaching
!
! -----------------------------------------------------------------------
!
      end module sph_poynting_flux_smp
