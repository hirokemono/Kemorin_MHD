!> @file  cal_energy_flux_rj.f90
!!      module cal_energy_flux_rj
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine s_cal_energy_flux_rj(rj_fld)
!!@endverbatim
!
      module cal_energy_flux_rj
!
      use m_precision
      use m_constants
!
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
      subroutine s_cal_energy_flux_rj(rj_fld)
!
      use m_sph_phys_address
      use const_sph_radial_grad
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol%i_rot_Coriolis .gt. 0) then
        call const_grad_poloidal_moment(ipol%i_rot_Coriolis, rj_fld)
      end if
!
      end subroutine s_cal_energy_flux_rj
!
!-----------------------------------------------------------------------
!
      end module cal_energy_flux_rj
