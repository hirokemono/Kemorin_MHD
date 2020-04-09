!>@file   set_sph_phys_address.f90
!!@brief  module set_sph_phys_address
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2007
!
!>@brief  start addresses for spetr fields
!!
!!@verbatim
!!      subroutine set_sph_sprctr_data_address(sph_rj, ipol, rj_fld)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(inout) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module set_sph_phys_address
!
      use m_precision
      use m_constants
!
      use t_phys_address
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_sph_sprctr_data_address(sph_rj, ipol, rj_fld)
!
      use t_spheric_rj_data
      use t_phys_data
!
      use set_field_address
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(inout) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!   set address of spectr fields
!
      call alloc_phys_data_type(sph_rj%nnod_rj, rj_fld)
      call set_field_addresses(ione, rj_fld%num_phys,                   &
     &    rj_fld%phys_name, rj_fld%num_component, ipol)
!
      end subroutine set_sph_sprctr_data_address
!
!  --------------------------------------------------------------------
!
      end module set_sph_phys_address
