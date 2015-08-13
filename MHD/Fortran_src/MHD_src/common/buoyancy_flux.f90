!> @file  buoyancy_flux.f90
!!      module buoyancy_flux
!!
!! @author  H. Matsui
!! @date Programmed in June, 2005
!
!> @brief Evaluate buoyancy flux
!!
!!@verbatim
!!      subroutine cal_gravity_flux(coef, i_scalar, i_flux)
!!@endverbatim
!
      module buoyancy_flux
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
!
      implicit none
!
      private :: cal_gravity_flux_self
      private :: cal_gravity_flux_radial, cal_gravity_flux_const
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_gravity_flux(coef, i_scalar, i_flux)
!
      use m_physical_property
!
      integer (kind = kint), intent(in) :: i_scalar, i_flux
      real (kind = kreal), intent(in) :: coef
!
!
      if      (i_grav .eq. iflag_const_g) then
        call cal_gravity_flux_const(grav, coef, i_scalar, i_flux)
      else if (i_grav .eq. iflag_radial_g) then
        call cal_gravity_flux_radial(coef, i_scalar, i_flux)
      else if (i_grav .eq. iflag_self_r_g) then
        call cal_gravity_flux_self(coef, i_scalar, i_flux)
      end if
!
      end subroutine cal_gravity_flux
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_gravity_flux_self(coef_buo, i_scalar, i_flux)
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: i_scalar, i_flux
      real (kind = kreal), intent(in) :: coef_buo
!
      integer (kind = kint) :: inod, ip, ist, ied
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = node1%istack_nod_smp(ip-1) + 1
        ied = node1%istack_nod_smp(ip)
        do inod = ist, ied
          d_nod(inod,i_flux) = coef_buo * d_nod(inod,i_scalar)          &
     &                    * ( d_nod(inod,iphys%i_velo  ) * xx(inod,1)   &
     &                      + d_nod(inod,iphys%i_velo+1) * xx(inod,2)   &
     &                      + d_nod(inod,iphys%i_velo+2) * xx(inod,3))
         end do
       end do
!$omp end parallel do
!
      end subroutine cal_gravity_flux_self
!
! -----------------------------------------------------------------------
!
      subroutine cal_gravity_flux_radial(coef_buo, i_scalar, i_flux)
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: i_scalar, i_flux
      real (kind = kreal), intent(in) :: coef_buo
!
      integer (kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = node1%istack_nod_smp(ip-1) + 1
        ied = node1%istack_nod_smp(ip)
        do inod = ist, ied
            d_nod(inod,i_flux) = coef_buo * d_nod(inod,i_scalar)        &
     &       * (d_nod(inod,iphys%i_velo  ) * xx(inod,1)*a_radius(inod)  &
     &        + d_nod(inod,iphys%i_velo+1) * xx(inod,2)*a_radius(inod)  &
     &        + d_nod(inod,iphys%i_velo+2) * xx(inod,3)*a_radius(inod))
         end do
       end do
!$omp end parallel do
!
!
      end subroutine cal_gravity_flux_radial
!
! -----------------------------------------------------------------------
!
      subroutine cal_gravity_flux_const(grav, coef_buo,                 &
     &          i_scalar, i_flux)
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: i_scalar, i_flux
      real (kind = kreal), intent(in) :: coef_buo
      real (kind = kreal), intent(in) :: grav(3)
!
      integer (kind = kint) :: inod, ip, ist, ied
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = node1%istack_nod_smp(ip-1) + 1
        ied = node1%istack_nod_smp(ip)
        do inod = ist, ied
          d_nod(inod,i_flux) = coef_buo * d_nod(inod,i_scalar)          &
     &              * ( d_nod(inod,iphys%i_velo  ) * grav(1)            &
     &                + d_nod(inod,iphys%i_velo+1) * grav(2)            &
     &                + d_nod(inod,iphys%i_velo+2) * grav(3) )
         end do
       end do
!$omp end parallel do
!
      end subroutine cal_gravity_flux_const
!
! -----------------------------------------------------------------------
!
      end module buoyancy_flux
