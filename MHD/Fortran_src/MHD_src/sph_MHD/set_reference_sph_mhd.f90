!> @file  set_reference_sph_mhd.f90
!!      module set_reference_sph_mhd
!!
!! @author  H. Matsui
!! @date Programmed in Sep., 2007
!
!> @brief Convert temperature data using reference temperature
!!
!!@verbatim
!!      subroutine adjust_by_ave_pressure_on_CMB(kr_in, kr_out,         &
!!     &          idx_rj_degree_zero, nidx_rj, i_press,                 &
!!     &          n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine set_ref_temp_sph_mhd                                 &
!!     &         (low_temp, depth_top, high_temp, depth_bottom,         &
!!     &          nidx_rj, r_ICB, r_CMB, ar_1d_rj, kr_ICB, kr_CMB,      &
!!     &          reftemp_rj)
!!      subroutine adjust_sph_temp_bc_by_reftemp                        &
!!     &         (idx_rj_degree_zero, nri, reftemp_rj, sph_bc_T)
!!
!!      subroutine chenge_temp_to_per_temp_sph(idx_rj_degree_zero,      &
!!     &         nidx_rj, radius_1d_rj_r, reftemp_rj, ipol, idpdr,      &
!!     &         n_point, ntot_phys_rj, d_rj)
!!        d_rj(inod,ipol%i_temp):        T => \Theta = T - T0
!!        d_rj(inod,ipol%i_par_temp):    \Theta = T - T0
!!        d_rj(inod,ipol%i_grad_t):      T => d \Theta / dr
!!        d_rj(inod,ipol%i_grad_part_t): d \Theta / dr
!!
!!
!!      subroutine transfer_per_temp_to_temp_sph(idx_rj_degree_zero,    &
!!     &          nidx_rj, radius_1d_rj_r, reftemp_rj, ipol, idpdr,     &
!!     &          n_point, ntot_phys_rj, d_rj)
!!        type(phys_address), intent(in) :: ipol, idpdr
!!        d_rj(inod,ipol%i_temp):        \Theta = T - T0 => T
!!        d_rj(inod,ipol%i_par_temp):    \Theta = T - T0
!!        d_rj(inod,ipol%i_grad_t):      d \Theta / dr   => dT / dr
!!        d_rj(inod,ipol%i_grad_part_t): d \Theta / dr
!!
!!      subroutine delete_zero_degree_comp(is_fld, idx_rj_degree_zero,  &
!!     &          n_point, nidx_rj, ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@param sph_bc_T  Structure for basic boundary condition parameters
!!                 for temperature
!!@n @param is_fld Address of poloidal component
!!
!!@n @param ntot_phys_rj   Total number of components
!!@n @param d_rj           Spectrum data
!
      module set_reference_sph_mhd
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use t_phys_address
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine adjust_by_ave_pressure_on_CMB(kr_in, kr_out,           &
     &          idx_rj_degree_zero, nidx_rj, i_press,                   &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: i_press
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, inod
      real(kind = kreal) :: ref_p
!
!
      if (idx_rj_degree_zero .eq. 0) return
!
      inod = idx_rj_degree_zero + (kr_out-1)*nidx_rj(2)
      ref_p = d_rj(inod,i_press)
!
      do k = kr_in, kr_out
        inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
        d_rj(inod,i_press) = d_rj(inod,i_press) - ref_p
      end do
!
      end subroutine adjust_by_ave_pressure_on_CMB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ref_temp_sph_mhd                                   &
     &         (low_temp, depth_top, high_temp, depth_bottom,           &
     &          nidx_rj, r_ICB, r_CMB, ar_1d_rj, kr_ICB, kr_CMB,        &
     &          reftemp_rj)
!
      use m_physical_property
!
      real (kind = kreal), intent(in) :: low_temp, high_temp
      real (kind = kreal), intent(inout) :: depth_top, depth_bottom
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: kr_ICB, kr_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind=kreal), intent(in) :: ar_1d_rj(nidx_rj(1),3)
!
      real(kind=kreal), intent(inout) :: reftemp_rj(nidx_rj(1),0:1)
!
      integer (kind = kint) :: k
!
! set reference temperature (for spherical shell)
!
      if (ref_param_T1%iflag_reference .eq. id_sphere_ref_temp) then
        do k = 1, kr_ICB-1
          reftemp_rj(k,0) = high_temp
          reftemp_rj(k,1) = zero
        end do
        do k = kr_ICB, kr_CMB
          reftemp_rj(k,0) = (depth_bottom*depth_top*ar_1d_rj(k,1)       &
     &                   * (high_temp - low_temp)                       &
     &                    - depth_bottom*high_temp                      &
     &                    + depth_top* low_temp )                       &
     &                     / (depth_top - depth_bottom)
          reftemp_rj(k,1) = - depth_bottom*depth_top*ar_1d_rj(k,2)      &
     &                   * (high_temp - low_temp)                       &
     &                     / (depth_top - depth_bottom)
        end do
        do k = kr_CMB+1, nidx_rj(1)
          reftemp_rj(k,0) = low_temp
          reftemp_rj(k,1) = zero
        end do
      else
        reftemp_rj(1:nidx_rj(1),0) = zero
        reftemp_rj(1:nidx_rj(1),1) = zero
        depth_bottom = r_ICB
        depth_top =    r_CMB
      end if
!
      end subroutine set_ref_temp_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine adjust_sph_temp_bc_by_reftemp                          &
     &         (idx_rj_degree_zero, nri, reftemp_rj, sph_bc_T)
!
      use t_boundary_params_sph_MHD
      use m_physical_property
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nri
      real(kind=kreal), intent(in) :: reftemp_rj(nri,0:1)
!
      type(sph_boundary_type), intent(inout) :: sph_bc_T
!
!
      if(ref_param_T1%iflag_reference .eq. id_sphere_ref_temp           &
     &       .and. idx_rj_degree_zero .gt. 0) then
        sph_bc_T%ICB_fld(idx_rj_degree_zero)                            &
     &   = sph_bc_T%ICB_fld(idx_rj_degree_zero)                         &
     &    - reftemp_rj(sph_bc_T%kr_in,0)
        sph_bc_T%CMB_fld(idx_rj_degree_zero)                            &
     &   = sph_bc_T%CMB_fld(idx_rj_degree_zero)                         &
     &     - reftemp_rj(sph_bc_T%kr_out,0)
        sph_bc_T%ICB_flux(idx_rj_degree_zero)                           &
     &   = sph_bc_T%ICB_flux(idx_rj_degree_zero)                        &
     &    - reftemp_rj(sph_bc_T%kr_in,1)
        sph_bc_T%CMB_flux(idx_rj_degree_zero)                           &
     &   = sph_bc_T%CMB_flux(idx_rj_degree_zero)                        &
     &    - reftemp_rj(sph_bc_T%kr_out,1)
      end if
!
      end subroutine adjust_sph_temp_bc_by_reftemp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine chenge_temp_to_per_temp_sph(idx_rj_degree_zero,        &
     &         nidx_rj, radius_1d_rj_r, reftemp_rj, ipol, idpdr,        &
     &         n_point, ntot_phys_rj, d_rj)
!
      use m_physical_property
!
      type(phys_address), intent(in) :: ipol, idpdr
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind=kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind=kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:1)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, inod
!
!
      if (ref_param_T1%iflag_reference .ne. id_sphere_ref_temp) return
!
      if (idx_rj_degree_zero .gt. 0) then
        do k = 1, nidx_rj(1)
          inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
          d_rj(inod,ipol%i_temp) = d_rj(inod,ipol%i_temp)               &
     &                    - reftemp_rj(k,0)
          d_rj(inod,ipol%i_grad_t) = d_rj(inod,ipol%i_grad_t)           &
     &                    - two*reftemp_rj(k,1) * radius_1d_rj_r(k)**2
          d_rj(inod,idpdr%i_grad_t) = d_rj(inod,ipol%i_temp)
        end do
      end if
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,ipol%i_par_temp) =     d_rj(inod,ipol%i_temp)
        d_rj(inod,ipol%i_grad_part_t) =  d_rj(inod,ipol%i_grad_t)
        d_rj(inod,idpdr%i_grad_part_t) = d_rj(inod,ipol%i_temp)
      end do
!$omp end parallel do
!
      end subroutine chenge_temp_to_per_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine transfer_per_temp_to_temp_sph(idx_rj_degree_zero,      &
     &          nidx_rj, radius_1d_rj_r, reftemp_rj, ipol, idpdr,       &
     &          n_point, ntot_phys_rj, d_rj)
!
      use m_physical_property
!
      type(phys_address), intent(in) :: ipol, idpdr
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind=kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind=kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:1)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, inod
!
!
      if (ref_param_T1%iflag_reference .ne. id_sphere_ref_temp) return
!
!$omp parallel do
      do inod = 1, n_point
        d_rj(inod,ipol%i_par_temp) =    d_rj(inod,ipol%i_temp)
        d_rj(inod,ipol%i_grad_part_t) = d_rj(inod,ipol%i_grad_t)
      end do
!$omp end parallel do
!
      if (idx_rj_degree_zero .gt. 0) then
        do k = 1, nidx_rj(1)
          inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
          d_rj(inod,ipol%i_temp) = d_rj(inod,ipol%i_temp)               &
     &                            + reftemp_rj(k,0)
          d_rj(inod,ipol%i_grad_t) = d_rj(inod,ipol%i_grad_part_t)      &
     &                 + two*reftemp_rj(k,1) * radius_1d_rj_r(k)**2
          d_rj(inod,idpdr%i_grad_t) = d_rj(inod,ipol%i_temp)
        end do
      end if
!
      end subroutine transfer_per_temp_to_temp_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine delete_zero_degree_comp(is_fld, idx_rj_degree_zero,    &
     &          n_point, nidx_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: n_point, nidx_rj(2)
      integer(kind = kint), intent(in) :: ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, inod
!
!
      if (idx_rj_degree_zero .eq. 0) return
!
      do k = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
        d_rj(inod,is_fld  ) = zero
        d_rj(inod,is_fld+2) = zero
      end do
!
      end subroutine delete_zero_degree_comp
!
! -----------------------------------------------------------------------
!
      end module set_reference_sph_mhd

