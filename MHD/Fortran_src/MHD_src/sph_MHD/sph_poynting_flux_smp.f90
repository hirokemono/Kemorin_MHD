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
!!      subroutine copy_velo_to_grad_v_rtp(sph_rtp, b_trns, ft_trns,    &
!!     &          ncomp_rj_2_rtp, ncomp_tmp_rtp_2_rj, fld_rtp, frt_rtp)
!!      subroutine cal_grad_of_velocities_sph(sph_rj, g_sph_rj, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module sph_poynting_flux_smp
!
      use m_precision
!
      use m_machine_parameter
      use m_sph_phys_address
      use m_physical_property
!
      private :: copy_grad_vect_to_m_stretch
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_velo_to_grad_v_rtp(sph_rtp, b_trns, ft_trns,      &
     &          ncomp_rj_2_rtp, ncomp_tmp_rtp_2_rj, fld_rtp, frt_rtp)
!
      use t_spheric_rtp_data
      use t_phys_address
      use m_work_4_sph_trans
      use sel_fld_copy_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: b_trns
      type(phys_address), intent(in) :: ft_trns
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_tmp_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: fld_rtp(sph_rtp%nnod_rtp,ncomp_rj_2_rtp)
      real(kind = kreal), intent(inout)                                 &
     &                   :: frt_rtp(sph_rtp%nnod_rtp,ncomp_tmp_rtp_2_rj)
!
!
      if(ft_trns%i_grad_vx.gt.0) then
        call sel_scalar_from_trans                                      &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, ione,                   &
     &      sph_rtp%istack_inod_rtp_smp, sph_rtp%nnod_rtp,              &
     &      fld_rtp(1,b_trns%i_velo  ), frt_rtp(1,ft_trns%i_grad_vx) )
      end if
      if(ft_trns%i_grad_vy.gt.0) then
        call sel_scalar_from_trans                                      &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, ione,                   &
     &      sph_rtp%istack_inod_rtp_smp, sph_rtp%nnod_rtp,              &
     &      fld_rtp(1,b_trns%i_velo+1), frt_rtp(1,ft_trns%i_grad_vy) )
      end if
      if(ft_trns%i_grad_vz.gt.0) then
        call sel_scalar_from_trans                                      &
      &    (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, ione,                   &
     &      sph_rtp%istack_inod_rtp_smp, sph_rtp%nnod_rtp,              &
     &      fld_rtp(1,b_trns%i_velo+2), frt_rtp(1,ft_trns%i_grad_vz) )
      end if
!
      end subroutine copy_velo_to_grad_v_rtp
!
! -----------------------------------------------------------------------
!
      subroutine cal_grad_of_velocities_sph(sph_rj, g_sph_rj, rj_fld)
!
      use t_spheric_rj_data
      use t_phys_data
      use m_boundary_params_sph_MHD
      use const_sph_radial_grad
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol%i_mag_stretch .eq. 0) return
!
      call copy_grad_vect_to_m_stretch(sph_rj%istack_inod_rj_smp,       &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call const_sph_gradient_no_bc(sph_rj, sph_bc_U, g_sph_rj,         &
     &   (ipol%i_mag_stretch  ), ipol%i_grad_vx, rj_fld)
      call const_sph_gradient_no_bc(sph_rj, sph_bc_U, g_sph_rj,         &
     &   (ipol%i_mag_stretch+1), ipol%i_grad_vy, rj_fld)
      call const_sph_gradient_no_bc(sph_rj, sph_bc_U, g_sph_rj,         &
     &   (ipol%i_mag_stretch+2), ipol%i_grad_vz, rj_fld)
!
      end subroutine cal_grad_of_velocities_sph
!
! -----------------------------------------------------------------------
!
      subroutine copy_grad_vect_to_m_stretch                            &
     &         (inod_rj_smp_stack, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: inod_rj_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
!
      integer(kind = kint) :: ip, ist, ied, inod
!
      if(ipol%i_mag_stretch .eq. 0) return
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          d_rj(inod,ipol%i_mag_stretch  ) = d_rj(inod,ipol%i_grad_vx)
          d_rj(inod,ipol%i_mag_stretch+1) = d_rj(inod,ipol%i_grad_vy)
          d_rj(inod,ipol%i_mag_stretch+2) = d_rj(inod,ipol%i_grad_vz)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_grad_vect_to_m_stretch
!
! -----------------------------------------------------------------------
!
      end module sph_poynting_flux_smp
