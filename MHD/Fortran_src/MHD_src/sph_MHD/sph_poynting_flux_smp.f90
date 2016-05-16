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
!!      subroutine copy_velo_to_grad_v_rtp
!!      subroutine cal_grad_of_velocities_sph(rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module sph_poynting_flux_smp
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
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
      subroutine copy_velo_to_grad_v_rtp
!
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_tmp
      use m_work_4_sph_trans
      use sel_fld_copy_4_sph_trans
!
!
      if(ft_trns%i_grad_vx.gt.0) then
        call sel_scalar_from_trans(nnod_rtp, nidx_rtp, ione,            &
     &      sph_rtp1%istack_inod_rtp_smp, nnod_rtp,                     &
     &      fld_rtp(1,b_trns%i_velo  ), frt_rtp(1,ft_trns%i_grad_vx) )
      end if
      if(ft_trns%i_grad_vy.gt.0) then
        call sel_scalar_from_trans(nnod_rtp, nidx_rtp, ione,            &
     &      sph_rtp1%istack_inod_rtp_smp, nnod_rtp,                     &
     &      fld_rtp(1,b_trns%i_velo+1), frt_rtp(1,ft_trns%i_grad_vy) )
      end if
      if(ft_trns%i_grad_vz.gt.0) then
        call sel_scalar_from_trans(nnod_rtp, nidx_rtp, ione,            &
     &      sph_rtp1%istack_inod_rtp_smp, nnod_rtp,                     &
     &      fld_rtp(1,b_trns%i_velo+2), frt_rtp(1,ft_trns%i_grad_vz) )
      end if
!
      end subroutine copy_velo_to_grad_v_rtp
!
! -----------------------------------------------------------------------
!
      subroutine cal_grad_of_velocities_sph(rj_fld)
!
      use t_phys_data
      use m_boundary_params_sph_MHD
      use const_sph_radial_grad
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol%i_mag_stretch .eq. 0) return
!
      call copy_grad_vect_to_m_stretch                                  &
     &   (rj_fld%ntot_phys, rj_fld%d_fld)
!
      call const_sph_gradient_no_bc(sph_rj1, sph_bc_U,                  &
     &   (ipol%i_mag_stretch  ), ipol%i_grad_vx, rj_fld)
      call const_sph_gradient_no_bc(sph_rj1, sph_bc_U,                  &
     &   (ipol%i_mag_stretch+1), ipol%i_grad_vy, rj_fld)
      call const_sph_gradient_no_bc(sph_rj1, sph_bc_U,                  &
     &   (ipol%i_mag_stretch+2), ipol%i_grad_vz, rj_fld)
!
      end subroutine cal_grad_of_velocities_sph
!
! -----------------------------------------------------------------------
!
      subroutine copy_grad_vect_to_m_stretch(ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
!
      integer(kind = kint) :: ip, ist, ied, inod
!
      if(ipol%i_mag_stretch .eq. 0) return
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = sph_rj1%istack_inod_rj_smp(ip-1) + 1
        ied = sph_rj1%istack_inod_rj_smp(ip)
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
