!>@file   m_work_4_sph_trans_fldout.f90
!!@brief  module m_work_4_sph_trans_fldout
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Field data for Legendre transform
!!       (innermost loop is field and radius)
!!
!!@verbatim
!!      subroutine allocate_work_sph_trans_krin(nb_sph_trans)
!!      subroutine deallocate_work_sph_trans_krin
!!
!!      subroutine clear_b_trans_field_fldout(nb)
!!      subroutine clear_f_trans_field_fldout(nb)
!!
!!    Data for single vector field
!!      radial component:      vr_rtp(3*i_rtp-2)
!!      elevetional component: vr_rtp(3*i_rtp-1)
!!      azimuthal component:   vr_rtp(2*i_rtp  )
!!
!!    Data for single vector spectrum
!!      Poloidal component:          sp_rj(3*i_rj-2)
!!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!!      Toroidal component:          sp_rj(3*i_rj  )
!!
!!    Data for single scalar
!!      field: vr_rtp(i_rtp)
!!      spectr: sp_rj(i_rj)
!!
!!@endverbatim
!!
!!@n @param  nb_sph_trans
!!             maximum number of fields for Legendre transform
!!@n @param  nb  number of fields to be transformed
!
      module m_work_4_sph_trans_fldout
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!>      field data for Legendre transform  @f$ f(r,\theta,m) @f$ 
!!@n     Order: vr_rtm_fdout(l_rtm,k_rtm,m_rtm,i_comp,i_fld)
!!@n     size:  vr_rtm_fdout(nidx_rtm(2)*nidx_rtm(1)*nidx_rtm(3),3*nb)
      real(kind = kreal), allocatable :: vr_rtm_fdout(:,:)
!
!>      Spectr data for Legendre transform  @f$ f(r,l,m) @f$ 
!>@n      Order: sp_rlm(i_comp,i_fld,j_rlm,k_rtm)
!>@n      size: sp_rlm(3*nb*nidx_rlm(2)*nidx_rtm(1))
      real(kind = kreal), allocatable :: sp_rlm_fdout(:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_sph_trans_krin(nb_sph_trans)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb_sph_trans
!
!
      allocate(vr_rtm_fdout(nnod_rtm,3*nb_sph_trans) )
      vr_rtm_fdout = 0.0d0
!
      allocate(sp_rlm_fdout(nnod_rlm,3*nb_sph_trans))
      sp_rlm_fdout = 0.0d0
!
      end subroutine allocate_work_sph_trans_krin
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_sph_trans_krin
!
      deallocate(sp_rlm_fdout, vr_rtm_fdout)
!
      end subroutine deallocate_work_sph_trans_krin
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_b_trans_field_fldout(nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb
      integer(kind = kint) :: nd, inod
!
!
!$omp parallel private(nd,inod)
      do nd = 1, nb
!$omp do
        do inod = 1, nnod_rtm
          vr_rtm_fdout(inod,nd) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine clear_b_trans_field_fldout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_field_fldout(nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) ::  inod, nd
!
!$omp parallel private(nd,inod)
      do nd = 1, nb
!$omp do
        do inod = 1, nnod_rlm
          sp_rlm_fdout(inod,nd) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine clear_f_trans_field_fldout
!
! -----------------------------------------------------------------------
!
      end module m_work_4_sph_trans_fldout
