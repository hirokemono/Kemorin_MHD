!>@file   m_legendre_work_matmul.f90
!!@brief  module m_legendre_work_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine allocate_hemi_schmidt_rtm(nth_rtm, jmax_rlm)
!!      subroutine deallocate_hemi_schmidt_rtm
!!
!!      subroutine alloc_leg_vec_sym_matmul                             &
!!     &         (nth_rtm, maxidx_rtm_r_smp, nvector)
!!      subroutine alloc_leg_scl_sym_matmul                             &
!!     &         (nth_rtm, maxidx_rtm_r_smp, nscalar)
!!      subroutine dealloc_leg_vec_sym_matmul
!!      subroutine dealloc_leg_scl_sym_matmul
!!
!!      subroutine alloc_leg_vec_matmul                                 &
!!     &         (nth_rtm, maxidx_rtm_r_smp, nvector)
!!      subroutine alloc_leg_scl_matmul                                 &
!!     &         (nth_rtm, maxidx_rtm_r_smp, nscalar)
!!      subroutine dealloc_leg_vec_matmul
!!
!!      subroutine alloc_leg_vec_symmetry(nth_rtm)
!!      subroutine alloc_leg_scl_symmetry(nth_rtm)
!!
!!      subroutine alloc_leg_vec_blocked(nth_rtm)
!!      subroutine alloc_leg_scl_blocked(nth_rtm)
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm(l_rtm,m_rtm,k_rtm,icomp)
!!       size: vr_rtm(nth_rtm,nidx_rtm(1)*ncomp,nidx_rtm(3))
!!      real(kind = kreal), allocatable :: vr_rtm(:,:,:)
!!
!!     spectr data for Legendre transform
!!       original layout: sp_rlm(j_rlm,k_rtm,icomp)
!!        size: sp_rlm(nidx_rlm(2),nidx_rtm(1)*ncomp)
!!      real(kind = kreal), allocatable :: sp_rlm(:,:)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module m_legendre_work_matmul
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_4_sph_trans
      use matmul_for_legendre_trans
!
      implicit none
!
!
!>     Maximum matrix size for spectr data
      integer(kind = kint) :: nvec_jk
!>     Poloidal component with evem (l-m)
      real(kind = kreal), allocatable :: pol_e(:,:)
!>     radial difference of Poloidal component with evem (l-m)
      real(kind = kreal), allocatable :: dpoldt_e(:,:)
!>     radial difference of Poloidal component with evem (l-m)
      real(kind = kreal), allocatable :: dpoldp_e(:,:)
!>     Toroidal component with evem (l-m)
      real(kind = kreal), allocatable :: dtordt_e(:,:)
!>     Toroidal component with evem (l-m)
      real(kind = kreal), allocatable :: dtordp_e(:,:)
!
!>     Maximum matrix size for field data
      integer(kind = kint) :: nvec_lk
!>     Symmetric radial component
      real(kind = kreal), allocatable :: symp_r(:,:)
!>     Anti-symmetric theta-component
      real(kind = kreal), allocatable :: asmp_t(:,:)
!>     Anti-symmetric phi-component
      real(kind = kreal), allocatable :: asmp_p(:,:)
!>     Symmetric theta-component with condugate order
      real(kind = kreal), allocatable :: symn_t(:,:)
!>     Symmetric phi-component with condugate order
      real(kind = kreal), allocatable :: symn_p(:,:)
!
!>     Maximum matrix size for spectr data
      integer(kind = kint) :: nscl_jk
!>     Scalar with evem (l-m)
      real(kind = kreal), allocatable :: scl_e(:,:)
!
!>     Maximum matrix size for field data
      integer(kind = kint) :: nscl_lk
!>     Symmetric scalar component
      real(kind = kreal), allocatable :: symp(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_matmul                                   &
     &         (nth_rtm, maxidx_rtm_r_smp, nvector)
!
      integer(kind = kint), intent(in) :: nth_rtm
      integer(kind = kint), intent(in) :: maxidx_rtm_r_smp
      integer(kind = kint), intent(in) :: nvector
!
!
      nvec_jk = maxdegree_rlm * maxidx_rtm_r_smp * nvector
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpoldt_e(nvec_jk,np_smp))
      allocate(dpoldp_e(nvec_jk,np_smp))
      allocate(dtordt_e(nvec_jk,np_smp))
      allocate(dtordp_e(nvec_jk,np_smp))
!
      nvec_lk = nth_rtm * maxidx_rtm_r_smp * nvector
      allocate(symp_r(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_matmul
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_matmul                                   &
     &         (nth_rtm, maxidx_rtm_r_smp, nscalar)
!
      integer(kind = kint), intent(in) :: nth_rtm
      integer(kind = kint), intent(in) :: maxidx_rtm_r_smp
      integer(kind = kint), intent(in) :: nscalar
!
!
      nscl_jk = maxdegree_rlm * maxidx_rtm_r_smp * nscalar
      allocate(scl_e(nscl_jk,np_smp))
!
      nscl_lk = nth_rtm * maxidx_rtm_r_smp * nscalar
      allocate(symp(nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_matmul
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_vec_matmul
!
!
      deallocate(pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e)
      deallocate(symp_r, symn_t, symn_p, asmp_t, asmp_p)
!
      deallocate(scl_e, symp)
!
      end subroutine dealloc_leg_vec_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_blocked(nth_rtm)
!
      integer(kind = kint), intent(in) :: nth_rtm
!
!
      nvec_jk = maxdegree_rlm
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpoldt_e(nvec_jk,np_smp))
      allocate(dpoldp_e(nvec_jk,np_smp))
      allocate(dtordt_e(nvec_jk,np_smp))
      allocate(dtordp_e(nvec_jk,np_smp))
!
      nvec_lk = nth_rtm
      allocate(symp_r(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_blocked
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_blocked(nth_rtm)
!
      integer(kind = kint), intent(in) :: nth_rtm
!
!
      nscl_jk = maxdegree_rlm
      allocate(scl_e(nscl_jk,np_smp))
!
      nscl_lk = nth_rtm
      allocate(symp(nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_blocked
!
! -----------------------------------------------------------------------
!
      end module m_legendre_work_matmul
