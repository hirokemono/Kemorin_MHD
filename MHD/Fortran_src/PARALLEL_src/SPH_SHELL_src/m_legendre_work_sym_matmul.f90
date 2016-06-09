!>@file   m_legendre_work_sym_matmul.f90
!!@brief  module m_legendre_work_sym_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Work array for forward Legendre transform useing mat multi 
!>@n      data are strored communication buffer
!!
!!@verbatim
!!      subroutine init_legendre_sym_matmul                             &
!!     &         (sph_rtm, sph_rlm, leg, nvector, nscalar)
!!      subroutine init_legendre_symmetry(sph_rtm, sph_rlm, leg)
!!
!!      subroutine deallocate_hemi_schmidt_rtm
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
      module m_legendre_work_sym_matmul
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_schmidt_poly_on_rtm
!
      use m_machine_parameter
      use m_work_4_sph_trans
      use matmul_for_legendre_trans
!
      implicit none
!
!>        Number of meridional grid points in northern hemisphere
      integer(kind = kint) :: nth_hemi_rtm
!>        @$f P_{l}{m} @$f
!!        at gouss points in northen hemisphere
      real(kind = kreal), allocatable :: Ps_rtm(:,:)
!>        @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!        at gouss points in northen hemisphere
      real(kind = kreal), allocatable :: dPsdt_rtm(:,:)
!
!>        @$f P_{l}{m} @$f
!!        at gouss points in northen hemisphere
      real(kind = kreal), allocatable :: Ps_jl(:,:)
!>        @$f dP_{l}{m}/d\theta @$f  with even (l-m) 
!!        at gouss points in northen hemisphere
      real(kind = kreal), allocatable :: dPsdt_jl(:,:)
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
!>     Poloidal component with odd (l-m)
      real(kind = kreal), allocatable :: pol_o(:,:)
!>     radial difference of Poloidal component with odd (l-m)
      real(kind = kreal), allocatable :: dpoldt_o(:,:)
!>     radial difference of Poloidal component with odd (l-m)
      real(kind = kreal), allocatable :: dpoldp_o(:,:)
!>     Toroidal component with odd (l-m)
      real(kind = kreal), allocatable :: dtordt_o(:,:)
!>     Toroidal component with odd (l-m)
      real(kind = kreal), allocatable :: dtordp_o(:,:)
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
!>     Anti-symmetric radial component
      real(kind = kreal), allocatable :: asmp_r(:,:)
!>     Symmetric theta-component
      real(kind = kreal), allocatable :: symp_t(:,:)
!>     Symmetric phi-component
      real(kind = kreal), allocatable :: symp_p(:,:)
!>     Anti-symmetric theta-component with condugate order
      real(kind = kreal), allocatable :: asmn_t(:,:)
!>     Anti-symmetric phi-component with condugate order
      real(kind = kreal), allocatable :: asmn_p(:,:)
!
!>     Maximum matrix size for spectr data
      integer(kind = kint) :: nscl_jk
!>     Scalar with evem (l-m)
      real(kind = kreal), allocatable :: scl_e(:,:)
!>     Scalar with odd (l-m)
      real(kind = kreal), allocatable :: scl_o(:,:)
!
!>     Maximum matrix size for field data
      integer(kind = kint) :: nscl_lk
!>     Symmetric scalar component
      real(kind = kreal), allocatable :: symp(:,:)
!>     Anti-symmetric scalar component
      real(kind = kreal), allocatable :: asmp(:,:)
!
      private :: alloc_leg_vec_sym_matmul, dealloc_leg_vec_sym_matmul
      private :: alloc_leg_scl_sym_matmul, dealloc_leg_scl_sym_matmul
      private :: alloc_leg_vec_symmetry, alloc_leg_scl_symmetry
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_legendre_sym_matmul                               &
     &         (sph_rtm, sph_rlm, leg, nvector, nscalar)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      integer(kind = kint), intent(in) :: nvector, nscalar
!
!
      call const_symmetric_legendres(sph_rlm%nidx_rlm(2),             &
     &    sph_rtm%nidx_rtm(2), sph_rtm%nidx_rtm(3), leg)
!
      call alloc_leg_vec_sym_matmul                                   &
     &   (sph_rtm%nidx_rtm(2), sph_rtm%maxidx_rtm_smp(1), nvector)
      call alloc_leg_scl_sym_matmul                                   &
     &   (sph_rtm%nidx_rtm(2), sph_rtm%maxidx_rtm_smp(1), nscalar)
!
      end subroutine init_legendre_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine init_legendre_symmetry(sph_rtm, sph_rlm, leg)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
!
!
      call const_symmetric_legendres(sph_rlm%nidx_rlm(2),               &
     &    sph_rtm%nidx_rtm(2), sph_rtm%nidx_rtm(3), leg)
!
      call alloc_leg_vec_symmetry(sph_rtm%nidx_rtm(2))
      call alloc_leg_scl_symmetry(sph_rtm%nidx_rtm(2))
!
      end subroutine init_legendre_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine finalize_legendre_sym_matmul
!
!
      call dealloc_leg_vec_sym_matmul
      call dealloc_leg_scl_sym_matmul
      call deallocate_hemi_schmidt_rtm
!
      end subroutine finalize_legendre_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_symmetric_legendres                              &
     &         (jmax_rlm, nth_rtm, mphi_rtm, leg)
!
      use set_legendre_matrices
!
      integer(kind = kint), intent(in) :: nth_rtm, mphi_rtm, jmax_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
!
!
      call allocate_hemi_schmidt_rtm(nth_rtm, jmax_rlm)
!
      call set_symmetric_legendre_lj(nth_rtm, mphi_rtm,                 &
     &    jmax_rlm, nth_hemi_rtm, lstack_rlm, lstack_even_rlm,          &
     &    leg%P_rtm, leg%dPdt_rtm, Ps_rtm, dPsdt_rtm)
      call set_symmetric_legendre_jl(nth_rtm, mphi_rtm,                 &
     &    jmax_rlm, nth_hemi_rtm, lstack_rlm, lstack_even_rlm,          &
     &    leg%P_rtm, leg%dPdt_rtm, Ps_jl, dPsdt_jl)
!
      end subroutine const_symmetric_legendres
!
! -----------------------------------------------------------------------
!
      subroutine allocate_hemi_schmidt_rtm(nth_rtm, jmax_rlm)
!
      integer(kind = kint), intent(in) :: nth_rtm, jmax_rlm
!
!
      nth_hemi_rtm = (nth_rtm+1) / 2
      allocate( Ps_rtm(nth_hemi_rtm,jmax_rlm) )
      allocate( dPsdt_rtm(nth_hemi_rtm,jmax_rlm) )
!
      allocate( Ps_jl(jmax_rlm,nth_hemi_rtm) )
      allocate( dPsdt_jl(jmax_rlm,nth_hemi_rtm) )
!
      Ps_rtm =    0.0d0
      dPsdt_rtm = 0.0d0
!
      Ps_jl =    0.0d0
      dPsdt_jl = 0.0d0
!
      end subroutine allocate_hemi_schmidt_rtm
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_hemi_schmidt_rtm
!
      deallocate(Ps_rtm, dPsdt_rtm)
      deallocate(Ps_jl,  dPsdt_jl)
!
      end subroutine deallocate_hemi_schmidt_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_sym_matmul                               &
     &         (nth_rtm, maxidx_rtm_r_smp, nvector)
!
      integer(kind = kint), intent(in) :: nth_rtm
      integer(kind = kint), intent(in) :: maxidx_rtm_r_smp
      integer(kind = kint), intent(in) :: nvector
!
!
      nvec_jk = ((maxdegree_rlm+1)/2) * maxidx_rtm_r_smp * nvector
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpoldt_e(nvec_jk,np_smp))
      allocate(dpoldp_e(nvec_jk,np_smp))
      allocate(dtordt_e(nvec_jk,np_smp))
      allocate(dtordp_e(nvec_jk,np_smp))
      allocate(pol_o(nvec_jk,np_smp))
      allocate(dpoldt_o(nvec_jk,np_smp))
      allocate(dpoldp_o(nvec_jk,np_smp))
      allocate(dtordt_o(nvec_jk,np_smp))
      allocate(dtordp_o(nvec_jk,np_smp))
!
      nvec_lk = ((nth_rtm + 1)/2) * maxidx_rtm_r_smp * nvector
      allocate(symp_r(nvec_lk,np_smp))
      allocate(symp_t(nvec_lk,np_smp))
      allocate(symp_p(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
!
      allocate(asmp_r(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
      allocate(asmn_t(nvec_lk,np_smp))
      allocate(asmn_p(nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_sym_matmul                               &
     &         (nth_rtm, maxidx_rtm_r_smp, nscalar)
!
      integer(kind = kint), intent(in) :: nth_rtm
      integer(kind = kint), intent(in) :: maxidx_rtm_r_smp
      integer(kind = kint), intent(in) :: nscalar
!
!
      nscl_jk = ((maxdegree_rlm+1)/2) * maxidx_rtm_r_smp * nscalar
      allocate(scl_e(nscl_jk,np_smp))
      allocate(scl_o(nscl_jk,np_smp))
!
      nscl_lk = ((nth_rtm + 1)/2) * maxidx_rtm_r_smp * nscalar
      allocate(symp(nscl_lk,np_smp))
      allocate(asmp(nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_vec_sym_matmul
!
!
      deallocate(pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e)
      deallocate(pol_o, dpoldt_o, dpoldp_o, dtordt_o, dtordp_o)
!
      deallocate(symp_r, symp_t, symp_p, symn_t, symn_p)
      deallocate(asmp_r, asmp_t, asmp_p, asmn_t, asmn_p)
!
      end subroutine dealloc_leg_vec_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_leg_scl_sym_matmul
!
!
      deallocate(scl_e, scl_o)
      deallocate(symp, asmp)
!
      end subroutine dealloc_leg_scl_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_vec_symmetry(nth_rtm)
!
      integer(kind = kint), intent(in) :: nth_rtm
!
!
      nvec_jk = (maxdegree_rlm+1)/2
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpoldt_e(nvec_jk,np_smp))
      allocate(dpoldp_e(nvec_jk,np_smp))
      allocate(dtordt_e(nvec_jk,np_smp))
      allocate(dtordp_e(nvec_jk,np_smp))
      allocate(pol_o(nvec_jk,np_smp))
      allocate(dpoldt_o(nvec_jk,np_smp))
      allocate(dpoldp_o(nvec_jk,np_smp))
      allocate(dtordt_o(nvec_jk,np_smp))
      allocate(dtordp_o(nvec_jk,np_smp))
!
      nvec_lk = (nth_rtm + 1)/2
      allocate(symp_r(nvec_lk,np_smp))
      allocate(symp_t(nvec_lk,np_smp))
      allocate(symp_p(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
!
      allocate(asmp_r(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
      allocate(asmn_t(nvec_lk,np_smp))
      allocate(asmn_p(nvec_lk,np_smp))
!
      end subroutine alloc_leg_vec_symmetry
!
! -----------------------------------------------------------------------
!
      subroutine alloc_leg_scl_symmetry(nth_rtm)
!
      integer(kind = kint), intent(in) :: nth_rtm
!
!
      nscl_jk = (maxdegree_rlm+1)/2
      allocate(scl_e(nscl_jk,np_smp))
      allocate(scl_o(nscl_jk,np_smp))
!
      nscl_lk = (nth_rtm + 1)/2
      allocate(symp(nscl_lk,np_smp))
      allocate(asmp(nscl_lk,np_smp))
!
      end subroutine alloc_leg_scl_symmetry
!
! -----------------------------------------------------------------------
!
      end module m_legendre_work_sym_matmul
