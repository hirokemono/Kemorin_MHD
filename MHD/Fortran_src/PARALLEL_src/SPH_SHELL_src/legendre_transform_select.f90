!>@file   legendre_transform_select.f90
!!@brief  module legendre_transform_select
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transform selector
!!
!!
!!@verbatim
!!      subroutine set_legendre_trans_mode_ctl(tranx_loop_ctl)
!!      subroutine sel_init_legendre_trans(ncomp, nvector, nscalar,     &
!!     &          sph_rtm, sph_rlm, leg, idx_trns)
!!      subroutine sel_finalize_legendre_trans
!!
!!    Backward transforms
!!      subroutine sel_backward_legendre_trans                          &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine sel_forward_legendre_trans                           &
!!     &         (ncomp, nvector, nscalar,                              &
!!     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,  &
!!     &          n_WR, n_WS, WR, WS)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_select
!
      use m_precision
!
      use m_work_4_sph_trans_spin
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_legendre_work_sym_matmul
      use t_leg_trans_sym_matmul_big
!
      use legendre_transform_org
      use legendre_transform_krin
      use legendre_transform_spin
      use legendre_transform_testloop
      use legendre_transform_matmul
      use legendre_trans_sym_matmul
      use legendre_trans_matmul_big
!
      implicit none
!
      type(leg_trns_sym_mul_work), save :: WK1_l_sml
      type(leg_trns_bsym_mul_work), save :: WK1_l_bsm
!
      integer(kind = kint), parameter :: ntype_Leg_trans_loop = 15
!
!>      Character flag to perform Legendre transform 
!@n     using original array order
      character(len = kchara), parameter                                &
     &           :: leg_orginal_loop = 'original_loop'
!>      Character flag to perform Legendre transform 
!@n     using blocked loop
      character(len = kchara), parameter                                &
     &           :: leg_blocked_loop = 'blocked_loop'
!>      Character flag to perform Legendre transform 
!!@n    using longer loop for original array order 
      character(len = kchara), parameter                                &
     &           :: leg_krloop_inner = 'inner_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      character(len = kchara), parameter                                &
     &           :: leg_krloop_outer = 'outer_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with symmetry
      character(len = kchara), parameter                                &
     &           :: leg_sym_org_loop =   'symmetric_original_loop'
!>      Character flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      character(len = kchara), parameter                                &
     &           :: leg_sym_spin_loop = 'symmetric_outer_radial_loop'
!>      Character flag to perform Legendre transform 
!@n     with mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_matmul = 'matmul'
!>      Character flag to perform Legendre transform 
!@n     with dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_dgemm = 'BLAS'
!>      Character flag to perform Legendre transform 
!@n     with self matrix product
      character(len = kchara), parameter                                &
     &           :: leg_matprod = 'matproduct'
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_matmul =  'symmetric_matmul'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_sym_dgemm =   'symmetric_BLAS'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      character(len = kchara), parameter                                &
     &           :: leg_sym_matprod = 'symmetric_matproduct'
!>      Chalacter flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      character(len = kchara), parameter                                &
     &           :: leg_sym_matmul_big =  'symmetric_matmul_big'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      character(len = kchara), parameter                                &
     &           :: leg_sym_dgemm_big =   'symmetric_BLAS_big'
!>      Character flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      character(len = kchara), parameter                                &
     &           :: leg_sym_matprod_big = 'symmetric_matproduct_big'
!>      Character flag to perform Legendre transform 
!@n     with testing loop
      character(len = kchara), parameter                                &
     &           :: leg_test_loop =    'test_loop'
!
!
!>      integer flag to run elpse time check for legendre transform
      integer(kind = kint), parameter :: iflag_leg_undefined = -1
!>      integer flag to perform Legendre transform 
!@n     using original array order
      integer(kind = kint), parameter :: iflag_leg_orginal_loop = 1
!>      integer flag to perform Legendre transform 
!@n     using blocked loop
      integer(kind = kint), parameter :: iflag_leg_blocked =      2
!>      integer flag to perform Legendre transform 
!!@n    using longer loop for original array order 
      integer(kind = kint), parameter :: iflag_leg_krloop_inner = 3
!>      integer flag to perform Legendre transform 
!@n     with inneromst Legendre polynomial loop
      integer(kind = kint), parameter :: iflag_leg_krloop_outer = 4
!>      integer flag to perform Legendre transform with symmetry
      integer(kind = kint), parameter :: iflag_leg_symmetry =     5
!>      integer flag to perform Legendre transform 
!@n     with symmetry and inneromst Legendre polynomial loop
      integer(kind = kint), parameter :: iflag_leg_sym_spin_loop = 6
!>      integer flag to perform Legendre transform 
!@n     with mutmul function
      integer(kind = kint), parameter :: iflag_leg_matmul =        7
!>      integer flag to perform Legendre transform 
!@n     with dgemm in BLAS
      integer(kind = kint), parameter :: iflag_leg_dgemm =         8
!>      integer flag to perform Legendre transform 
!@n     with self matrix product
      integer(kind = kint), parameter :: iflag_leg_matprod =       9
!>      integer flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      integer(kind = kint), parameter :: iflag_leg_sym_matmul =   10
!>      integer flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm =    11
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_matprod =  12
!>      integer flag to perform Legendre transform 
!@n     with symmetry and mutmul function
      integer(kind = kint), parameter :: iflag_leg_sym_matmul_big =  13
!>      integer flag to perform Legendre transform 
!@n     with symmetry and dgemm in BLAS
      integer(kind = kint), parameter :: iflag_leg_sym_dgemm_big =   14
!>      integer flag to perform Legendre transform 
!@n     with symmetry and  self matrix product
      integer(kind = kint), parameter :: iflag_leg_sym_matprod_big = 15
!>      integer flag to perform Legendre transform 
!@n     with testing loop
      integer(kind = kint), parameter :: iflag_leg_test_loop =   99
!
!>      Integer flag for Legendre transform
      integer(kind = kint)                                              &
     &              :: id_legendre_transfer = iflag_leg_undefined
!
!>      vector length for legendre transform
      integer(kind = kint) :: nvector_legendre = 0
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_legendre_trans_mode_ctl(tranx_loop_ctl)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: tranx_loop_ctl
!
!
      if(     cmp_no_case(tranx_loop_ctl, leg_test_loop)) then
        id_legendre_transfer = iflag_leg_test_loop
      else if(cmp_no_case(tranx_loop_ctl, leg_krloop_inner)) then
        id_legendre_transfer = iflag_leg_krloop_inner
      else if(cmp_no_case(tranx_loop_ctl, leg_krloop_outer)) then
        id_legendre_transfer = iflag_leg_krloop_outer
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_org_loop)) then
        id_legendre_transfer = iflag_leg_symmetry
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_spin_loop)) then
        id_legendre_transfer = iflag_leg_sym_spin_loop
      else if(cmp_no_case(tranx_loop_ctl, leg_matmul)) then
        id_legendre_transfer = iflag_leg_matmul
      else if(cmp_no_case(tranx_loop_ctl, leg_dgemm)) then
        id_legendre_transfer = iflag_leg_dgemm
      else if(cmp_no_case(tranx_loop_ctl, leg_matprod)) then
        id_legendre_transfer = iflag_leg_matprod
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_matmul)) then
        id_legendre_transfer = iflag_leg_sym_matmul
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_dgemm)) then
        id_legendre_transfer = iflag_leg_sym_dgemm
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_matprod)) then
        id_legendre_transfer = iflag_leg_sym_matprod
      else if(cmp_no_case(tranx_loop_ctl, leg_blocked_loop)) then
        id_legendre_transfer = iflag_leg_blocked
      else if(cmp_no_case(tranx_loop_ctl, leg_orginal_loop)) then
        id_legendre_transfer = iflag_leg_orginal_loop
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_matmul_big)) then
        id_legendre_transfer = iflag_leg_sym_matmul_big
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_dgemm_big)) then
        id_legendre_transfer = iflag_leg_sym_dgemm_big
      else if(cmp_no_case(tranx_loop_ctl, leg_sym_matprod_big)) then
        id_legendre_transfer = iflag_leg_sym_matprod_big
      else
        id_legendre_transfer = iflag_leg_orginal_loop
      end if
!
      end subroutine set_legendre_trans_mode_ctl
!
! -----------------------------------------------------------------------
!
      subroutine sel_init_legendre_trans(ncomp, nvector, nscalar,       &
     &          sph_rtm, sph_rlm, leg, idx_trns)
!
      use m_legendre_work_matmul
      use m_legendre_work_testlooop
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      if     (id_legendre_transfer .eq. iflag_leg_sym_matmul            &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_dgemm             &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_matprod) then
        call init_legendre_sym_matmul(sph_rtm, sph_rlm, leg,            &
     &      idx_trns, nvector, nscalar, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matmul_big        &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_dgemm_big         &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_matprod_big) then
        call init_leg_sym_matmul_big(sph_rtm, sph_rlm, leg,             &
     &      idx_trns, nvector, nscalar, WK1_l_bsm)
      else if(id_legendre_transfer .eq. iflag_leg_matmul                &
     &   .or. id_legendre_transfer .eq. iflag_leg_dgemm                 &
     &   .or. id_legendre_transfer .eq. iflag_leg_matprod) then
        call alloc_leg_vec_matmul                                       &
     &     (sph_rtm%nidx_rtm(2), sph_rtm%maxidx_rtm_smp(1),             &
     &      nvector, idx_trns)
        call alloc_leg_scl_matmul                                       &
     &     (sph_rtm%nidx_rtm(2), sph_rtm%maxidx_rtm_smp(1),             &
     &      nscalar, idx_trns)
      else if(id_legendre_transfer .eq. iflag_leg_symmetry              &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_spin_loop) then
        call init_legendre_symmetry                                     &
     &     (sph_rtm, sph_rlm, leg, idx_trns, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_blocked               &
     &   .or. id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call alloc_leg_vec_blocked(sph_rtm%nidx_rtm(2), idx_trns)
        call alloc_leg_scl_blocked(sph_rtm%nidx_rtm(2), idx_trns)
      else if(id_legendre_transfer .eq. iflag_leg_test_loop) then
        call init_legendre_testloop                                     &
     &     (sph_rtm, sph_rlm, leg, idx_trns, nvector, nscalar)
      else
        call allocate_work_sph_trans                                    &
     &     (ncomp, sph_rtm%nnod_rtm, sph_rlm%nnod_rlm)
      end if
!
      end subroutine sel_init_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine sel_finalize_legendre_trans
!
      use m_legendre_work_matmul
      use m_legendre_work_testlooop
!
!
      if     (id_legendre_transfer .eq. iflag_leg_sym_matmul            &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_dgemm             &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_matprod           &
     &   .or. id_legendre_transfer .eq. iflag_leg_symmetry              &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_spin_loop) then
        call finalize_legendre_sym_matmul(WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matmul_big        &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_dgemm_big         &
     &   .or. id_legendre_transfer .eq. iflag_leg_sym_matprod_big) then
        call dealloc_leg_sym_matmul_big(WK1_l_bsm)
      else if(id_legendre_transfer .eq. iflag_leg_matmul                &
     &   .or. id_legendre_transfer .eq. iflag_leg_dgemm                 &
     &   .or. id_legendre_transfer .eq. iflag_leg_matprod) then
        call dealloc_leg_vec_matmul
      else if(id_legendre_transfer .eq. iflag_leg_blocked               &
     &   .or. id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call dealloc_leg_vec_matmul
      else if(id_legendre_transfer .eq. iflag_leg_test_loop) then
        call dealloc_leg_vec_test
      else
        call deallocate_work_sph_trans
      end if
!
      end subroutine sel_finalize_legendre_trans
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_backward_legendre_trans                            &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm, comm_rtm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      if(ncomp .le. 0) return
      if(id_legendre_transfer .eq. iflag_leg_test_loop) then
        call leg_backward_trans_test(ncomp, nvector, nscalar,           &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call leg_backward_trans_spin(ncomp, nvector, nscalar,           &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call leg_bwd_trans_fields_krin(ncomp, nvector, nscalar,         &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_symmetry) then
        call leg_backward_trans_sym_org(ncomp, nvector, nscalar,        &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_sym_spin_loop) then
        call leg_backward_trans_sym_spin(ncomp, nvector, nscalar,       &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_matmul) then
        call leg_backward_trans_matmul(ncomp, nvector, nscalar,         &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_dgemm) then
        call leg_backward_trans_dgemm(ncomp, nvector, nscalar,          &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_matprod) then
        call leg_backward_trans_matprod(ncomp, nvector, nscalar,        &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matmul) then
        call leg_backward_trans_sym_matmul(ncomp, nvector, nscalar,     &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_sym_dgemm) then
        call leg_backward_trans_sym_dgemm(ncomp, nvector, nscalar,      &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matprod) then
        call leg_backward_trans_sym_matprod(ncomp, nvector, nscalar,    &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_blocked) then
        call leg_backward_trans_blocked(ncomp, nvector, nscalar,        &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matmul_big) then
        call leg_backward_trans_matmul_big(ncomp, nvector, nscalar,     &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_bsm)
      else if(id_legendre_transfer .eq. iflag_leg_sym_dgemm_big) then
        call leg_backward_trans_dgemm_big(ncomp, nvector, nscalar,      &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_bsm)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matprod_big) then
        call leg_backward_trans_matprod_big(ncomp, nvector, nscalar,    &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_bsm)
      else
        call leg_backward_trans_org(ncomp, nvector, nscalar,            &
     &      sph_rlm, sph_rtm, comm_rlm, comm_rtm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      end if
!
      end subroutine sel_backward_legendre_trans
!
! -----------------------------------------------------------------------
!
      subroutine sel_forward_legendre_trans                             &
     &         (ncomp, nvector, nscalar,                                &
     &          sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,    &
     &          n_WR, n_WS, WR, WS)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rtm, comm_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
      type(index_4_sph_trans), intent(in) :: idx_trns
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR, n_WS
      real (kind=kreal), intent(inout):: WR(n_WR)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
!
      if(ncomp .le. 0) return
      if(id_legendre_transfer .eq. iflag_leg_test_loop) then
        call leg_forward_trans_test(ncomp, nvector, nscalar,            &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
        call leg_forward_trans_spin(ncomp, nvector, nscalar,            &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
        call leg_fwd_trans_fields_krin(ncomp, nvector, nscalar,         &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_symmetry) then
        call leg_forward_trans_sym_org(ncomp, nvector, nscalar,         &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_sym_spin_loop) then
        call leg_forward_trans_sym_spin(ncomp, nvector, nscalar,        &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_matmul) then
        call leg_forward_trans_matmul(ncomp, nvector, nscalar,          &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_dgemm) then
        call leg_forward_trans_dgemm(ncomp, nvector, nscalar,           &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_matprod) then
        call leg_forward_trans_matprod(ncomp, nvector, nscalar,         &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matmul) then
        call leg_forward_trans_sym_matmul(ncomp, nvector, nscalar,      &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_sym_dgemm) then
        call leg_forward_trans_sym_dgemm(ncomp, nvector, nscalar,       &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matprod) then
        call leg_forward_trans_sym_matprod(ncomp, nvector, nscalar,     &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_sml)
      else if(id_legendre_transfer .eq. iflag_leg_blocked) then
        call leg_forwawd_trans_blocked(ncomp, nvector, nscalar,         &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matmul_big) then
        call leg_forward_trans_matmul_big(ncomp, nvector, nscalar,      &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_bsm)
      else if(id_legendre_transfer .eq. iflag_leg_sym_dgemm_big) then
        call leg_forward_trans_dgemm_big(ncomp, nvector, nscalar,       &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_bsm)
      else if(id_legendre_transfer .eq. iflag_leg_sym_matprod_big) then
        call leg_forward_trans_matprod_big(ncomp, nvector, nscalar,     &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS, WK1_l_bsm)
      else
        call leg_forwawd_trans_org(ncomp, nvector, nscalar,             &
     &      sph_rtm, sph_rlm, comm_rtm, comm_rlm, leg, idx_trns,        &
     &      n_WR, n_WS, WR, WS)
      end if
!
      end subroutine sel_forward_legendre_trans
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_select
