!>@file   const_global_sph_grids_modes.f90
!!@brief  module const_global_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set global spherical harmonics ID
!!
!!@verbatim
!!      subroutine s_const_global_sph_grids_modes                       &
!!     &         (sph_params, sph_rtp, sph_rtm, sph_rj)
!!      subroutine const_global_sph_FEM_grid(sph_params, sph_rtp, sph_rj)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!      subroutine check_nidx_local(ip_rank)
!!@endverbatim
!!
!!@param ip_rank process ID
!
      module const_global_sph_grids_modes
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      use t_spheric_parameter
!
      use set_global_spherical_param
      use set_indices_4_sph_tranform
!
      implicit none
!
      integer(kind = kint), allocatable :: nidx_local_rtp_r(:)
      integer(kind = kint), allocatable :: nidx_local_rtp_t(:)
      integer(kind = kint), allocatable :: nidx_local_rtp_p(:)
!
      integer(kind = kint), allocatable :: nidx_local_rtm_r(:)
      integer(kind = kint), allocatable :: nidx_local_rtm_t(:)
      integer(kind = kint), allocatable :: nidx_local_rtm_m(:)
!
      integer(kind = kint), allocatable :: nidx_local_rlm_r(:)
      integer(kind = kint), allocatable :: nidx_local_rlm_j(:)
!
      integer(kind = kint), allocatable :: nidx_local_rj_r(:)
      integer(kind = kint), allocatable :: nidx_local_rj_j(:)
!
!      private :: nidx_local_rtp_r, nidx_local_rtp_t, nidx_local_rtp_p
!      private :: nidx_local_rtm_r, nidx_local_rtm_t, nidx_local_rtm_m
!      private :: nidx_local_rlm_r, nidx_local_rlm_j
!      private :: nidx_local_rj_r, nidx_local_rj_j
!
!      private :: const_global_rtp_grids, const_global_rtm_grids
!      private :: const_global_rj_modes_by_rlm, const_global_rlm_modes
!      private :: allocate_nidx_local, deallocate_nidx_local
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_global_sph_grids_modes                         &
     &         (sph_params, sph_rtp, sph_rtm, sph_rj)
!
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use m_sph_1d_global_index
      use m_2d_sph_trans_table
!
      use set_sph_1d_global_index
      use set_sph_1d_domain_id
      use set_sph_tranform_ordering
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      call allocate_sph_1d_global_stack
      call allocate_sph_gl_parameter
      call allocate_sph_gl_bc_param
      call allocate_sph_ranks
      call allocate_nidx_local
!
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rtp_grids'
      call const_global_rtp_grids(sph_params, sph_rtp)
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rtm_grids'
      call const_global_rtm_grids(sph_params, sph_rtm)
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rlm_modes'
      call const_global_rlm_modes(sph_params, sph_rtp, sph_rj)
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rj_modes_by_rlm'
      call const_global_rj_modes_by_rlm(sph_rj)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_trans_table_fft_2_lgd'
      call set_trans_table_fft_2_lgd(sph_params%l_truncation,           &
     &    sph_rtp%nidx_global_rtp(2), sph_rtp%nidx_global_rtp(3),       &
     &    sph_params%m_folding, mspec_4_ispack,                         &
     &    jdx_fsph, mtbl_fft_2_lgd)
!
!
      call deallocate_nidx_local
      call allocate_sph_1d_global_idx
!
      call set_sph_1d_global_idx_rtp                                    &
     &   (sph_params%m_folding, sph_rtp%nidx_global_rtp(3),             &
     &    mdx_ispack)
      call set_sph_1d_global_idx_rtm                                    &
     &   (sph_params%m_folding, sph_rtp%nidx_global_rtp(3),             &
     &    mtbl_fft_2_lgd, mdx_4_lgd)
      call set_sph_1d_global_idx_rlm                                    &
     &   (sph_rj%nidx_global_rj(2), jtbl_fsph)
      call set_sph_1d_global_idx_rj(sph_rj%nidx_global_rj(2), jtbl_rj)
!
      call allocate_sph_1d_domain_id(sph_rtp, sph_rj)
!
      call set_sph_1d_domain_id_rtp
      call set_sph_1d_domain_id_rj
!
      if(iflag_debug .gt. 0) then
        write(50,*) 'idx_global_rtp_r', idx_global_rtp_r
        write(50,*) 'idx_global_rtm_r', idx_global_rtm_r
        write(50,*) 'idx_global_rlm_r', idx_global_rlm_r
!        call check_sph_1d_domain_id(sph_rtp, sph_rj)
      end if
!
      end subroutine s_const_global_sph_grids_modes
!
! -----------------------------------------------------------------------
!
      subroutine const_global_sph_FEM_grid(sph_params, sph_rtp, sph_rj)
!
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use m_sph_1d_global_index
      use m_2d_sph_trans_table
!
      use set_sph_1d_global_index
      use set_sph_1d_domain_id
      use set_sph_tranform_ordering
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      call allocate_sph_1d_global_stack
      call allocate_sph_gl_parameter
      call allocate_sph_gl_bc_param
      call allocate_sph_ranks
      call allocate_nidx_local
!
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rtp_grids'
      call const_global_rtp_grids(sph_params, sph_rtp)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_trans_table_fft_2_lgd'
      call set_trans_table_fft_2_lgd(sph_params%l_truncation,           &
     &    sph_rtp%nidx_global_rtp(2), sph_rtp%nidx_global_rtp(3),       &
     &    sph_params%m_folding, mspec_4_ispack,                         &
     &    jdx_fsph, mtbl_fft_2_lgd)
!
      call deallocate_nidx_local
      call allocate_sph_1d_global_idx
!
      call set_sph_1d_global_idx_rtp                                    &
     &   (sph_params%m_folding, sph_rtp%nidx_global_rtp(3), mdx_ispack)
!
      call allocate_sph_1d_domain_id(sph_rtp, sph_rj)
!
      call set_sph_1d_domain_id_rtp
!
      if(iflag_debug .gt. 0) then
        write(50,*) 'idx_global_rtp_r', idx_global_rtp_r
      end if
!
      end subroutine const_global_sph_FEM_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_nidx_local
!
      use m_spheric_global_ranks
!
      integer(kind = kint) :: num
!
      num = ndomain_rtp(1)
      allocate(nidx_local_rtp_r(num))
      num = ndomain_rtp(2)
      allocate(nidx_local_rtp_t(num))
      num = ndomain_rtp(3)
      allocate(nidx_local_rtp_p(num))
!
      num = ndomain_rtm(1)
      allocate(nidx_local_rtm_r(num))
      num = ndomain_rtm(2)
      allocate(nidx_local_rtm_t(num))
      num = ndomain_rtm(3)
      allocate(nidx_local_rtm_m(num))
!
      num = ndomain_rlm(1)
      allocate(nidx_local_rlm_r(num))
      num = ndomain_rlm(2)
      allocate(nidx_local_rlm_j(num))
!
      num = ndomain_rj(1)
      allocate(nidx_local_rj_r(num))
      num = ndomain_rj(2)
      allocate(nidx_local_rj_j(num))
!
      nidx_local_rtp_r = 0
      nidx_local_rtp_t = 0
      nidx_local_rtp_p = 0
      nidx_local_rtm_r = 0
      nidx_local_rtm_t = 0
      nidx_local_rtm_m = 0
      nidx_local_rlm_r = 0
      nidx_local_rlm_j = 0
      nidx_local_rj_r =  0
      nidx_local_rj_j =  0
!
      end subroutine allocate_nidx_local
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nidx_local
!
!
      deallocate(nidx_local_rtp_r, nidx_local_rtp_t, nidx_local_rtp_p)
      deallocate(nidx_local_rtm_r, nidx_local_rtm_t, nidx_local_rtm_m)
      deallocate(nidx_local_rlm_r, nidx_local_rlm_j)
      deallocate(nidx_local_rj_r, nidx_local_rj_j)
!
      end subroutine deallocate_nidx_local
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_nidx_local(ip_rank)
!
      integer(kind = kint), intent(in) :: ip_rank
!
!
      write(ip_rank+50,*) 'nidx_local_rtp_r', nidx_local_rtp_r
      write(ip_rank+50,*) 'nidx_local_rtp_t', nidx_local_rtp_t
      write(ip_rank+50,*) 'nidx_local_rtp_p', nidx_local_rtp_p
!
      write(ip_rank+50,*) 'nidx_local_rtm_r', nidx_local_rtm_r
      write(ip_rank+50,*) 'nidx_local_rtm_t', nidx_local_rtm_t
      write(ip_rank+50,*) 'nidx_local_rtm_m', nidx_local_rtm_m
!
      write(ip_rank+50,*) 'nidx_local_rlm_r', nidx_local_rlm_r
      write(ip_rank+50,*) 'nidx_local_rlm_j', nidx_local_rlm_j
!
      write(ip_rank+50,*) 'nidx_local_rj_r', nidx_local_rj_r
      write(ip_rank+50,*) 'nidx_local_rj_j', nidx_local_rj_j
!
      end subroutine check_nidx_local
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_global_rtp_grids(sph_params, sph_rtp)
!
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use m_sph_1d_global_index
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint) :: ist, ied
!
!
!      write(*,*) 'set_gl_rank_3d', ndomain_rtp(1:3)
      call set_gl_rank_3d(iflag_radial_inner_domain,                    &
     &    ndomain_sph, ndomain_rtp, iglobal_rank_rtp)
!
!      call cal_local_nums                                              &
!     &   (ndomain_rtp(1), ione, sph_rtp%nidx_global_rtp(1),            &
!     &    nidx_local_rtp_r, istack_idx_local_rtp_r)
!      write(*,*) 'cal_local_nums 1',                                   &
!     &     ndomain_rtp(1:3), sph_rtp%nidx_global_rtp(2)
      call cal_local_nums                                               &
     &   (ndomain_rtp(2), ione, sph_rtp%nidx_global_rtp(2),             &
     &    nidx_local_rtp_t, istack_idx_local_rtp_t)
!      write(*,*) 'cal_local_nums 2'
      call cal_local_nums                                               &
     &   (ndomain_rtp(3), ione, sph_rtp%nidx_global_rtp(3),             &
     &    nidx_local_rtp_p, istack_idx_local_rtp_p)
!
!
!      write(*,*) 'cal_local_nums_st'
      call cal_local_nums_st(ndomain_rtp(1),                            &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    nidx_local_rtp_OC, ist_idx_local_rtp_OC)
!
      if (sph_params%nlayer_ICB .gt. 1) then
        ied = sph_params%nlayer_ICB - 1
!      write(*,*) 'cal_local_nums_rev'
        call cal_local_nums_rev(ndomain_rtp(1), ione, ied,              &
     &      nidx_local_rtp_IC, ist_idx_local_rtp_IC)
      end if
!
      if (sph_params%nlayer_CMB .lt. sph_rtp%nidx_global_rtp(1)) then
        ist = sph_params%nlayer_CMB + 1
!      write(*,*) 'cal_local_nums_rev'
        call cal_local_nums_rev(ndomain_rtp(1), ist,                    &
     &      sph_rtp%nidx_global_rtp(1), nidx_local_rtp_MT,              &
     &      ist_idx_local_rtp_MT)
      end if
!
!      write(*,*) 'merge_num_3_local_layers'
      call merge_num_3_local_layers(ndomain_rtp(1),                     &
     &    nidx_local_rtp_OC, nidx_local_rtp_IC, nidx_local_rtp_MT,      &
     &    ione, nidx_local_rtp_r, istack_idx_local_rtp_r)
!
!
!      write(*,*) 'set_gl_nnod_spherical'
      call set_gl_nnod_spherical(ndomain_sph,                           &
     &    ndomain_rtp(1), ndomain_rtp(2), ndomain_rtp(3),               &
     &    iglobal_rank_rtp, nidx_local_rtp_r, nidx_local_rtp_t,         &
     &    nidx_local_rtp_p, nidx_local_rtp, nnod_local_rtp)
!
      end subroutine const_global_rtp_grids
!
! -----------------------------------------------------------------------
!
      subroutine const_global_rtm_grids(sph_params, sph_rtm)
!
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use m_sph_1d_global_index
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(in) :: sph_rtm
!
      integer(kind = kint) :: ist, ied
!
!
      call set_gl_rank_3d(iflag_radial_inner_domain,                    &
     &    ndomain_sph, ndomain_rtm, iglobal_rank_rtm)
!
!      call cal_local_nums                                              &
!     &   (ndomain_rtm(1), ione, sph_rtm%nidx_global_rtm(1),            &
!     &    nidx_local_rtm_r, istack_idx_local_rtm_r)
      call cal_local_nums                                               &
     &   (ndomain_rtm(2), ione, sph_rtm%nidx_global_rtm(2),             &
     &    nidx_local_rtm_t, istack_idx_local_rtm_t)
      call cal_local_num_rtm_m(ndomain_rtm(3), sph_params%l_truncation, &
     &    sph_params%m_folding, nidx_local_rtm_m,                       &
     &    istack_idx_local_rtm_m)
!
      call cal_local_nums_st(ndomain_rtm(1),                            &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    nidx_local_rtm_OC, ist_idx_local_rtm_OC)
!
      if (sph_params%nlayer_ICB .gt. 1) then
        ied = sph_params%nlayer_ICB - 1
        call cal_local_nums_rev(ndomain_rtm(1), ione, ied,              &
     &      nidx_local_rtm_IC, ist_idx_local_rtm_IC)
      end if
!
      if (sph_params%nlayer_CMB .lt. sph_rtm%nidx_global_rtm(1)) then
        ist = sph_params%nlayer_CMB + 1
        call cal_local_nums_rev(ndomain_rtm(1), ist,                    &
     &      sph_rtm%nidx_global_rtm(1), nidx_local_rtm_MT,              &
     &      ist_idx_local_rtm_MT)
      end if
!
      call merge_num_3_local_layers(ndomain_rtm(1),                     &
     &    nidx_local_rtm_OC, nidx_local_rtm_IC, nidx_local_rtm_MT,      &
     &    ione, nidx_local_rtm_r, istack_idx_local_rtm_r)
!
      call set_gl_nnod_spherical(ndomain_sph,                           &
     &    ndomain_rtm(1), ndomain_rtm(2), ndomain_rtm(3),               &
     &    iglobal_rank_rtm, nidx_local_rtm_r, nidx_local_rtm_t,         &
     &    nidx_local_rtm_m, nidx_local_rtm, nnod_local_rtm)
!
!      call check_sph_gl_bc_param(izero)
!
      end subroutine const_global_rtm_grids
!
! -----------------------------------------------------------------------
!
      subroutine const_global_rj_modes_by_rlm(sph_rj)
!
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use m_sph_1d_global_index
      use m_2d_sph_trans_table
      use set_sph_tranform_ordering
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      call set_gl_rank_2d(iflag_radial_inner_domain,                    &
     &    ndomain_sph, ndomain_rj,  iglobal_rank_rj)
!
      call cal_local_nums                                               &
     &   (ndomain_rj(1), ione, sph_rj%nidx_global_rj(1),                &
     &    nidx_local_rj_r, istack_idx_local_rj_r)
! 
      call set_merged_index_4_sph_rj(ndomain_rtm(1), ndomain_rtm(3),    &
     &    ndomain_rj(2), sph_rj%nidx_global_rj(2),                      &
     &    istack_idx_local_rlm_j, jtbl_fsph, nidx_local_rj_j,           &
     &    istack_idx_local_rj_j, jtbl_rj)
!
      call set_gl_nnod_spheric_rj(ndomain_sph,                          &
     &    ndomain_rj(1), ndomain_rj(2),                                 &
     &    iglobal_rank_rj, nidx_local_rj_r, nidx_local_rj_j,            &
     &    nidx_local_rj, nnod_local_rj)
!
      end subroutine const_global_rj_modes_by_rlm
!
! -----------------------------------------------------------------------
!
      subroutine const_global_rlm_modes(sph_params, sph_rtp, sph_rj)
!
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use m_sph_1d_global_index
      use m_2d_sph_trans_table
      use set_sph_tranform_ordering
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      call allocate_2d_sph_trans_table(sph_rtp%nidx_global_rtp(2),      &
     &    sph_rtp%nidx_global_rtp(3), sph_rj%nidx_global_rj(2))
!
      call set_gl_rank_2d(iflag_radial_inner_domain,                    &
     &    ndomain_sph, ndomain_rlm, iglobal_rank_rlm)
!
      nidx_local_rlm_r(1:ndomain_rlm(1))                                &
     &      = nidx_local_rtm_r(1:ndomain_rlm(1))
      istack_idx_local_rlm_r(0:ndomain_rlm(1))                          &
     &      = istack_idx_local_rtm_r(0:ndomain_rlm(1))
!
!
      call set_wavenumber_4_ispack_fft(sph_rtp%nidx_global_rtp(2),      &
     &    sph_rtp%nidx_global_rtp(3), sph_params%m_folding,             &
     &    mspec_4_ispack, mdx_ispack)
!
      call set_zonal_wavenum_4_legendre(ndomain_rtm(3),                 &
     &    sph_params%l_truncation, sph_params%m_folding,                &
     &    sph_rtp%nidx_global_rtp(2), sph_rtp%nidx_global_rtp(3),       &
     &    jdx_fsph, mdx_4_lgd)
!
      call set_merged_index_4_sph_trans(ndomain_rtm(3),                 &
     &    sph_params%l_truncation, sph_rj%nidx_global_rj(2),            &
     &    sph_rtp%nidx_global_rtp(3), sph_params%m_folding,             &
     &    istack_idx_local_rtm_m, mdx_4_lgd, nidx_local_rlm_j,          &
     &    istack_idx_local_rlm_j, jtbl_fsph)
!
      call set_gl_nnod_spheric_rj(ndomain_sph,                          &
     &    ndomain_rlm(1), ndomain_rlm(2),                               &
     &    iglobal_rank_rlm, nidx_local_rlm_r, nidx_local_rlm_j,         &
     &    nidx_local_rlm, nnod_local_rlm)
!
      end subroutine const_global_rlm_modes
!
! -----------------------------------------------------------------------
!
      end module const_global_sph_grids_modes
