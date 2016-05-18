!>@file   m_addresses_trans_sph_snap.f90
!!@brief  module m_addresses_trans_sph_snap
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine allocate_snap_trans_rtp(nnod_rtp)
!!      subroutine deallocate_snap_trans_rtp
!!@endverbatim
!
      module m_addresses_trans_sph_snap
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
!>      strucutre for spherical transform data addresses
      type(address_4_sph_trans), save :: trns_snap
!
!>      Nonoliear terms data using simulation
      real(kind = kreal), allocatable :: frm_rtp(:,:)
!
!
!>      field data to evaluate nonliear terms at pole
      real(kind = kreal), allocatable :: fls_pl(:,:)
!>      local field data to evaluate nonliear terms at pole
      real(kind = kreal), allocatable :: flc_pl(:,:)
!
!>      field data to evaluate nonliear terms at pole
      real(kind = kreal), allocatable :: frs_pl(:,:)
!>      field data to evaluate nonliear terms at pole
      real(kind = kreal), allocatable :: frm_pl(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_snap_trans_rtp(nnod_rtp)
!
      use m_work_pole_sph_trans
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: nnod_rtp
!
!
      call alloc_nonlinear_data(nnod_rtp, trns_snap)
!
      allocate(frm_rtp(nnod_rtp,trns_MHD%ncomp_rtp_2_rj))
      if(trns_MHD%ncomp_rtp_2_rj .gt. 0) frm_rtp = 0.0d0
!
!
      allocate(fls_pl(nnod_pole,trns_snap%ncomp_rj_2_rtp))
      allocate(flc_pl(nnod_pole,trns_snap%ncomp_rj_2_rtp))
      allocate(frs_pl(nnod_pole,trns_snap%ncomp_rtp_2_rj))
      allocate(frm_pl(nnod_pole,trns_MHD%ncomp_rtp_2_rj))
!
      if(trns_snap%ncomp_rj_2_rtp .gt. 0) fls_pl = 0.0d0
      if(trns_snap%ncomp_rj_2_rtp .gt. 0) flc_pl = 0.0d0
      if(trns_snap%ncomp_rtp_2_rj .gt. 0) frs_pl = 0.0d0
      if(trns_MHD%ncomp_rtp_2_rj .gt. 0) frm_pl = 0.0d0
!
      end subroutine allocate_snap_trans_rtp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_snap_trans_rtp
!
      call dealloc_nonlinear_data(trns_snap)
      deallocate(frm_rtp, fls_pl, flc_pl, frs_pl, frm_pl)
!
      end subroutine deallocate_snap_trans_rtp
!
!-----------------------------------------------------------------------
!
      end module m_addresses_trans_sph_snap
