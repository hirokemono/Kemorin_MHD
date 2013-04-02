!m_SGS_address.f90
!     module m_SGS_address
!
!> @brief addresses for SGS coefficients
!
!     Written by H. Matsui
!
!
      module m_SGS_address
!
      use m_precision
      use m_constants
!
      implicit  none
! 
!
      integer (kind=kint) :: iak_sgs_hf =   izero
      integer (kind=kint) :: iak_sgs_mf =   izero
      integer (kind=kint) :: iak_sgs_cf =   izero
      integer (kind=kint) :: iak_sgs_lor =  izero
      integer (kind=kint) :: iak_sgs_uxb =  izero
      integer (kind=kint) :: iak_sgs_tbuo = izero
      integer (kind=kint) :: iak_sgs_cbuo = izero
!
      integer (kind=kint) :: iak_diff_v =  izero
      integer (kind=kint) :: iak_diff_t =  izero
      integer (kind=kint) :: iak_diff_b =  izero
      integer (kind=kint) :: iak_diff_c =  izero
!
      integer (kind=kint) :: iak_diff_hf =   izero
      integer (kind=kint) :: iak_diff_mf =   izero
      integer (kind=kint) :: iak_diff_cf =   izero
      integer (kind=kint) :: iak_diff_lor =  izero
      integer (kind=kint) :: iak_diff_uxb =  izero
!
!
      integer (kind=kint) :: icomp_sgs_hf =   izero
      integer (kind=kint) :: icomp_sgs_mf =   izero
      integer (kind=kint) :: icomp_sgs_cf =   izero
      integer (kind=kint) :: icomp_sgs_lor =  izero
      integer (kind=kint) :: icomp_sgs_uxb =  izero
      integer (kind=kint) :: icomp_sgs_tbuo = izero
      integer (kind=kint) :: icomp_sgs_cbuo = izero
!
      integer (kind=kint) :: icomp_diff_v =  izero
      integer (kind=kint) :: icomp_diff_t =  izero
      integer (kind=kint) :: icomp_diff_b =  izero
      integer (kind=kint) :: icomp_diff_c =  izero
!
      integer (kind=kint) :: icomp_diff_hf =   izero
      integer (kind=kint) :: icomp_diff_mf =   izero
      integer (kind=kint) :: icomp_diff_cf =   izero
      integer (kind=kint) :: icomp_diff_lor =  izero
      integer (kind=kint) :: icomp_diff_uxb =  izero
!
      end module m_SGS_address
