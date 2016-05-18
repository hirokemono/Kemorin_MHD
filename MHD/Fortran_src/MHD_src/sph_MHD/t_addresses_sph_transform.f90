!>@file   t_addresses_sph_transform.f90
!!@brief  module t_addresses_sph_transform
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine alloc_nonlinear_data(nnod_rtp)
!!      subroutine dealloc_nonlinear_data
!!
!!      subroutine add_scalar_trans_flag(is_fld, irtp_fld,              &
!!     &          nfield_vec, num_trans, itrans)
!!      subroutine add_vec_trans_flag(is_fld, irtp_fld,                 &
!!     &          num_trans, itrans)
!!@endverbatim
!
      module t_addresses_sph_transform
!
      use m_precision
!
      use t_phys_address
!
      implicit none
!
!>      strucutre for spherical transform data addresses
      type address_4_sph_trans
!>        number of components for backward spherical harmonics transform
        integer(kind = kint) :: ncomp_rj_2_rtp = 0
!>        number of components
!!        for backward vector spherical harmonics transform
        integer(kind = kint) :: nvector_rj_2_rtp = 0
!>        number of scalars for backward spherical harmonics transform
        integer(kind = kint) :: nscalar_rj_2_rtp = 0
!>        number of tensors for backward spherical harmonics transform
        integer(kind = kint) :: ntensor_rj_2_rtp = 0
!
!>        number of components for forward spherical harmonics transform
        integer(kind = kint) :: ncomp_rtp_2_rj = 0
!>        number of vectors for forward spherical harmonics transform
        integer(kind = kint) :: nvector_rtp_2_rj = 0
!>        number of scalars for forward spherical harmonics transform
        integer(kind = kint) :: nscalar_rtp_2_rj = 0
!>        number of tensors for forward spherical harmonics transform
        integer(kind = kint) :: ntensor_rtp_2_rj = 0
!
!>        addresses of fields for backward transform
        type(phys_address) :: b_trns
!>        addresses of forces for forward transform
        type(phys_address) :: f_trns
!
!>        field data to evaluate nonliear terms in grid space
        real(kind = kreal), pointer :: fld_rtp(:,:)
!>        Nonliear terms data in grid space
        real(kind = kreal), pointer :: frc_rtp(:,:)
      end type address_4_sph_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_data(nnod_rtp, trns)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      allocate(trns%fld_rtp(nnod_rtp,trns%ncomp_rj_2_rtp))
      allocate(trns%frc_rtp(nnod_rtp,trns%ncomp_rtp_2_rj))
      if(trns%ncomp_rj_2_rtp .gt. 0) trns%fld_rtp = 0.0d0
      if(trns%ncomp_rtp_2_rj .gt. 0) trns%frc_rtp = 0.0d0
!
      end subroutine alloc_nonlinear_data
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_data(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
      deallocate(trns%fld_rtp, trns%frc_rtp)
!
      end subroutine dealloc_nonlinear_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_scalar_trans_flag(is_fld, irtp_fld,                &
     &          nfield_vec, num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld, nfield_vec
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
!
      if( (is_fld*irtp_fld) .gt. 0) then
        num_trans = num_trans + 1
        itrans = num_trans + 3*nfield_vec
      end if
!
      end subroutine add_scalar_trans_flag
!
!-----------------------------------------------------------------------
!
      subroutine add_vec_trans_flag(is_fld, irtp_fld,                   &
     &          num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
!
      if( (is_fld*irtp_fld) .gt. 0) then
        num_trans = num_trans + 1
        itrans = 3*num_trans - 2
      end if
!
      end subroutine add_vec_trans_flag
!
!-----------------------------------------------------------------------
!
      end module t_addresses_sph_transform
