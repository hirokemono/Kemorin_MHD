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
!!      subroutine alloc_sph_trns_field_name(each_trns)
!!        type(spherical_transform_data), intent(inout) :: each_trns
!!      subroutine alloc_nonlinear_data(sph_rtp, trns)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!      subroutine alloc_nonlinear_pole(sph_rtp, trns)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!      subroutine dealloc_bwd_trns_field_name(trns)
!!      subroutine dealloc_fwd_trns_field_name(trns)
!!      subroutine dealloc_nonlinear_data(trns)
!!      subroutine dealloc_nonlinear_pole(trns)
!!      subroutine dealloc_nonlinear_zmean(trns)
!!        type(address_4_sph_trans), intent(inout) :: trns
!!
!!      subroutine count_num_fields_each_trans(each_trns,               &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(spherical_transform_data), intent(inout) :: each_trns
!!      subroutine copy_field_name_4_sph_trns                           &
!!     &         (num_copy, etrns_org, etrns_new)
!!        type(spherical_transform_data), intent(in) :: etrns_org
!!        type(spherical_transform_data), intent(inout) :: etrns_new
!!@endverbatim
!
      module t_addresses_sph_transform
!
      use m_precision
!
      use t_phys_address
      use t_SGS_model_addresses
      use t_spheric_rtp_data
      use t_sph_multi_FFTW
!
      implicit none
!
!>      strucutre of spherical transform data addresses
      type spherical_transform_data
!>        number of fields for spherical harmonics transform
        integer(kind = kint) :: nfield = 0
!>        number of components for spherical harmonics transform
        integer(kind = kint) :: ncomp =  0
!>        number of vector for spherical harmonics transform
        integer(kind = kint) :: num_vector = 0
!>        number of scalars for spherical harmonics transform
        integer(kind = kint) :: num_scalar = 0
!>        number of tensors for spherical harmonics transform
        integer(kind = kint) :: num_tensor = 0
!
!>        Field name for spherical transform
        character(len = kchara), allocatable :: field_name(:)
!>        address of spherical transform array
        integer(kind = kint), allocatable :: ifld_trns(:)
!>        address of backward transform for sprctr
        integer(kind = kint), allocatable :: ifld_rj(:)
!>        address of backward transform for nodal field
        integer(kind = kint), allocatable :: ifld_rtp(:)
!
!>        field data in grid space
        real(kind = kreal), allocatable :: fld_rtp(:,:)
!
!>        field data at pole
        real(kind = kreal), allocatable :: fld_pole(:,:)
!>        local field data at pole
        real(kind = kreal), allocatable :: flc_pole(:,:)
      end type spherical_transform_data
!
!
!>      strucutre of spherical transform data addresses
      type address_4_sph_trans
!>        strucutre of backward spherical transform data addresses
        type(spherical_transform_data) :: backward
!>        strucutre of forward spherical transform data addresses
        type(spherical_transform_data) :: forward
!
!>        addresses of fields for backward transform
        type(phys_address) :: b_trns
!>        addresses of forces for forward transform
        type(phys_address) :: f_trns
!>        addresses of SGS models for backward transform
        type(SGS_model_addresses) :: b_trns_LES
!>        addresses of SGS models for forward transform
        type(SGS_model_addresses) :: f_trns_LES
!
!>        Work area of Fourier transform for MHD
        type(work_for_sgl_FFTW) :: mul_FFTW
      end type address_4_sph_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trns_field_name(each_trns)
!
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      allocate(each_trns%field_name(each_trns%nfield))
      allocate(each_trns%ifld_trns(each_trns%nfield))
      allocate(each_trns%ifld_rj(each_trns%nfield))
      allocate(each_trns%ifld_rtp(each_trns%nfield))
!
      if(each_trns%nfield .le. 0) return
      each_trns%ifld_trns = 0
      each_trns%ifld_rj =   0
      each_trns%ifld_rtp =  0
!
      end subroutine alloc_sph_trns_field_name
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trns_field_data(sph_rtp, each_trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      allocate(each_trns%fld_rtp(sph_rtp%nnod_rtp,each_trns%ncomp))
      if(each_trns%ncomp .gt. 0) each_trns%fld_rtp = 0.0d0
!
      end subroutine alloc_sph_trns_field_data
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trns_pole_data(sph_rtp, each_trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      allocate(each_trns%fld_pole(sph_rtp%nnod_pole,each_trns%ncomp))
      allocate(each_trns%flc_pole(sph_rtp%nnod_pole,each_trns%ncomp))
!
      if(each_trns%ncomp*sph_rtp%nnod_pole .gt. 0) then
        each_trns%fld_pole = 0.0d0
        each_trns%flc_pole = 0.0d0
      end if
!
      end subroutine alloc_sph_trns_pole_data
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_sph_trns_field_name(each_trns)
!
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      deallocate(each_trns%field_name, each_trns%ifld_trns)
      deallocate(each_trns%ifld_rj, each_trns%ifld_rtp)
!
      end subroutine dealloc_sph_trns_field_name
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_sph_trns_field_dats(each_trns)
!
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      deallocate(each_trns%fld_rtp)
!
      end subroutine dealloc_sph_trns_field_dats
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_sph_trns_pole_data(each_trns)
!
      type(spherical_transform_data), intent(inout) :: each_trns
!
!
      deallocate(each_trns%fld_pole, each_trns%flc_pole)
!
      end subroutine dealloc_sph_trns_pole_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_data(sph_rtp, trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call alloc_sph_trns_field_data(sph_rtp, trns%backward)
      call alloc_sph_trns_field_data(sph_rtp, trns%forward)
!
      end subroutine alloc_nonlinear_data
!
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_pole(sph_rtp, trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call alloc_sph_trns_pole_data(sph_rtp, trns%backward)
      call alloc_sph_trns_pole_data(sph_rtp, trns%forward)
!
      end subroutine alloc_nonlinear_pole
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_data(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call dealloc_sph_trns_field_dats(trns%backward)
      call dealloc_sph_trns_field_dats(trns%forward)
!
      end subroutine dealloc_nonlinear_data
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_pole(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call dealloc_sph_trns_pole_data(trns%backward)
      call dealloc_sph_trns_pole_data(trns%forward)
!
      end subroutine dealloc_nonlinear_pole
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_bwd_trns_field_name(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call dealloc_sph_trns_field_name(trns%backward)
!
      end subroutine dealloc_bwd_trns_field_name
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_fwd_trns_field_name(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call dealloc_sph_trns_field_name(trns%forward)
!
      end subroutine dealloc_fwd_trns_field_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_fields_each_trans(each_trns,                 &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(spherical_transform_data), intent(inout) :: each_trns
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rj_2_rtp
!
!
      nscltsr_rj_2_rtp = each_trns%num_scalar + 6*each_trns%num_tensor
!
      ncomp_sph_trans =   max(ncomp_sph_trans, each_trns%ncomp)
      nvector_sph_trans = max(nvector_sph_trans, each_trns%num_vector)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rj_2_rtp)
!
      end subroutine count_num_fields_each_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_field_name_4_sph_trns                             &
     &         (num_copy, etrns_org, etrns_new)
!
      integer(kind = kint), intent(in) :: num_copy
      type(spherical_transform_data), intent(in) :: etrns_org
      type(spherical_transform_data), intent(inout) :: etrns_new
!
!
      if(num_copy .le. 0) return
      etrns_new%field_name(1:num_copy)                                  &
     &            = etrns_org%field_name(1:num_copy) 
      etrns_new%ifld_trns(1:num_copy) = etrns_org%ifld_trns(1:num_copy)
      etrns_new%ifld_rj(1:num_copy) =   etrns_org%ifld_rj(1:num_copy)
      etrns_new%ifld_rtp(1:num_copy) =  etrns_org%ifld_rtp(1:num_copy)
!
      end subroutine copy_field_name_4_sph_trns
!
!-----------------------------------------------------------------------
!
      end module t_addresses_sph_transform
