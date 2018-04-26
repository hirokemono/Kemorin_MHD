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
!!      subroutine add_scalar_trans_flag                                &
!!     &         (is_fld, nfield_vec, num_trans, itrans)
!!      subroutine add_vector_trans_flag(is_fld, num_trans, itrans)
!!
!!      subroutine add_scl_trans_flag_snap(is_fld, irtp_fld,            &
!!     &          nfield_vec, num_trans, itrans)
!!      subroutine add_vec_trans_flag_snap(is_fld, irtp_fld,            &
!!     &          num_trans, itrans)
!!
!!      subroutine count_num_fields_4_sph_trans(trns, ncomp_sph_trans,  &
!!     &          nvector_sph_trans, nscalar_sph_trans)
!!      subroutine set_field_name_4_bwd_trns                            &
!!     &         (field_name, i_trns, i_pol, i_tor, irtp, trns)
!!      subroutine set_field_name_4_fwd_trns                            &
!!     &         (field_name, i_trns, i_pol, i_tor, irtp, trns)
!!        type(address_4_sph_trans), intent(inout) :: trns
!!@endverbatim
!
      module t_addresses_sph_transform
!
      use m_precision
!
      use t_phys_address
      use t_spheric_rtp_data
!
      implicit none
!
!>      strucutre of spherical transform data addresses
      type address_each_sph_trans
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
      end type address_each_sph_trans
!

!>      strucutre of spherical transform data addresses
      type address_4_sph_trans
!>        strucutre of backward spherical transform data addresses
        type(address_each_sph_trans) :: backward
!>        strucutre of forward spherical transform data addresses
        type(address_each_sph_trans) :: forward
!
!>        addresses of fields for backward transform
        type(phys_address) :: b_trns
!>        addresses of forces for forward transform
        type(phys_address) :: f_trns
!
!>        field data in grid space
        real(kind = kreal), allocatable :: fld_rtp(:,:)
!>        Nonliear terms data in grid space
        real(kind = kreal), allocatable :: frc_rtp(:,:)
!
!>        field data at pole
        real(kind = kreal), allocatable :: fld_pole(:,:)
!>        local field data at pole
        real(kind = kreal), allocatable :: flc_pole(:,:)
!
!>        Nonlinear terms data at pole
        real(kind = kreal), allocatable :: frc_pole(:,:)
!
!>        zonal mean of field data in grid space
        real(kind = kreal), allocatable :: fld_zm(:,:)
      end type address_4_sph_trans
!
      private :: alloc_sph_trns_field_name
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trns_field_name(each_trns)
!
      type(address_each_sph_trans), intent(inout) :: each_trns
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
      subroutine dealloc_sph_trns_field_name(each_trns)
!
      type(address_each_sph_trans), intent(inout) :: each_trns
!
!
      deallocate(each_trns%field_name, each_trns%ifld_trns)
      deallocate(each_trns%ifld_rj, each_trns%ifld_rtp)
!
      end subroutine dealloc_sph_trns_field_name
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
      allocate(trns%fld_rtp(sph_rtp%nnod_rtp,trns%backward%ncomp))
      allocate(trns%frc_rtp(sph_rtp%nnod_rtp,trns%forward%ncomp))
      allocate(trns%fld_zm(sph_rtp%nnod_med,6))
!
      trns%fld_zm = 0.0d0
      if(trns%backward%ncomp .gt. 0) trns%fld_rtp = 0.0d0
      if(trns%forward%ncomp .gt. 0) trns%frc_rtp = 0.0d0
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
      allocate(trns%fld_pole(sph_rtp%nnod_pole,trns%backward%ncomp))
      allocate(trns%flc_pole(sph_rtp%nnod_pole,trns%backward%ncomp))
!
      allocate(trns%frc_pole(sph_rtp%nnod_pole,trns%forward%ncomp))
!
      if(trns%backward%ncomp .gt. 0) trns%fld_pole = 0.0d0
      if(trns%backward%ncomp .gt. 0) trns%flc_pole = 0.0d0
      if(trns%forward%ncomp .gt. 0) trns%frc_pole = 0.0d0
!
      end subroutine alloc_nonlinear_pole
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_data(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
      deallocate(trns%fld_rtp, trns%frc_rtp)
      deallocate(trns%fld_zm)
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
      deallocate(trns%fld_pole, trns%flc_pole, trns%frc_pole)
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
      subroutine add_scalar_trans_flag                                  &
     &         (iflag, nfield_vec, num_trans, itrans)
!
      integer(kind = kint), intent(in) :: iflag, nfield_vec
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
!
      if(iflag .gt. 0) then
        num_trans = num_trans + 1
        itrans = num_trans + 3*nfield_vec
      end if
!
      end subroutine add_scalar_trans_flag
!
!-----------------------------------------------------------------------
!
      subroutine add_vector_trans_flag(iflag, num_trans, itrans)
!
      integer(kind = kint), intent(in) :: iflag
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
!
      if(iflag .gt. 0) then
        num_trans = num_trans + 1
        itrans = 3*num_trans -  2
      end if
!
      end subroutine add_vector_trans_flag
!
!-----------------------------------------------------------------------
!
      subroutine add_tensor_trans_flag                                  &
     &         (iflag, nfield_vec, nfield_scl, num_trans, itrans)
!
      integer(kind = kint), intent(in) :: iflag, nfield_vec, nfield_scl
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
!
      if(iflag .gt. 0) then
        num_trans = num_trans + 1
        itrans = num_trans + 3*nfield_vec + nfield_scl
      end if
!
      end subroutine add_tensor_trans_flag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_scl_trans_flag_snap(is_fld, irtp_fld,              &
     &          nfield_vec, num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld, nfield_vec
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
      integer(kind = kint) :: iflag
!
      iflag = is_fld*irtp_fld
      call add_scalar_trans_flag (iflag, nfield_vec, num_trans, itrans)
!
      end subroutine add_scl_trans_flag_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_vec_trans_flag_snap(is_fld, irtp_fld,              &
     &          num_trans, itrans)
!
      integer(kind = kint), intent(in) :: is_fld, irtp_fld
      integer(kind = kint), intent(inout) :: num_trans, itrans
!
      integer(kind = kint) :: iflag
!
      iflag = is_fld*irtp_fld
      call add_vector_trans_flag(iflag, num_trans, itrans)
!
      end subroutine add_vec_trans_flag_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_fields_each_trans(each_trns,                 &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(address_each_sph_trans), intent(inout) :: each_trns
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
!
!
      each_trns%nfield =  each_trns%num_vector + each_trns%num_scalar   &
     &                  + each_trns%num_tensor
!
      nscltsr_rj_2_rtp = each_trns%num_scalar + 6*each_trns%num_tensor
      each_trns%ncomp = 3*each_trns%num_vector + nscltsr_rj_2_rtp
!
      ncomp_sph_trans =   max(ncomp_sph_trans, each_trns%ncomp)
      nvector_sph_trans = max(nvector_sph_trans, each_trns%num_vector)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rj_2_rtp)
!
      end subroutine count_num_fields_each_trans
!
!-----------------------------------------------------------------------
!
      subroutine count_num_fields_4_sph_trans(trns,                     &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(address_4_sph_trans), intent(inout) :: trns
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      call count_num_fields_each_trans(trns%backward, ncomp_sph_trans,  &
     &          nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns%forward, ncomp_sph_trans,   &
     &          nvector_sph_trans, nscalar_sph_trans)
!
      call alloc_sph_trns_field_name(trns%backward)
      call alloc_sph_trns_field_name(trns%forward)
!
      end subroutine count_num_fields_4_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_field_name_4_sph_trns(field_name, i_trns,          &
     &          i_pol, i_tor, irtp, icou, each_trns)
!
      use m_machine_parameter
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i_trns, i_pol, i_tor, irtp
      integer(kind = kint), intent(inout) :: icou
      type(address_each_sph_trans), intent(inout) :: each_trns
!
!
      if(i_trns .eq. 0) return
      icou = icou + 1
      each_trns%field_name(icou) = field_name
      each_trns%ifld_trns(icou) = i_trns
      each_trns%ifld_rj(icou) =   i_pol
      each_trns%ifld_rtp(icou) =  irtp
!
      if(iflag_debug .eq. 0) return
      write(*,'(i5,a2,a,a2,4i5)')                                       &
     &    icou, '. ', trim(each_trns%field_name(icou)), ': ',           &
     &    each_trns%ifld_trns(icou), each_trns%ifld_rj(icou), i_tor,    &
     &    each_trns%ifld_rtp(icou)
!
      end subroutine set_field_name_4_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine set_field_name_4_bwd_trns                              &
     &         (field_name, i_trns, i_pol, i_tor, irtp, icou, trns)
!
      use m_machine_parameter
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i_trns, i_pol, i_tor, irtp
      integer(kind = kint), intent(inout) :: icou
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call set_field_name_4_sph_trns(field_name, i_trns,               &
     &          i_pol, i_tor, irtp, icou, trns%backward)
!
      end subroutine set_field_name_4_bwd_trns
!
!-----------------------------------------------------------------------
!
      subroutine set_field_name_4_fwd_trns                              &
     &         (field_name, i_trns, i_pol, i_tor, irtp, icou, trns)
!
      use m_machine_parameter
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i_trns, i_pol, i_tor, irtp
      integer(kind = kint), intent(inout) :: icou
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      call set_field_name_4_sph_trns(field_name, i_trns,                &
     &          i_pol, i_tor, irtp, icou, trns%forward)
!
      end subroutine set_field_name_4_fwd_trns
!
!-----------------------------------------------------------------------
!
      end module t_addresses_sph_transform
