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
!!      subroutine alloc_bwd_trns_field_name(trns)
!!      subroutine alloc_fwd_trns_field_name(trns)
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
!!     &         (field_name, i_trns, i_rj, irtp, trns)
!!      subroutine set_field_name_4_fwd_trns                            &
!!     &         (field_name, i_trns, i_rj, irtp, trns)
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
!>        number of components for backward spherical harmonics transform
        integer(kind = kint) :: nfield_rj_2_rtp = 0
!>        Field name for backward transform
        character(len = kchara), allocatable :: b_trns_name(:)
!>        address of backward transform array
        integer(kind = kint), allocatable :: ifld_trns(:)
!>        address of backward transform for sprctr
        integer(kind = kint), allocatable :: ifld_rj(:)
!>        address of backward transform for nodal field
        integer(kind = kint), allocatable :: ifld_rtp(:)
!
!>        number of components for backward spherical harmonics transform
        integer(kind = kint) :: nfield_rtp_2_rj = 0
!>        Field name for forward transform
        character(len = kchara), allocatable :: f_trns_name(:)
!>        address of forward transform array
        integer(kind = kint), allocatable :: ifrc_trns(:)
!>        address of forward transform for sprctr
        integer(kind = kint), allocatable :: ifrc_rj(:)
!>        address of forward transform for field
        integer(kind = kint), allocatable :: ifrc_rtp(:)
!
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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_bwd_trns_field_name(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      allocate(trns%b_trns_name(trns%nfield_rj_2_rtp))
      allocate(trns%ifld_trns(trns%nfield_rj_2_rtp))
      allocate(trns%ifld_rj(trns%nfield_rj_2_rtp))
      allocate(trns%ifld_rtp(trns%nfield_rj_2_rtp))
!
      if(trns%nfield_rj_2_rtp .le. 0) return
      trns%ifld_trns = 0
      trns%ifld_rj =   0
      trns%ifld_rtp =  0
!
      end subroutine alloc_bwd_trns_field_name
!
!-----------------------------------------------------------------------
!
      subroutine alloc_fwd_trns_field_name(trns)
!
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      allocate(trns%f_trns_name(trns%nfield_rtp_2_rj))
      allocate(trns%ifrc_trns(trns%nfield_rtp_2_rj))
      allocate(trns%ifrc_rj(trns%nfield_rtp_2_rj))
      allocate(trns%ifrc_rtp(trns%nfield_rtp_2_rj))
!
      if(trns%nfield_rtp_2_rj .le. 0) return
      trns%ifrc_trns = 0
      trns%ifrc_rj =   0
      trns%ifrc_rtp =  0
!
      end subroutine alloc_fwd_trns_field_name
!
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_data(sph_rtp, trns)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      allocate(trns%fld_rtp(sph_rtp%nnod_rtp,trns%ncomp_rj_2_rtp))
      allocate(trns%frc_rtp(sph_rtp%nnod_rtp,trns%ncomp_rtp_2_rj))
      allocate(trns%fld_zm(sph_rtp%nnod_med,6))
!
      trns%fld_zm = 0.0d0
      if(trns%ncomp_rj_2_rtp .gt. 0) trns%fld_rtp = 0.0d0
      if(trns%ncomp_rtp_2_rj .gt. 0) trns%frc_rtp = 0.0d0
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
      allocate(trns%fld_pole(sph_rtp%nnod_pole,trns%ncomp_rj_2_rtp))
      allocate(trns%flc_pole(sph_rtp%nnod_pole,trns%ncomp_rj_2_rtp))
!
      allocate(trns%frc_pole(sph_rtp%nnod_pole,trns%ncomp_rtp_2_rj))
!
      if(trns%ncomp_rj_2_rtp .gt. 0) trns%fld_pole = 0.0d0
      if(trns%ncomp_rj_2_rtp .gt. 0) trns%flc_pole = 0.0d0
      if(trns%ncomp_rtp_2_rj .gt. 0) trns%frc_pole = 0.0d0
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
      deallocate(trns%b_trns_name)
      deallocate(trns%ifld_trns, trns%ifld_rj, trns%ifld_rtp)
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
      deallocate(trns%f_trns_name)
      deallocate(trns%ifrc_trns, trns%ifrc_rj, trns%ifrc_rtp)
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
      subroutine count_num_fields_4_sph_trans(trns, ncomp_sph_trans,    &
     &          nvector_sph_trans, nscalar_sph_trans)
!
      type(address_4_sph_trans), intent(inout) :: trns
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
!
!
      trns%nfield_rj_2_rtp =  trns%nvector_rj_2_rtp                     &
     &                      + trns%nscalar_rj_2_rtp                     &
     &                      + trns%ntensor_rj_2_rtp
      trns%nfield_rtp_2_rj =  trns%nvector_rtp_2_rj                     &
     &                      + trns%nscalar_rtp_2_rj                     &
     &                      + trns%ntensor_rtp_2_rj
!
      nscltsr_rj_2_rtp                                                  &
     &      = trns%nscalar_rj_2_rtp + 6*trns%ntensor_rj_2_rtp
      trns%ncomp_rj_2_rtp                                               &
     &      = 3*trns%nvector_rj_2_rtp + nscltsr_rj_2_rtp
!
      nscltsr_rtp_2_rj                                                  &
     &      = trns%nscalar_rtp_2_rj + 6*trns%ntensor_rtp_2_rj
      trns%ncomp_rtp_2_rj                                               &
     &      = 3*trns%nvector_rtp_2_rj + nscltsr_rtp_2_rj
!
      ncomp_sph_trans =   max(ncomp_sph_trans, trns%ncomp_rtp_2_rj)
      ncomp_sph_trans =   max(ncomp_sph_trans, trns%ncomp_rj_2_rtp)
      nvector_sph_trans = max(nvector_sph_trans, trns%nvector_rj_2_rtp)
      nvector_sph_trans = max(nvector_sph_trans, trns%nvector_rtp_2_rj)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rj_2_rtp)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rtp_2_rj)
!
      end subroutine count_num_fields_4_sph_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_field_name_4_bwd_trns                              &
     &         (field_name, i_trns, i_rj, irtp, icou, trns)
!
      use m_machine_parameter
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i_trns, i_rj, irtp
      integer(kind = kint), intent(inout) :: icou
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      if(i_trns .eq. 0) return
      icou = icou + 1
      trns%b_trns_name(icou) = field_name
      trns%ifld_trns(icou) = i_trns
      trns%ifld_rj(icou) =   i_rj
      trns%ifld_rtp(icou) =  irtp
!
      if(iflag_debug .eq. 0) return
      write(*,*) icou, trim(trns%b_trns_name(icou)), ': ',              &
     &    trns%ifld_trns(icou), trns%ifld_rj(icou), trns%ifld_rtp(icou)
!
      end subroutine set_field_name_4_bwd_trns
!
!-----------------------------------------------------------------------
!
      subroutine set_field_name_4_fwd_trns                              &
     &         (field_name, i_trns, i_rj, irtp, icou, trns)
!
      use m_machine_parameter
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i_trns, i_rj, irtp
      integer(kind = kint), intent(inout) :: icou
      type(address_4_sph_trans), intent(inout) :: trns
!
!
      if(i_trns .eq. 0) return
      icou = icou + 1
      trns%f_trns_name(icou) = field_name
      trns%ifrc_trns(icou) = i_trns
      trns%ifrc_rj(icou) =   i_rj
      trns%ifrc_rtp(icou) =  irtp
!
      if(iflag_debug .eq. 0) return
      write(*,*) icou, trim(trns%f_trns_name(icou)), ': ',              &
     &    trns%ifrc_trns(icou), trns%ifrc_rj(icou), trns%ifrc_rtp(icou)
!
      end subroutine set_field_name_4_fwd_trns
!
!-----------------------------------------------------------------------
!
      end module t_addresses_sph_transform
