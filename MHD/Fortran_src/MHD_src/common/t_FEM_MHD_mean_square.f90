!>@file   t_FEM_MHD_mean_square.f90
!!        module t_FEM_MHD_mean_square
!!
!! @author H. Matsui
!! @date   Programmed in 2002
!! @n      Modified  on Jan., 2013
!!
!
!> @brief addresses for volume integrated data
!!
!!@verbatim
!!      subroutine init_FEM_MHD_mean_square(nod_fld, fem_sq)
!!@endverbatim
!
      module t_FEM_MHD_mean_square
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
      use t_mean_square_values
!
      implicit  none
!
!
!>      strucutre of mean square data addresses
      type mean_square_list
!>        number of fields for spherical harmonics transform
        integer(kind = kint) :: nfield = 0
!>        number of mean square data for spherical harmonics transform
        integer(kind = kint) :: numrms =  0
!>        number of fields for spherical harmonics transform
        integer(kind = kint) :: numave =  0
!>        Field name for spherical transform
        character(len = kchara), allocatable :: field_name(:)
!>        address of spherical transform array
        integer(kind = kint), allocatable :: ifld_msq(:)
!>        address of spherical transform array
        integer(kind = kint), allocatable :: irms_msq(:)
!>        address of spherical transform array
        integer(kind = kint), allocatable :: jave_msq(:)
      end type mean_square_list
!
!>        Structure for mean square data for FEM_MHD
      type FEM_MHD_mean_square
!>        Structure for mean square values
        type(mean_square_values) :: msq
!>        Structure for mean square addresses not listed in phys_address
        type(mean_square_address) :: i_msq
!
!>      strucutre of mean square data addresses
        type(mean_square_list) :: msq_list
!
!>        Structure for addresses of volume average
        type(phys_address) :: i_rms
!>        Structure for addresses of mean square
        type(phys_address) :: j_ave
      end type FEM_MHD_mean_square
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_mean_square_name(msq_list)
!
      type(mean_square_list), intent(inout) :: msq_list
!
!
      allocate(msq_list%field_name(msq_list%nfield))
      allocate(msq_list%ifld_msq(msq_list%nfield))
      allocate(msq_list%irms_msq(msq_list%nfield))
      allocate(msq_list%jave_msq(msq_list%nfield))
!
      if(msq_list%nfield .le. 0) return
      msq_list%ifld_msq = 0
      msq_list%irms_msq = 0
      msq_list%jave_msq = 0
!
      end subroutine alloc_mean_square_name
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_mean_square_name(msq_list)
!
      type(mean_square_list), intent(inout) :: msq_list
!
!
      deallocate(msq_list%field_name, msq_list%ifld_msq)
      deallocate(msq_list%irms_msq, msq_list%jave_msq)
!
      end subroutine dealloc_mean_square_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_rms_address(field_name, num_comp,                  &
     &          i_phys, ir_rms, ja_ave, msq_list)
!
      use m_machine_parameter
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i_phys, num_comp
!
      integer(kind = kint), intent(inout) :: ir_rms, ja_ave
      type(mean_square_list), intent(inout) :: msq_list
!
      type(mean_square_list) :: tmp_list
!
!
      if(i_phys .eq. 0) return
      ir_rms = msq_list%numrms + 1
      ja_ave = msq_list%numave + 1
!
      tmp_list%nfield = msq_list%nfield
      call alloc_mean_square_name(tmp_list)
      call copy_field_name_4_mean_square                                &
     &   (tmp_list%nfield, msq_list, tmp_list)
      call dealloc_mean_square_name(msq_list)
!
      msq_list%nfield = msq_list%nfield + 1
      msq_list%numrms = msq_list%numrms + 1
      msq_list%numave =  msq_list%numave + num_comp
      call alloc_mean_square_name(msq_list)
      call copy_field_name_4_mean_square                                &
     &   (tmp_list%nfield, tmp_list, msq_list)
      call dealloc_mean_square_name(tmp_list)
!
      msq_list%field_name(msq_list%nfield) = field_name
      msq_list%ifld_msq(msq_list%nfield) = i_phys
      msq_list%irms_msq(msq_list%nfield) = ir_rms
      if(num_comp .gt. 0) msq_list%jave_msq(msq_list%nfield) = ja_ave
!
      if(iflag_debug .eq. 0) return
      write(*,'(i5,a2,a,a2,4i5)') msq_list%nfield, '. ',                &
     &    trim(msq_list%field_name(msq_list%nfield)), ': ',             &
     &    msq_list%ifld_msq(msq_list%nfield),                           &
     &    msq_list%irms_msq(msq_list%nfield),                           &
     &    msq_list%jave_msq(msq_list%nfield)
!
      end subroutine set_rms_address
!
!-----------------------------------------------------------------------
!
      subroutine copy_field_name_4_mean_square                          &
     &         (num_copy, list_org, list_new)
!
      integer(kind = kint), intent(in) :: num_copy
      type(mean_square_list), intent(in) :: list_org
      type(mean_square_list), intent(inout) :: list_new
!
!
      if(num_copy .le. 0) return
      list_new%field_name(1:num_copy)                                   &
     &            = list_org%field_name(1:num_copy) 
      list_new%ifld_msq(1:num_copy) = list_org%ifld_msq(1:num_copy)
      list_new%irms_msq(1:num_copy) = list_org%irms_msq(1:num_copy)
      list_new%jave_msq(1:num_copy) = list_org%jave_msq(1:num_copy)
!
      end subroutine copy_field_name_4_mean_square
!
!-----------------------------------------------------------------------
!
      end module t_FEM_MHD_mean_square
