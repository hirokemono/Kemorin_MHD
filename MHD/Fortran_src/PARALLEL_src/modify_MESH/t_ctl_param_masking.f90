!>@file   t_ctl_param_masking.f90
!!@brief  module t_ctl_param_masking
!!
!!@author Y. Lyao and H. Matsui
!!@date Programmed in Apr. 2018
!
!> @brief control parameters for parallel LIC
!!
!!@verbatim
!!      logical function multi_mask_flag(num_masking, masking, value)
!!        integer(kind=kint), intent(in) :: num_masking
!!        type(masking_parameter), intent(in) :: masking(num_masking)
!!        real(kind=kreal), intent(in) :: value(num_masking)
!!      logical function single_mask_flag(masking, value)
!!        type(masking_parameter), intent(in) :: masking
!!        real(kind=kreal), intent(in) :: value
!!
!!      subroutine set_control_lic_masking                            &
!!     &         (num_nod_phys, phys_nod_name, mask_ctl, masking)
!!      subroutine dealloc_lic_masking_range(masking)
!!        type(lic_masking_ctl), intent(in) :: mask_ctl
!!        type(lic_masking_parameter), intent(inout) :: masking
!!@endverbatim
!
      module t_ctl_param_masking
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_error_IDs
      use skip_comment_f
!
      implicit  none
!
      character(len=kchara), parameter, private                         &
     &                      :: hd_masking_geometry = 'geometry'
      character(len=kchara), parameter, private                         &
     &                     :: hd_masking_field = 'field'
!
!>        Mask type 1: geometry
      integer(kind = kint), parameter :: iflag_geometrymask = 1
!>        Mask type 2: field data
      integer(kind = kint), parameter :: iflag_fieldmask =    2
!

!>      Structure of masking parameter
      type lic_masking_parameter
!>        Mask type 1: geometry 2: field data
        integer(kind = kint) :: mask_type =   iflag_geometrymask
!>        Structure of soure decision field parameter for LIC
        type(pvr_field_parameter) :: field_info
!>        Geometry mask component index
        integer(kind = kint) :: idx_comp =  0
!>        Number of masking range
        integer(kind = kint) :: num_range = 1
!>        minimum value of source point range
        real(kind = kreal), allocatable :: range_min(:)
!>        maximum value of source point range
        real(kind = kreal), allocatable :: range_max(:)
      end type lic_masking_parameter
!
      private :: alloc_lic_masking_range
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      logical function multi_mask_flag(num_masking, masking, value)
!
      integer(kind=kint), intent(in) :: num_masking
      type(masking_parameter), intent(in) :: masking(num_masking)
      real(kind=kreal), intent(in) :: value(num_masking)
!
      integer(kind=kint) :: i
!
      multi_mask_flag = .TRUE.
      do i = 1, num_masking
        multi_mask_flag = single_mask_flag(masking(i), value(i))
        if(multi_mask_flag .eqv. .FALSE.) return
      end do
!
      end function multi_mask_flag
!
!  ---------------------------------------------------------------------
!
      logical function single_mask_flag(masking, value)
!
      type(masking_parameter), intent(in) :: masking
      real(kind=kreal), intent(in) :: value
!
      integer(kind=kint) :: j
!
      single_mask_flag = .TRUE.
      do j = 1, masking%num_range
        if(      (value .lt. masking%range_min(j))                      &
     &      .or. (value .gt. masking%range_max(j))) then
          single_mask_flag = .FALSE.
          return
        end if
      end do
!
      end function single_mask_flag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_control_lic_masking                                &
     &         (num_nod_phys, phys_nod_name, mask_ctl, masking)
!
      use t_control_data_LIC_masking
      use set_field_comp_for_viz
      use set_components_flags
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(lic_masking_ctl), intent(in) :: mask_ctl
      type(lic_masking_parameter), intent(inout) :: masking
!
      integer(kind = kint) :: icheck_ncomp(1)
      integer(kind = kint) :: ifld_tmp(1), icomp_tmp(1), ncomp_tmp(1)
      character(len = kchara) :: fldname_tmp(1)
      character(len = kchara) :: tmpfield(1), tmpcomp(1)
      character(len = kchara), parameter :: position_name = 'position'
!
      integer(kind = kint) :: j
!
!
      if(mask_ctl%mask_type_ctl%iflag .eq. 0) then
        e_message = 'Set mask type for LIC source masking'
        call calypso_mpi_abort(ierr_fld, e_message)
      end if
      if(mask_ctl%field_name_ctl%iflag .eq. 0) then
        e_message = 'Set field for LIC source masking'
        call calypso_mpi_abort(ierr_fld, e_message)
      end if
      if(mask_ctl%component_ctl%iflag .eq. 0) then
        e_message = 'Set component for LIC source masking'
        call calypso_mpi_abort(ierr_fld, e_message)
      end if
!
      if(mask_ctl%mask_type_ctl%charavalue                              &
     &                     .eq. hd_masking_geometry) then
        masking%mask_type = iflag_geometrymask
      else
        masking%mask_type = iflag_fieldmask
      end if
!
      if(masking%mask_type .eq. iflag_geometrymask) then
        call s_set_components_flags                                     &
     &     (mask_ctl%component_ctl%charavalue, position_name,           &
     &      masking%idx_comp, icheck_ncomp(1), ncomp_tmp(1),            &
     &      fldname_tmp(1))
      end if
!
      if(masking%mask_type .eq. iflag_fieldmask) then
        tmpfield(1) = mask_ctl%field_name_ctl%charavalue
        tmpcomp(1) =  mask_ctl%component_ctl%charavalue
        call set_components_4_viz(num_nod_phys, phys_nod_name,          &
     &      ione, tmpfield, tmpcomp, ione, ifld_tmp, icomp_tmp,         &
     &      icheck_ncomp, ncomp_tmp, fldname_tmp)
        masking%field_info%id_field = ifld_tmp(1)
        masking%field_info%id_component = icomp_tmp(1)
        masking%field_info%num_original_comp = ncomp_tmp(1)
        masking%field_info%field_name = fldname_tmp(1)
      end if
!
      call alloc_lic_masking_range                                      &
     &   (mask_ctl%mask_range_ctl%num, masking)
!
      do j = 1, masking%num_range
        masking%range_min(j) = mask_ctl%mask_range_ctl%vec1(j)
        masking%range_max(j) = mask_ctl%mask_range_ctl%vec2(j)
      end do
!
      end subroutine set_control_lic_masking
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_masking_range(masking)
!
      type(lic_masking_parameter), intent(inout) :: masking
!
      deallocate(masking%range_min, masking%range_max)
!
      end subroutine dealloc_lic_masking_range
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_lic_masking_range(num, masking)
!
      integer(kind = kint), intent(in) :: num
      type(lic_masking_parameter), intent(inout) :: masking
!
      masking%num_range = num
      allocate(masking%range_min(masking%num_range))
      allocate(masking%range_max(masking%num_range))
!
      if(masking%num_range .gt. 0) then
        masking%range_min(1:masking%num_range) = -1.0e15
        masking%range_max(1:masking%num_range) =  1.0e15
      end if
!
      end subroutine alloc_lic_masking_range
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_param_masking
