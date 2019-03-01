!>@file   t_control_param_LIC_masking.f90
!!@brief  module t_control_param_LIC_masking
!!
!!@author H. Matsui
!!@date Programmed in Apr. 2018
!
!> @brief control parameters for parallel LIC
!!
!!@verbatim
!!      subroutine set_control_lic_masking                            &
!!     &         (num_nod_phys, phys_nod_name, mask_ctl, masking)
!!      subroutine dealloc_lic_masking_range(masking)
!!        type(lic_masking_ctl), intent(in) :: mask_ctl
!!        type(lic_masking_parameter), intent(inout) :: masking
!!@endverbatim
!
      module t_control_param_LIC_masking
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_error_IDs
      use skip_comment_f
      use t_control_params_4_pvr
!
      implicit  none
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
!>        Geometry mask component index 1:x 2:y 3:z 4:magnitude
        integer(kind = kint) :: comp_idx =   1
!>        Number of masking range
        integer(kind = kint) :: num_range =   1
!>        minimum value of source point range
        real(kind = kreal), allocatable :: range_min(:)
!>        maximum value of source point range
        real(kind = kreal), allocatable :: range_max(:)
      end type lic_masking_parameter
!
      private :: alloc_lic_masking_range
!
      character(len=kchara) :: hd_masking_geometry = 'geometry'
      character(len=kchara) :: hd_masking_field = 'field'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_lic_masking                              &
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
      if(mask_ctl%mask_type_ctl%charavalue .eq. hd_masking_geometry) then
        masking%mask_type = iflag_geometrymask
      else
        masking%mask_type = iflag_fieldmask
      end if
!
      if(masking%mask_type .eq. iflag_geometrymask) then
        if(mask_ctl%component_ctl%charavalue .eq. 'x') then
          masking%comp_idx = 1
        else if(mask_ctl%component_ctl%charavalue .eq. 'y') then
          masking%comp_idx = 2
        else if(mask_ctl%component_ctl%charavalue .eq. 'z') then
          masking%comp_idx = 3
        else if(mask_ctl%component_ctl%charavalue .eq. 'magnitude') then
          masking%comp_idx = 4
        end if
      end if
!
      if(masking%mask_type .eq. iflag_fieldmask) then
        tmpfield(1) = mask_ctl%field_name_ctl%charavalue
        tmpcomp(1) =  mask_ctl%component_ctl%charavalue
        call set_components_4_viz(num_nod_phys, phys_nod_name,            &
       &    ione, tmpfield, tmpcomp, ione, ifld_tmp, icomp_tmp,           &
       &    icheck_ncomp, ncomp_tmp, fldname_tmp)
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
      end module t_control_param_LIC_masking
