!>@file   t_ctl_param_masking.f90
!!@brief  module set_control_LIC_masking
!!
!!@author Y. Lyao and H. Matsui
!!@date Programmed in Apr. 2018
!
!> @brief Set control parameters for LIC masking
!!
!!@verbatim
!!      subroutine s_set_control_LIC_masking                            &
!!     &         (num_nod_phys, phys_nod_name, mask_ctl, masking)
!!        integer(kind = kint), intent(in) :: num_nod_phys
!!        character(len=kchara), intent(in)                             &
!!     &                       :: phys_nod_name(num_nod_phys)
!!        type(masking_by_field_ctl), intent(in) :: mask_ctl
!!        type(masking_parameter), intent(inout) :: masking
!!@endverbatim
!
      module set_control_LIC_masking
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_error_IDs
      use skip_comment_f
      use t_ctl_param_masking
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_control_LIC_masking                              &
     &         (num_nod_phys, phys_nod_name, mask_ctl, masking)
!
      use t_control_data_masking
      use set_field_comp_for_viz
      use set_components_flags
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      type(masking_by_field_ctl), intent(in) :: mask_ctl
      type(masking_parameter), intent(inout) :: masking
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
        masking%id_mask_field = ifld_tmp(1)
        masking%id_mask_comp = icomp_tmp(1)
      end if
!
      call alloc_masking_range(mask_ctl%mask_range_ctl%num, masking)
!
      do j = 1, masking%num_range
        masking%range_min(j) = mask_ctl%mask_range_ctl%vec1(j)
        masking%range_max(j) = mask_ctl%mask_range_ctl%vec2(j)
      end do
!
      end subroutine s_set_control_LIC_masking
!
!  ---------------------------------------------------------------------
!
      end module set_control_LIC_masking
