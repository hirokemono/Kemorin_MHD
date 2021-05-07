!>@file  t_control_param_4_pvr_field.f90
!!       module t_control_param_4_pvr_field
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Structures for parameteres for volume rendering
!!
!!@verbatim
!!      subroutine set_control_field_4_pvr(field_ctl, comp_ctl,         &
!!     &          num_nod_phys, phys_nod_name, fld_param, icheck_ncomp)
!!        type(read_character_item), intent(in) :: field_ctl
!!        type(read_character_item), intent(in) :: comp_ctl
!!        type(pvr_field_parameter), intent(inout) :: fld_param
!!      subroutine check_pvr_field_control                              &
!!     &         (pvr_field_ctl, num_nod_phys, phys_nod_name)
!!        type(read_character_item), intent(in) :: pvr_field_ctl
!!@endverbatim
!
      module t_control_param_4_pvr_field
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit  none
!
!
!>  Structure for field parameter for PVR
      type pvr_field_parameter
!>     Field type for PVR data
        integer(kind = kint) :: id_field =          0
!>     Component flag for PVR data
        integer(kind = kint) :: id_component =      0
!>     Number of component of data for Rendering
        integer(kind = kint) :: num_original_comp = 0
!>     Field name of data for Rendering
        character(len = kchara) :: field_name
      end type pvr_field_parameter
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_field_4_pvr(field_ctl, comp_ctl,           &
     &          num_nod_phys, phys_nod_name, fld_param, icheck_ncomp)
!
      use t_control_array_character
      use set_field_comp_for_viz
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
      type(read_character_item), intent(in) :: field_ctl
      type(read_character_item), intent(in) :: comp_ctl
!
      type(pvr_field_parameter), intent(inout) :: fld_param
      integer(kind = kint), intent(inout) :: icheck_ncomp(1)
!
      integer(kind = kint) :: ifld_tmp(1), icomp_tmp(1), ncomp_tmp(1)
      character(len = kchara) :: fldname_tmp(1)
      character(len = kchara) :: tmpfield(1), tmpcomp(1)
!
!
      tmpfield(1) = field_ctl%charavalue
      tmpcomp(1) =  comp_ctl%charavalue
      call set_components_4_viz                                         &
     &   (num_nod_phys, phys_nod_name, ione, tmpfield, tmpcomp, ione,   &
     &    ifld_tmp, icomp_tmp, icheck_ncomp, ncomp_tmp, fldname_tmp)
      fld_param%id_field =          ifld_tmp(1)
      fld_param%id_component =      icomp_tmp(1)
      fld_param%num_original_comp = ncomp_tmp(1)
      fld_param%field_name =        fldname_tmp(1)
!
      end subroutine set_control_field_4_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine check_pvr_field_control                                &
     &         (pvr_field_ctl, num_nod_phys, phys_nod_name)
!
      use m_error_IDs
      use t_control_array_character
      use skip_comment_f
      use set_field_comp_for_viz
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
      type(read_character_item), intent(in) :: pvr_field_ctl
!
      integer(kind = kint) :: num_field, num_phys_viz
      character(len = kchara) :: tmpfield(1)
!
!
      tmpfield(1) = pvr_field_ctl%charavalue
      call check_field_4_viz(num_nod_phys, phys_nod_name,               &
     &    ione, tmpfield, num_field, num_phys_viz)
      if(num_field .eq. 0) then
        call calypso_MPI_abort(ierr_PVR,'set correct field name')
      end if
!
      end subroutine check_pvr_field_control
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_4_pvr_field
