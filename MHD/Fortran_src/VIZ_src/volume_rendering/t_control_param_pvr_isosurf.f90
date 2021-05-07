!>@file   t_control_param_pvr_isosurf.f90
!!@brief  module t_control_param_pvr_isosurf
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set each PVR parameters from control
!!
!!@verbatim
!!      subroutine set_control_pvr_isosurf(num_nod_phys, phys_nod_name, &
!!     &          pvr_iso_ctl, pvr_field_ctl, pvr_comp_ctl, pvr_iso_p)
!!      subroutine dealloc_pvr_isosurf_param(pvr_iso_p)
!!        type(read_character_item), intent(in) :: pvr_field_ctl
!!        type(read_character_item), intent(in) :: pvr_comp_ctl
!!        type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!!        type(pvr_isosurf_parameter), intent(inout) :: pvr_iso_p
!!@endverbatim
!
      module t_control_param_pvr_isosurf
!
      use m_precision
!
      use m_constants
      use m_error_IDs
      use calypso_mpi
!
      use t_control_param_4_pvr_field
!
      implicit  none
!
!>  Structure of parameters for isosurfaces in PVR
      type pvr_isosurf_parameter
!>        Number of isosurfaces
        integer(kind = kint) :: num_isosurf
!>        Number of isosurfaces
        integer(kind = kint), allocatable :: itype_isosurf(:)
!>        field value for isosurfaces
        real(kind = kreal), allocatable :: iso_value(:)
!>        Opacity value for isosurfaces
        real(kind = kreal), allocatable :: iso_opacity(:)
!
!>        field paramters for isosurface
        type(pvr_field_parameter), allocatable :: iso_fld_param(:)
!>        field paramters for isosurface
        integer(kind = kint), allocatable :: icheck_iso_ncomp(:)
!>
        real(kind = kreal), allocatable :: d_iso(:,:)
      end type pvr_isosurf_parameter
!
      private :: alloc_pvr_isosurf_param, set_control_pvr_isosurf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_pvr_isosurfs(num_nod_phys, phys_nod_name,  &
     &          pvr_field_ctl, pvr_comp_ctl, pvr_isos_c, pvr_iso_p)
!
      use t_control_data_pvr_isosurfs
      use t_control_array_character
      use pvr_surface_enhancement
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
      type(read_character_item), intent(in) :: pvr_field_ctl
      type(read_character_item), intent(in) :: pvr_comp_ctl
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
      type(pvr_isosurf_parameter), intent(inout) :: pvr_iso_p
!
      integer(kind = kint) ::  i
!
!
      pvr_iso_p%num_isosurf = pvr_isos_c%num_pvr_iso_ctl
      call alloc_pvr_isosurf_param(pvr_iso_p)
      if(pvr_iso_p%num_isosurf .le. 0) return
!
      do i = 1, pvr_iso_p%num_isosurf
        call set_control_pvr_isosurf                                    &
     &    (i, num_nod_phys, phys_nod_name, pvr_field_ctl, pvr_comp_ctl, &
     &     pvr_isos_c%pvr_iso_ctl(i), pvr_iso_p)
      end do
!
      end subroutine set_control_pvr_isosurfs
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurf_param(pvr_iso_p)
!
      type(pvr_isosurf_parameter), intent(inout) :: pvr_iso_p
!
!
      deallocate(pvr_iso_p%itype_isosurf)
      deallocate(pvr_iso_p%iso_value, pvr_iso_p%iso_opacity)
      deallocate(pvr_iso_p%iso_fld_param, pvr_iso_p%icheck_iso_ncomp)
!
      end subroutine dealloc_pvr_isosurf_param
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurf_param(pvr_iso_p)
!
      type(pvr_isosurf_parameter), intent(inout) :: pvr_iso_p
!
!
      allocate(pvr_iso_p%iso_fld_param(pvr_iso_p%num_isosurf))
      allocate(pvr_iso_p%itype_isosurf(pvr_iso_p%num_isosurf))
      allocate(pvr_iso_p%iso_value(pvr_iso_p%num_isosurf))
      allocate(pvr_iso_p%iso_opacity(pvr_iso_p%num_isosurf))
      allocate(pvr_iso_p%icheck_iso_ncomp(pvr_iso_p%num_isosurf))
!
      if(pvr_iso_p%num_isosurf .le. 0) return
      pvr_iso_p%icheck_iso_ncomp(1:pvr_iso_p%num_isosurf) = 0
      pvr_iso_p%itype_isosurf(1:pvr_iso_p%num_isosurf) = 0
      pvr_iso_p%iso_value(1:pvr_iso_p%num_isosurf) =     zero
      pvr_iso_p%iso_opacity(1:pvr_iso_p%num_isosurf) =   zero
!
      end subroutine alloc_pvr_isosurf_param
!
! -----------------------------------------------------------------------
!
      subroutine set_control_pvr_isosurf                                &
     &         (i, num_nod_phys, phys_nod_name,                         &
     &          pvr_field_ctl, pvr_comp_ctl, pvr_iso_ctl, pvr_iso_p)
!
      use m_more_component_flags
      use t_ctl_data_pvr_isosurface
      use t_control_array_character
      use pvr_surface_enhancement
!
      integer(kind = kint), intent(in) ::  i
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
      type(read_character_item), intent(in) :: pvr_field_ctl
      type(read_character_item), intent(in) :: pvr_comp_ctl
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
      type(pvr_isosurf_parameter), intent(inout) :: pvr_iso_p
!
      character(len = kchara) :: tmpchara
!
!
      if(pvr_iso_ctl%isosurf_data_ctl%iflag .le. 0) then
        pvr_iso_ctl%isosurf_data_ctl%charavalue                         &
     &                                 = pvr_field_ctl%charavalue
        pvr_iso_ctl%isosurf_comp_ctl%charavalue                         &
     &                                 = pvr_comp_ctl%charavalue
        pvr_iso_ctl%isosurf_data_ctl%iflag = 1
        pvr_iso_ctl%isosurf_comp_ctl%iflag = 1
      end if
!
      if(pvr_iso_ctl%isosurf_comp_ctl%iflag .le. 0) then
        pvr_iso_ctl%isosurf_data_ctl%charavalue = mag_flags(1)
        pvr_iso_ctl%isosurf_data_ctl%iflag = 1
      end if
!
      call set_control_field_4_pvr                                      &
     &   (pvr_iso_ctl%isosurf_data_ctl, pvr_iso_ctl%isosurf_comp_ctl,   &
     &    num_nod_phys, phys_nod_name,                                  &
     &    pvr_iso_p%iso_fld_param(i), pvr_iso_p%icheck_iso_ncomp(i))
!
      if(pvr_iso_ctl%iso_value_ctl%iflag .gt. 0) then
        pvr_iso_p%iso_value(i) = pvr_iso_ctl%iso_value_ctl%realvalue
      end if
!
      if(pvr_iso_ctl%opacity_ctl%iflag .gt. 0) then
        pvr_iso_p%iso_opacity(i) = pvr_iso_ctl%opacity_ctl%realvalue
      end if
!
      if(pvr_iso_ctl%isosurf_type_ctl%iflag .gt. 0) then
        tmpchara = pvr_iso_ctl%isosurf_type_ctl%charavalue
        if(cmp_no_case(tmpchara, LABEL_DECREASE)) then
          pvr_iso_p%itype_isosurf(i) = IFLAG_SHOW_REVERSE
        else if(cmp_no_case(tmpchara, LABEL_DECREASE)) then
          pvr_iso_p%itype_isosurf(i) = IFLAG_SHOW_FORWARD
        else
          pvr_iso_p%itype_isosurf(i) = IFLAG_SHOW_FORWARD
        end if
      end if
!
      end subroutine set_control_pvr_isosurf
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_pvr_isosurf
