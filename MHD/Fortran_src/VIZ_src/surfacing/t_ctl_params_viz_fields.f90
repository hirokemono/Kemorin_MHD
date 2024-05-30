!>@file   t_ctl_params_viz_fields.f90
!!@brief  module t_ctl_params_viz_fields
!!
!!@date  Programmed by H.Matsui in Aug. 2011
!
!>@brief control parameters for each field line
!!
!!@verbatim
!!      subroutine dealloc_ctl_params_viz_fields(viz_fields)
!!        type(ctl_params_viz_fields), intent(inout) :: viz_fields
!!      subroutine set_ctl_params_viz_fields(field_output_ctl, nod_fld, &
!!     &                                     viz_fields)
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_array_c2), intent(in) :: field_output_ctl
!!       type(ctl_params_viz_fields), intent(inout) :: viz_fields
!!@endverbatim
!
      module t_ctl_params_viz_fields
!
      use m_precision
!
      implicit  none
!
!
!>        control parameter for vizulization field output
      type ctl_params_viz_fields
!>        number of field for coloring
        integer(kind = kint) :: num_color_fields = 0
!>        number of field for coloring
        integer(kind = kint) :: ntot_color_comp =  0
!>        Field name for fieldline color
        character(len = kchara), allocatable :: color_field_name(:)
!>        Field address for fieldline color
        integer(kind = kint), allocatable :: ifleld_color_field(:)
!>        component address for fieldline color
        integer(kind = kint), allocatable :: icomp_color_field(:)
!>        number of component for fieldline color
        integer(kind = kint), allocatable :: istack_color_field(:)
!>        number of component for fieldline color
        integer(kind = kint), allocatable :: ncomp_color_field(:)
!>        number of component for fieldline color
        integer(kind = kint), allocatable :: ncomp_org_color_field(:)
      end type ctl_params_viz_fields
!
      private :: alloc_ctl_params_viz_fields
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ctl_params_viz_fields(num_field, viz_fields)
!
      integer(kind = kint), intent(in) :: num_field
      type(ctl_params_viz_fields), intent(inout) :: viz_fields
!
      viz_fields%num_color_fields = num_field
      allocate(viz_fields%color_field_name(num_field))
      allocate(viz_fields%ifleld_color_field(num_field))
      allocate(viz_fields%icomp_color_field(num_field))
      allocate(viz_fields%istack_color_field(0:num_field))
      allocate(viz_fields%ncomp_color_field(num_field))
      allocate(viz_fields%ncomp_org_color_field(num_field))
!
      end subroutine alloc_ctl_params_viz_fields
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ctl_params_viz_fields(viz_fields)
!
      type(ctl_params_viz_fields), intent(inout) :: viz_fields
!
      deallocate(viz_fields%color_field_name)
      deallocate(viz_fields%ifleld_color_field)
      deallocate(viz_fields%icomp_color_field)
      deallocate(viz_fields%istack_color_field)
      deallocate(viz_fields%ncomp_color_field)
      deallocate(viz_fields%ncomp_org_color_field)
!
      end subroutine dealloc_ctl_params_viz_fields
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_params_viz_fields(field_output_ctl, nod_fld,   &
     &                                     viz_fields)
!
      use t_phys_data
      use t_control_array_character2
      use set_components_flags
      use set_field_comp_for_viz
!
      type(phys_data), intent(in) :: nod_fld
      type(ctl_array_c2), intent(in) :: field_output_ctl
      type(ctl_params_viz_fields), intent(inout) :: viz_fields
!
      integer(kind = kint) :: i
!
!
      if(field_output_ctl%num .le. izero) then
        call alloc_ctl_params_viz_fields (ione, viz_fields)
      else
        call alloc_ctl_params_viz_fields                              &
     &     (field_output_ctl%num, viz_fields)
      end if
!
      if(field_output_ctl%num .le. izero) then
        viz_fields%color_field_name(1) = 'data'
        viz_fields%ifleld_color_field = -1
        viz_fields%icomp_color_field =  -1
        viz_fields%ncomp_color_field = 1
        viz_fields%ncomp_org_color_field = 0
      else
        call set_components_4_viz                                       &
     &     (nod_fld%num_phys, nod_fld%phys_name, field_output_ctl%num,  &
     &      field_output_ctl%c1_tbl, field_output_ctl%c2_tbl,           &
     &      viz_fields%num_color_fields, viz_fields%ifleld_color_field, &
     &      viz_fields%icomp_color_field, viz_fields%ncomp_color_field, &
     &      viz_fields%ncomp_org_color_field,                           &
     &      viz_fields%color_field_name)
      end if
      viz_fields%istack_color_field(0) = 0
      do i = 1, viz_fields%num_color_fields
        viz_fields%istack_color_field(i)                                &
     &        = viz_fields%istack_color_field(i-1)                      &
     &         + viz_fields%ncomp_color_field(i)
      end do
      viz_fields%ntot_color_comp                                        &
     &     = viz_fields%istack_color_field(viz_fields%num_color_fields)
!
      end subroutine set_ctl_params_viz_fields
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_params_viz_fields
