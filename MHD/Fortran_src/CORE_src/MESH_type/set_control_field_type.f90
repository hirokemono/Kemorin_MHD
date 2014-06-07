!set_control_field_type.f90
!      module set_control_field_type
!
!        programmed by H.Matsui on Sep., 2006
!
!     subroutine set_control_field_data_type(fld, ierr, error_message)
!        type(phys_data), intent(inout) :: fld
!        integer (kind = kint), intent(inout) :: ierr
!        character(len=128), intent(inout) :: error_message
!
!     subroutine ordering_field_type_by_viz(fld)
!     subroutine ordering_field_type_by_comp_viz(fld)
!
      module set_control_field_type
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_field_data_type(fld, ierr, error_message)
!
      use m_machine_parameter
      use m_ctl_data_4_fields
      use t_phys_data
!
      type(phys_data), intent(inout) :: fld
      integer (kind = kint), intent(inout) :: ierr
      character(len=128), intent(inout) :: error_message
!
!
!   set physical values
!
      if(field_ctl%icou .le. 0) then
        error_message = 'Set field for simulation'
        ierr = 90
      else
        fld%num_phys = field_ctl%num
        ierr = 0
      end if
!
!    set nodal data
!
      if ( fld%num_phys .gt. 0 ) then
        call alloc_phys_name_type(fld)
        call ordering_field_type_by_viz(fld)
!
        if (iflag_debug.eq.1) call check_nodal_field_name_type(fld)
        call deallocate_phys_control
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'fld%num_phys ', fld%num_phys
!
      end subroutine set_control_field_data_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ordering_field_type_by_viz(fld)
!
      use t_phys_data
      use node_monitor_IO
      use ordering_field_by_viz
!
      type(phys_data), intent(inout) :: fld
!
!
      call s_ordering_field_by_viz(fld%num_phys, fld%num_phys_viz,      &
     &    fld%num_component, fld%phys_name, fld%iflag_monitor)
!
      call set_istack_4_nodal_field(fld%num_phys, fld%num_phys_viz,     &
     &    fld%num_component, fld%ntot_phys, fld%ntot_phys_viz,          &
     &    fld%istack_component)
!
      end subroutine ordering_field_type_by_viz
!
! -----------------------------------------------------------------------
!
      subroutine ordering_field_type_by_comp_viz(fld)
!
      use t_phys_data
      use node_monitor_IO
      use ordering_field_by_viz
!
      type(phys_data), intent(inout) :: fld
!
!
      call ordering_field_by_comp_viz(fld%num_phys, fld%num_phys_viz,   &
     &    fld%num_component, fld%phys_name, fld%iflag_monitor)
!
      call set_istack_4_nodal_field(fld%num_phys, fld%num_phys_viz,     &
     &    fld%num_component, fld%ntot_phys, fld%ntot_phys_viz,          &
     &    fld%istack_component)
!
      end subroutine ordering_field_type_by_comp_viz
!
! -----------------------------------------------------------------------
!
      end module set_control_field_type
