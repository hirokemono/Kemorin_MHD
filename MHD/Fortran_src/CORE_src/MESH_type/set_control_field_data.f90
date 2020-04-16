!set_control_field_data.f90
!      module set_control_field_data
!
!        programmed by H.Matsui on Sep., 2006
!
!!     subroutine s_set_control_field_data(field_ctl, fld, ierr)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!        type(phys_data), intent(inout) :: fld
!!        integer (kind = kint), intent(inout) :: ierr
!!
!!     subroutine ordering_field_type_by_comp_viz(fld)
!
      module set_control_field_data
!
      use m_precision
      use t_phys_data
      use t_control_array_character3
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_field_data(field_ctl, fld, ierr)
!
      use m_machine_parameter
      use m_error_IDs
      use ordering_field_by_viz
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
      integer (kind = kint), intent(inout) :: ierr
!
      integer(kind = kint), parameter :: id_six = 6
!
!   set physical values
!
      if(field_ctl%icou .le. 0) then
        e_message = 'Set field for simulation'
        ierr = ierr_file
      else
        fld%num_phys = field_ctl%num
        ierr = 0
      end if
!
!    set nodal data
!
      if ( fld%num_phys .gt. 0 ) then
        call alloc_phys_name_type(fld)
        call s_ordering_field_by_viz(field_ctl, fld)
        call set_istack_4_nodal_field(fld)
!
        if(iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'check_nodal_field_name_type for fld'
          call check_nodal_field_name_type(id_six, fld)
        end if
      end if
!
      end subroutine s_set_control_field_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ordering_field_type_by_comp_viz(field_ctl, fld)
!
      use node_monitor_IO
      use ordering_field_by_viz
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
!
      call ordering_field_by_comp_viz(field_ctl, fld)
      call set_istack_4_nodal_field(fld)
!
      end subroutine ordering_field_type_by_comp_viz
!
! -----------------------------------------------------------------------
!
      end module set_control_field_data
