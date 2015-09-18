!set_control_nodal_data.f90
!      module set_control_nodal_data
!
!        programmed by H.Matsui on Sep., 2006
!
!     subroutine s_set_control_nodal_data(ierr)
!
      module set_control_nodal_data
!
      use m_precision
!
      implicit  none
!
      private :: ordering_nod_field_by_viz
      private :: ordering_nod_field_by_comp_viz
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_nodal_data(ierr)
!
      use m_machine_parameter
      use m_error_IDs
      use m_ctl_data_4_fields
      use m_node_phys_data
!
      integer (kind = kint), intent(inout) :: ierr
!
!
!   set physical values
!
      ierr = 0
      if(field_ctl%icou .le. 0) then
        e_message = 'Set field for simulation'
        ierr = ierr_file
      else
        nod_fld1%num_phys = field_ctl%num
      end if
!
!    set nodal data
!
      if ( nod_fld1%num_phys .gt. 0 ) then
        call allocate_phys_name
        call ordering_nod_field_by_viz
!
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &      call check_nodal_field_name
        call deallocate_phys_control
      end if
!
      end subroutine s_set_control_nodal_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine ordering_nod_field_by_viz
!
      use m_node_phys_data
      use ordering_field_by_viz
!
!
      call s_ordering_field_by_viz                                      &
     &   (nod_fld1%num_phys, nod_fld1%num_phys_viz,                     &
     &    nod_fld1%num_component, phys_nod_name, iflag_nod_fld_monitor)
!
      call set_istack_4_nodal_field                                     &
     &   (nod_fld1%num_phys, nod_fld1%num_phys_viz,                     &
     &    nod_fld1%num_component, nod_fld1%ntot_phys,                   &
     &    nod_fld1%ntot_phys_viz, istack_nod_component)
!
      end subroutine ordering_nod_field_by_viz
!
! -----------------------------------------------------------------------
!
      subroutine ordering_nod_field_by_comp_viz
!
      use m_node_phys_data
      use ordering_field_by_viz
!
!
      call ordering_field_by_comp_viz                                   &
     &   (nod_fld1%num_phys, nod_fld1%num_phys_viz,                     &
     &    nod_fld1%num_component, phys_nod_name, iflag_nod_fld_monitor)
!
      call set_istack_4_nodal_field                                     &
     &   (nod_fld1%num_phys, nod_fld1%num_phys_viz,                     &
     &    nod_fld1%num_component, nod_fld1%ntot_phys,                   &
     &    nod_fld1%ntot_phys_viz, istack_nod_component)
!
      end subroutine ordering_nod_field_by_comp_viz
!
! -----------------------------------------------------------------------
!
      end module set_control_nodal_data
