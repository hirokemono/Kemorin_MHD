!>@file   set_control_field_data.f90
!!@brief  module set_control_field_data
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on  Aug., 2006
!
!>@brief  Ordering field data by visualization flag
!!
!!@verbatim
!!      subroutine s_set_control_field_data(field_ctl, fld, ierr)
!!      subroutine set_control_field_by_comp_viz(field_ctl, fld, ierr)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!        type(phys_data), intent(inout) :: fld
!!
!!      subroutine init_field_data(n_point, fld, iphys)
!!        type(phys_data), intent(inout) :: fld
!!        type(phys_address), intent(inout) :: iphys
!!
!!      subroutine count_field_4_monitor                                &
!!     &         (fld, num_field_monitor, ntot_comp_monitor)
!!        type(phys_data), intent(in) :: fld
!!@endverbatim
!!
!!@n @param fld    structure of field data
!!@n @param iphys  structure of field addresses
!
      module set_control_field_data
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_error_IDs
!
      use t_phys_address
      use t_phys_data
      use t_control_array_character3
!
      implicit  none
!
      private :: s_ordering_field_by_viz, ordering_field_by_comp_viz
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_field_data(field_ctl, fld, ierr)
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
      integer (kind = kint), intent(inout) :: ierr
!
      integer(kind = kint), parameter :: id_six = 6
!
!
      ierr = 0
      if(field_ctl%icou .le. 0) then
        e_message = 'Set field for simulation'
        ierr = ierr_file
        return
      end if
!
!    set nodal data
      call s_ordering_field_by_viz(field_ctl, fld)
!
      if(fld%num_phys .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'check_nodal_field_name_type for fld'
          call check_nodal_field_name_type(id_six, fld)
        end if
      end if
!
      end subroutine s_set_control_field_data
!
! -----------------------------------------------------------------------
!
      subroutine set_control_field_by_comp_viz(field_ctl, fld, ierr)
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
      integer (kind = kint), intent(inout) :: ierr
!
      integer(kind = kint), parameter :: id_six = 6
!
!
      ierr = 0
      if(field_ctl%icou .le. 0) then
        e_message = 'Set field for simulation'
        ierr = ierr_file
        return
      end if
!
!    set nodal data
      call ordering_field_by_comp_viz(field_ctl, fld)
!
      if(fld%num_phys .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'check_nodal_field_name_type for fld'
          call check_nodal_field_name_type(id_six, fld)
        end if
      end if
!
      end subroutine set_control_field_by_comp_viz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_field_data(n_point, fld, iphys)
!
      use set_MHD_field_address
      use set_SGS_MHD_field_address
!
      integer(kind = kint), intent(in) :: n_point
      type(phys_data), intent(inout) :: fld
!
      type(phys_address), intent(inout) :: iphys
!
      integer(kind = kint) :: i, i_fld
      logical :: flag
!
!
      call alloc_phys_data_type(n_point, fld)
!
      do i = 1, fld%num_phys
        i_fld = fld%istack_component(i-1) + 1
!
        call set_MHD_field_addresses                                    &
     &     (i_fld, fld%phys_name(i), iphys, flag)
        if(flag) cycle
        call set_SGS_MHD_field_addresses                                &
     &     (i_fld, fld%phys_name(i), iphys, flag)
        if(flag) cycle
!
!   Old field label... Should be deleted later!!
        call set_old_MHD_field_addresses                                &
     &     (i_fld, fld%phys_name(i), iphys, flag)
      end do
!
      end subroutine init_field_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_ordering_field_by_viz(field_ctl, fld)
!
      use set_nodal_field_name
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i
      logical :: flag
!
!
      fld%num_phys = 0
      call alloc_phys_name_type(fld)
!
      do i = 1, field_ctl%icou
        flag = .FALSE.
        call set_vector_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
        if(flag) cycle
        call set_scalar_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
        if(flag) cycle
        call set_tensor_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      end subroutine s_ordering_field_by_viz
!
! -----------------------------------------------------------------------
!
      subroutine ordering_field_by_comp_viz(field_ctl, fld)
!
      use set_nodal_field_name
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) :: i
      logical :: flag
!
!
      fld%num_phys = 0
      call alloc_phys_name_type(fld)
!
      do i = 1, field_ctl%icou
        call set_vector_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      do i = 1, field_ctl%icou
        call set_scalar_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      do i = 1, field_ctl%icou
        call set_tensor_field_name(field_ctl%c1_tbl(i),                 &
     &      check_vis_control_flag(field_ctl%c2_tbl(i)),                &
     &      check_monitor_control_flag(field_ctl%c3_tbl(i)), fld, flag)
      end do
!
      end subroutine ordering_field_by_comp_viz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_field_4_monitor                                  &
     &         (fld, num_field_monitor, ntot_comp_monitor)
!
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint), intent(inout) :: num_field_monitor
      integer(kind = kint), intent(inout) :: ntot_comp_monitor
!
      integer(kind = kint) :: i
!
!    count number of components for monitoring
!
      num_field_monitor = 0
      ntot_comp_monitor = 0
      do i = 1, fld%num_phys
        if(fld%flag_monitor(i)) then
          num_field_monitor = num_field_monitor + 1
          ntot_comp_monitor = ntot_comp_monitor + fld%num_component(i)
        end if
      end do
!
      end subroutine count_field_4_monitor
!
! -----------------------------------------------------------------------
!
      end module set_control_field_data
