!>@file   set_field_data_w_SGS.f90
!!@brief  module set_field_data_w_SGS
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui in  Aug., 2006
!!@n     Modified by T. Kera in    Aug., 2021
!
!>@brief  Ordering field data by visualization flag
!!
!!@verbatim
!!      subroutine set_SGS_field_ctl_by_viz(field_ctl, fld, ierr)
!!      subroutine set_SGS_field_ctl_by_comp_viz(field_ctl, fld, ierr)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!        type(phys_data), intent(inout) :: fld
!!
!!      subroutine init_field_data_w_SGS(n_point, fld, iphys, iphys_LES)
!!        type(phys_data), intent(inout) :: fld
!!        type(phys_address), intent(inout) :: iphys
!!        type(SGS_model_addresses), intent(inout) :: iphys_LES
!!@endverbatim
!!
!!@n @param fld    structure of field data
!!@n @param iphys  structure of field addresses
!
      module set_field_data_w_SGS
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
      integer(kind = kint), parameter, private :: id_std = 6
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_SGS_field_ctl_by_viz(field_ctl, fld, ierr)
!
      use set_field_name_w_SGS
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
      integer (kind = kint), intent(inout) :: ierr
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
      call ordering_fld_w_SGS_by_viz(field_ctl, fld)
!
      if(fld%num_phys .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'check_nodal_field_name for fld'
          call check_nodal_field_name(id_std, fld)
        end if
      end if
!
      end subroutine set_SGS_field_ctl_by_viz
!
! -----------------------------------------------------------------------
!
      subroutine set_SGS_field_ctl_by_comp_viz(field_ctl, fld, ierr)
!
      use set_field_name_w_SGS
!
      type(ctl_array_c3), intent(in) :: field_ctl
      type(phys_data), intent(inout) :: fld
      integer (kind = kint), intent(inout) :: ierr
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
      call ordering_fld_w_SGS_by_comp_viz(field_ctl, fld)
!
      if(fld%num_phys .gt. 0) then
        if(iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'check_nodal_field_name for fld'
          call check_nodal_field_name(id_std, fld)
        end if
      end if
!
      end subroutine set_SGS_field_ctl_by_comp_viz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_field_data_w_SGS(n_point, fld, iphys, iphys_LES)
!
      use set_MHD_field_address
      use set_MHD_sym_fld_address
      use set_SGS_MHD_field_address
!
      integer(kind = kint), intent(in) :: n_point
      type(phys_data), intent(inout) :: fld
!
      type(phys_address), intent(inout) :: iphys
      type(SGS_model_addresses), intent(inout) :: iphys_LES
!
      integer(kind = kint) :: i, i_fld
      logical :: flag
!
!
      call alloc_phys_data(n_point, fld)
!
      do i = 1, fld%num_phys
        i_fld = fld%istack_component(i-1) + 1
!
        call set_MHD_field_addresses                                    &
     &     (i_fld, fld%phys_name(i), iphys, flag)
        if(flag) cycle
        call set_SGS_MHD_field_addresses                                &
     &     (i_fld, fld%phys_name(i), iphys_LES, flag)
        if(flag) cycle
!
!
        call set_MHD_sym_field_addresses                                &
     &     (i_fld, fld%phys_name(i), iphys, flag)
        if(flag) cycle
!
!   Old field label... Should be deleted later!!
        call set_old_MHD_field_addresses                                &
     &     (i_fld, fld%phys_name(i), iphys, flag)
      end do
!
      end subroutine init_field_data_w_SGS
!
! -----------------------------------------------------------------------
!
      end module set_field_data_w_SGS
