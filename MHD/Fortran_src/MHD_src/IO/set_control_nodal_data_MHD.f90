!
!>@file   set_control_nodal_data_MHD.f90
!!@brief  module set_control_nodal_data_MHD
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief Set field information for MHD simulation from control data
!!
!!@verbatim
!!     subroutine set_control_4_fields(field_ctl, nod_fld)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module set_control_nodal_data_MHD
!
      use m_precision
!
      use m_machine_parameter
!
      use t_phys_data
      use t_read_control_arrays
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_fields(field_ctl, nod_fld)
!
      use calypso_mpi
      use m_error_IDs
      use m_physical_property
      use m_element_phys_data
!
      use set_control_nodal_data
      use add_nodal_fields_4_MHD
      use add_nodal_fields_4_SGS
!
      type(ctl_array_c3), intent(inout) :: field_ctl
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: ierr
!
!   set physical values
!
      if(field_ctl%icou .le. 0) then
        e_message = 'Set field for simulation'
        call calypso_MPI_abort(ierr_fld, e_message)
      end if
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'original field_ctl%num ', field_ctl%num
!
!
      if (field_ctl%num .gt. 0) then
!
!     add terms for MHD
!
        call add_field_name_4_mhd                                       &
     &     (fl_prop1, ref_param_T1, ref_param_C1, field_ctl)
        call add_ctl_4_ref_temp                                         &
     &     (ref_param_T1, ref_param_C1, field_ctl)
!
!     set work fields for SGS models
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &               write(*,*) 'add_work_area_4_sgs_model'
        call add_work_area_4_sgs_model                                  &
     &     (SGS_par1%model_p, fl_prop1, field_ctl)
!
        if (iflag_debug .ge. iflag_routine_msg) write(*,*)              &
     &    'num_nod_phys after modified ', field_ctl%num
!
!    set nodal data
!
        call s_set_control_nodal_data(field_ctl, nod_fld, ierr)
      end if
!
      call set_ele_field_names_MHD(SGS_par1%model_p, nod_fld)
!
      end subroutine set_control_4_fields
!
! -----------------------------------------------------------------------
!
      end module set_control_nodal_data_MHD
