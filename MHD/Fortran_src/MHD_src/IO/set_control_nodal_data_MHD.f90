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
!!     subroutine set_control_4_fields(nod_fld)
!!     subroutine add_nodal_fields_2_ctl
!!@endverbatim
!
      module set_control_nodal_data_MHD
!
      use m_precision
!
      use m_machine_parameter
      use m_ctl_data_4_fields
!
      use t_phys_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_fields(nod_fld)
!
      use calypso_mpi
      use m_error_IDs
      use m_element_phys_data
!
      use set_control_nodal_data
      use add_nodal_fields_4_MHD
!
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
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)               &
     &    'original field_ctl%num ', field_ctl%num
!
!
      if (field_ctl%num .gt. 0) then
!
!     add terms for MHD
!
        call add_field_name_4_mhd
        call add_field_name_4_fem_mhd
        if (iflag_debug .ge. iflag_routine_msg) write(*,*)              &
     &    'num_nod_phys after modified ', field_ctl%num
!
!    set nodal data
!
        call s_set_control_nodal_data(nod_fld, ierr)
      end if
!
      call set_ele_field_names_MHD(nod_fld)
!
      end subroutine set_control_4_fields
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_fem_mhd
!
      use add_nodal_fields_4_SGS
!
!
!     set work fields for SGS models
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &               write(*,*) 'add_work_area_4_sgs_model'
      call add_work_area_4_sgs_model
!
      end subroutine add_field_name_4_fem_mhd
!
! -----------------------------------------------------------------------
!
      end module set_control_nodal_data_MHD
