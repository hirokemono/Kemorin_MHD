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
!!     subroutine set_control_4_fields
!!     subroutine add_nodal_fields_2_ctl
!!     subroutine check_FEM_MHD_dependencies
!!@endverbatim
!
      module set_control_nodal_data_MHD
!
      use m_precision
!
      use m_machine_parameter
      use m_ctl_data_4_fields
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_fields
!
      use calypso_mpi
      use m_error_IDs
      use m_node_phys_data
      use m_element_phys_data
!
      use set_control_nodal_data
      use add_nodal_fields_4_MHD
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
        call s_set_control_nodal_data(nod_fld1, ierr)
      end if
!
      call set_ele_field_names_MHD
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
      subroutine check_FEM_MHD_dependencies
!
      use m_node_phys_data
      use node_monitor_IO
      use ordering_field_by_viz
      use check_dependency_for_MHD
!
!
      call count_field_4_monitor                                        &
     &   (nod_fld1%num_phys, nod_fld1%num_component,                    &
     &    nod_fld1%iflag_monitor, num_field_monitor, ntot_comp_monitor)
!
      call check_dependencies(nod_fld1%num_phys, nod_fld1%phys_name)
!
      end subroutine check_FEM_MHD_dependencies
!
! -----------------------------------------------------------------------
!
      end module set_control_nodal_data_MHD
