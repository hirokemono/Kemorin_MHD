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
!!      subroutine set_control_4_fields(FEM_prm, SGS_par, MHD_prop,     &
!!     &          field_ctl, nod_fld, ele_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!        type(phys_data), intent(inout) :: nod_fld, ele_fld
!!@endverbatim
!
      module set_control_nodal_data_MHD
!
      use m_precision
!
      use m_machine_parameter
!
      use t_phys_data
      use t_control_parameter
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_fields(FEM_prm, SGS_par, MHD_prop,       &
     &          field_ctl, nod_fld, ele_fld)
!
      use calypso_mpi
      use m_error_IDs
      use t_control_array_character3
      use t_FEM_control_parameter
      use t_SGS_control_parameter
!
      use set_control_nodal_data
      use add_nodal_fields_4_MHD
      use add_nodal_fields_4_SGS
      use initialize_element_field
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(ctl_array_c3), intent(inout) :: field_ctl
      type(phys_data), intent(inout) :: nod_fld, ele_fld
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
        call add_field_name_4_mhd(MHD_prop, field_ctl)
        call add_ctl_4_ref_temp                                         &
     &     (MHD_prop%ref_param_T, MHD_prop%ref_param_C, field_ctl)
!
!     set work fields for SGS models
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &               write(*,*) 'add_work_area_4_sgs_model'
        call add_work_area_4_sgs_model                                  &
     &     (SGS_par%model_p, MHD_prop%fl_prop, field_ctl)
!
        if (iflag_debug .ge. iflag_routine_msg) write(*,*)              &
     &    'num_nod_phys after modified ', field_ctl%num
!
!    set nodal data
!
        call s_set_control_nodal_data(field_ctl, nod_fld, ierr)
      end if
!
      call set_ele_field_names_MHD                                      &
     &   (FEM_prm, SGS_par%model_p, nod_fld, ele_fld)
!
      end subroutine set_control_4_fields
!
! -----------------------------------------------------------------------
!
      end module set_control_nodal_data_MHD
