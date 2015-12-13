!
!      module input_control_gen_filter
!
!     Written by H. Matsui on July, 2006
!
!     subroutine input_control_3d_commute:(FEM_elen)
!
      module input_control_gen_filter
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_3d_commute(FEM_elen)
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_filter_elength
      use m_ctl_data_gen_3d_filter
      use set_ctl_gen_filter
      use load_mesh_data
!
      type(gradient_model_data_type), intent(inout) :: FEM_elen
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_gen_filter'
      call read_control_4_gen_filter
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_gen_filter'
      call set_ctl_params_gen_filter(FEM_elen)
!
!  --  read geometry
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh_1st'
      call input_mesh_1st(my_rank)
!
      end subroutine input_control_3d_commute
!
! ----------------------------------------------------------------------
!
      end module input_control_gen_filter
