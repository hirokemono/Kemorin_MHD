!
!      module input_control_gen_z_filter
!
      module input_control_gen_z_filter
!
!     Written by H. Matsui on June, 2007
!
      use m_precision
!
      implicit none
!
!
!     subroutine s_input_control_4_z_commute
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_4_z_commute
!
      use m_machine_parameter
      use calypso_mpi
!
      use read_ctl_gen_z_filter
      use set_ctl_gen_z_filter
      use const_geometry_z_commute
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_z_filter'
      call read_control_4_z_filter
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_gen_z_filter'
      call set_ctl_params_4_gen_z_filter
!
!  --  set geometry
!
      if (iflag_debug.eq.1) write(*,*) 'set_geometry_z_commute'
      call set_geometry_z_commute
      if (iflag_debug.eq.1) write(*,*) 'set_geometry_z_commute end'
!
      end subroutine s_input_control_4_z_commute
!
! ----------------------------------------------------------------------
!
      end module input_control_gen_z_filter
