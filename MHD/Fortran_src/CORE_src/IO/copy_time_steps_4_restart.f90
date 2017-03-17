!>@file   copy_time_steps_4_restart.f90
!!@brief  module copy_time_steps_4_restart
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Routines to copy time step information from IO data
!!
!!@verbatim
!!      subroutine copy_time_from_restart(t_IO)
!!      subroutine copy_init_time_from_restart(t_IO)
!!      subroutine copy_time_steps_from_restart(t_IO, init_d)
!!        type(time_params_IO), intent(in) :: t_IO
!!        type(time_data), intent(inout) :: time_d
!!
!!      subroutine copy_time_steps_to_restart(time_d, t_IO)
!!      subroutine copy_time_steps_from_field(t_IO, time_d)
!!        type(time_params_IO), intent(inout) :: t_IO
!!        type(time_data), intent(inout) :: time_d
!!@endverbatim
!
      module copy_time_steps_4_restart
!
      use m_precision
!
      use m_constants
      use t_time_data
      use t_time_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_from_restart(t_IO)
!
      use m_t_step_parameter
!
      type(time_params_IO), intent(in) :: t_IO
!
      time_init =   t_IO%time_IO
!
      end subroutine copy_time_from_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_init_time_from_restart(t_IO)
!
      use m_t_step_parameter
!
      type(time_params_IO), intent(in) :: t_IO
!
!
      call copy_time_from_restart(t_IO)
      i_step_init = t_IO%i_time_step_IO
!
      end subroutine copy_init_time_from_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_steps_from_restart(t_IO, init_d)
!
      type(time_params_IO), intent(in) :: t_IO
      type(time_data), intent(inout) :: init_d
!
!
      call copy_init_time_from_restart(t_IO)
!
      if(t_IO%delta_t_IO .gt. zero) init_d%dt = t_IO%delta_t_IO
!
      end subroutine copy_time_steps_from_restart
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine copy_time_steps_to_restart(time_d, t_IO)
!
      type(time_data), intent(in) :: time_d
      type(time_params_IO), intent(inout) :: t_IO
!
!
      t_IO%i_time_step_IO = time_d%i_time_step
      t_IO%time_IO =        time_d%time
      t_IO%delta_t_IO =     time_d%dt
!
      end subroutine copy_time_steps_to_restart
!
!  ---------------------------------------------------------------------
!
      subroutine copy_time_steps_from_field(t_IO, time_d)
!
      type(time_params_IO), intent(in) :: t_IO
      type(time_data), intent(inout) :: time_d
!
!
      time_d%i_time_step = t_IO%i_time_step_IO
      time_d%time =        t_IO%time_IO
      time_d%dt =          t_IO%delta_t_IO
!
      end subroutine copy_time_steps_from_field
!
!  ---------------------------------------------------------------------
!
      end module copy_time_steps_4_restart
