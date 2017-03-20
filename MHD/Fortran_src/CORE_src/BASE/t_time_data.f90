!>@file   t_time_data.f90
!!@brief  module t_time_data
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  time and time step data for data IO
!!
!!@verbatim
!!      subroutine reset_time_data(time_d)
!!        type(time_data), intent(inout) :: time_d
!!      subroutine copy_time_step_size_data(time_org, time_new)
!!        type(time_data), intent(in) ::    time_org
!!        type(time_data), intent(inout) :: time_new
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
!!@n @param  id_file   file ID for data IO
!
      module t_time_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!
!>      Structure for time data
      type time_data
!>        Time step
        integer(kind = kint) :: i_time_step
!>        Time                  @f$ t @f$
        real(kind = kreal) :: time
!>        Length of time step   @f$ \Delta t @f$
        real(kind = kreal) :: dt
      end type time_data
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine reset_time_data(time_d)
!
      type(time_data), intent(inout) :: time_d
!
!
      time_d%i_time_step = izero
      time_d%time =        zero
      time_d%dt =          zero
!
      end subroutine reset_time_data
!
! -------------------------------------------------------------------
!
      subroutine copy_time_step_size_data(time_org, time_new)
!
      type(time_data), intent(in) ::    time_org
      type(time_data), intent(inout) :: time_new
!
!
      time_new%i_time_step = time_org%i_time_step
      time_new%time =        time_org%time
      time_new%dt =          time_org%dt
!
      end subroutine copy_time_step_size_data
!
!  ---------------------------------------------------------------------
!
      end module t_time_data
