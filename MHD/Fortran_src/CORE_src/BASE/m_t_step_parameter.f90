!>@file   m_t_step_parameter.f90
!!@brief  module m_t_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!
      module  m_t_step_parameter
!
!
      use m_precision
      use t_IO_step_parameter
      use t_VIZ_step_parameter
!
      implicit  none
!
!>      Time
      real(kind=kreal) :: time
!>      Time for the initail step
      real(kind=kreal) :: time_init
! 
!>      Time step
      integer(kind=kint) :: i_step_MHD
!>      Maximum length of time step
      integer(kind=kint) :: istep_max_dt
!>      Flexible time step number for maximum lenth of each step
      integer(kind=kint) :: istep_flex_to_max = 0
! 
!>      Elapsed time to terminate simulation
      real(kind=kreal)   :: elapsed_time
! 
!>      Flag for initial step to use Euler scheme
!!      insted of Adams-BAshforth
      integer(kind=kint) :: iflag_initial_step = 0
!
!>      Start time step
      integer(kind=kint) :: i_step_init
!>      End time steo
      integer(kind=kint) :: i_step_number
! 
!>      Start step for restarting file
      integer(kind=kint) :: istep_rst_start
!>      End step for restarting file
      integer(kind=kint) :: istep_rst_end
!
!
      type(IO_step_param), save :: rst_step1
!
      type(IO_step_param), save :: ucd_step1
!
      type(IO_step_param), save :: rms_step1
!
      type(IO_step_param), save :: point_step1
!
      type(IO_step_param), save :: boundary_step1
!
      type(VIZ_step_params), save :: viz_step1
!
!      pvr_step1%increment
!
      end module  m_t_step_parameter
