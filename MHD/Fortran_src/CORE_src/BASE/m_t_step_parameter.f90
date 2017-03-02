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
!>      Integer flag for flexible time stepping
      integer(kind= kint) :: iflag_flex_step_changed = 0
!>      
      integer(kind= kint) :: i_interval_flex_2_max
!
!>      Flag for initial step to use Euler scheme
!!      insted of Adams-BAshforth
      integer(kind=kint) :: iflag_initial_step = 0
!>      Flag for initial step for SGS model
      integer(kind=kint) :: iflag_SGS_initial =  1
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
!>      Increment of time step for monitoring output
      integer(kind=kint) :: i_step_check
!>      Time interval for monitoring output
      real(kind=kreal)   :: delta_t_step_check
! 
!>      Increment of time step for point data output
      integer(kind=kint) :: i_step_output_monitor
!>      Time interval for point data output
      real(kind=kreal)   :: delta_t_output_monitor
! 
!>      Increment of time step for boundary field data
      integer(kind=kint) :: i_step_output_boundary
!>      Time interval for boundary field data
      real(kind=kreal)   :: delta_t_output_boundary
! 
!>      Increment of time step for SGS model coefficients monitoring
      integer(kind=kint) :: i_step_sgs_output
!>      Time interval for SGS model coefficients monitoring
      real(kind=kreal)   :: delta_t_sgs_output
!>      Increment of time step for evaluation of SGS model coefficients
      integer(kind=kint) :: i_step_sgs_coefs
!
!
      type(IO_step_param), save :: rst_step1
!
      type(IO_step_param), save :: ucd_step1
!ucd_step1%delta_t
!
      type(VIZ_step_params), save :: viz_step1
!
!      pvr_step1%increment
!
      end module  m_t_step_parameter
