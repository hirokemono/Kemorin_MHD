!m_ctl_data_temp_model.f90
!      module m_ctl_data_temp_model
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine read_temp_control
!
!    begin temperature_define
!!!!!!!!! model for stratification !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    ref_temp_ctl: none           (No reference of temperature)
!                  spherical_shell ( for spherical shell model)
!                  linear_x        ( propotional to x-direction )
!                  linear_y        ( propotional to x-direction )
!                  linear_z        ( propotional to x-direction )
!
!
!    stratified_ctl:   0...off  1...on
!     stratified_sigma_ctl: intense ofstratification
!     stratified_width_ctl: width of stratification
!     stratified_outer_r_ctl: outer boundary of stratification
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      ref_temp_ctl              spherical_shell
!      begin low_temp_ctl
!           depth         1.5384615384615384
!           temperature   0.0d0
!      end  low_temp_ctl
!      begin high_temp_ctl
!           depth         0.5384615384615384
!           temperature   1.0d0
!      end  high_temp_ctl
!
!      stratified_ctl            Off
!      stratified_sigma_ctl         0.000   end
!      stratified_width_ctl         0.000   end
!      stratified_outer_r_ctl       0.000   end
!    end  temperature_define
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      module m_ctl_data_temp_model
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_temp_model
!
      implicit  none
!
!
      type(reference_temperature_ctl), save :: reft_ctl1
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_temp_def =     'temperature_define'
      integer (kind=kint) :: i_temp_def =      0
!
      private :: hd_temp_def, i_temp_def
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_temp_control
!
!
      call read_reftemp_ctl(hd_temp_def, i_temp_def, reft_ctl1)
!
      end subroutine read_temp_control
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_temp_model
