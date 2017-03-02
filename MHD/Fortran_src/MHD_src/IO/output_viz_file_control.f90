!
!     module output_viz_file_control
!
!     programmed by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      integer(kind = kint) function lead_field_data_flag(viz_step)
!!        type(VIZ_step_params), intent(in) :: viz_step
!
      module output_viz_file_control
!
      use m_machine_parameter
      use m_precision
!
      use m_constants
      use m_t_step_parameter
      use t_VIZ_step_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function lead_field_data_flag(viz_step)
!
      type(VIZ_step_params), intent(in) :: viz_step
!
      integer (kind =kint) :: i_monitor, i_bulk, i_udt, i_coef, irst
!
!
      lead_field_data_flag = 1
      call set_output_flag_4_viz                                        &
     &   (istep_max_dt, viz_step, lead_field_data_flag)
!
      irst = output_flag(istep_max_dt, i_step_output_rst)
      i_bulk = output_flag(istep_max_dt, i_step_check)
      i_udt = output_flag(istep_max_dt, i_step_output_ucd)
      i_monitor = output_flag(istep_max_dt, i_step_output_monitor)
!
      i_coef = output_flag(istep_max_dt, i_step_sgs_output)
!
      lead_field_data_flag = lead_field_data_flag                       &
     &                     * irst * i_udt * i_monitor * i_bulk * i_coef
!
      if (iflag_debug.eq.1) then
        write(*,*) 'irst: ', i_udt
        write(*,*) 'i_udt: ', i_udt
        write(*,*) 'i_monitor: ', i_monitor
        write(*,*) 'i_bulk: ', i_bulk
        write(*,*) 'i_coef: ', i_coef
      end if
!
      end function lead_field_data_flag
!
!-----------------------------------------------------------------------
!
      end module output_viz_file_control
