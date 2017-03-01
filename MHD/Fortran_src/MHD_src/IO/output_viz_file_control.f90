!
!     module output_viz_file_control
!
!     programmed by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!!      integer(kind = kint) function lead_field_data_flag()
!!      integer(kind = kint) function viz_file_step_4_flex(viz_step)
!!        integer(kind=kint ), intent(inout) :: visval
!
      module output_viz_file_control
!
      use m_machine_parameter
      use m_precision
!
      use m_constants
      use m_t_step_parameter
!
      implicit none
!
      private :: set_viz_flex_file_step
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function lead_field_data_flag()
!
      use set_exit_flag_4_visualizer
!
      integer (kind =kint) :: i_monitor, i_bulk, i_udt, i_coef, irst
!
!
      lead_field_data_flag = 1
      call set_output_flag_4_viz(istep_max_dt, lead_field_data_flag)
!
      call set_output_flag(irst, istep_max_dt, i_step_output_rst)
      call set_output_flag(i_bulk, istep_max_dt, i_step_check)
      call set_output_flag(i_udt, istep_max_dt, i_step_output_ucd)
      call set_output_flag(i_monitor, istep_max_dt,                     &
     &    i_step_output_monitor)
!
      call set_output_flag(i_coef, istep_max_dt, i_step_sgs_output)
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
!-----------------------------------------------------------------------
!
      integer(kind = kint) function viz_file_step_4_flex(viz_step)
!
      use m_t_int_parameter
!
      type(VIZ_step_params), intent(inout) :: viz_step
!
      integer(kind=kint ) :: ivis_pvr, ivis_psf, ivis_iso, ivis_fline
!
!
      call set_viz_flex_file_step(time, dt, delta_t_output_psf,         &
     &    ivis_psf, viz_step%PSF_t%istep_file )
      call set_viz_flex_file_step(time, dt, delta_t_output_iso,         &
     &    ivis_iso, viz_step%ISO_t%istep_file)
      call set_viz_flex_file_step(time, dt, delta_t_output_pvr,         &
     &    ivis_pvr, viz_step%PVR_t%istep_file)
      call set_viz_flex_file_step(time, dt, delta_t_output_fline,       &
     &    ivis_fline, viz_step%FLINE_t%istep_file)
!
      viz_file_step_4_flex = ivis_psf * ivis_iso
!
      end function viz_file_step_4_flex
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_viz_flex_file_step(time, dt, dt_viz, iviz, i_cnt)
!
      real(kind = kreal), intent(in) :: time, dt, dt_viz
      integer(kind = kint), intent(inout) :: iviz, i_cnt
!
      integer(kind = kint) :: istep, iref
!
!
      istep = int(time / dt)
      if ( dt_viz .eq. zero) then
        iviz =   ione
        i_cnt = -ione
      else
         iref =  int(dt_viz / dt)
         iviz = mod(istep, iref)
        if (iviz .eq. izero) then
          i_cnt = istep / iref
        else 
          i_cnt = -ione
        end if
      end if
!
      end subroutine set_viz_flex_file_step
!
! -----------------------------------------------------------------------
!
      end module output_viz_file_control
