!
!     module time_step_data_IO
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2007
!
!      subroutine output_time_step_control
!      subroutine skip_time_step_data
!
      module time_step_data_IO
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine output_time_step_control
!
      use m_parallel_var_dof
      use m_control_parameter
      use m_geometry_data
      use m_geometry_parameter
      use m_node_phys_address
      use m_t_step_parameter
      use m_t_int_parameter
      use m_file_control_parameter
      use m_bulk_values
!
      use int_norm_div_MHD
      use int_rms_div_MHD
      use estimate_stabilities
      use set_exit_flag_4_visualizer
      use int_bulk
!
      integer (kind = kint) :: nd, ii
!
!
      call time_prog_barrier
!
      call set_output_flag(ii, istep_max_dt, i_step_check)
!
      if ( ii .eq. 0 ) then
        if(my_rank .eq. 0) write(*,'(a10,i10,a10,e15.8)')               &
     &            'i_step=',i_step_MHD,'time=',time
!
        call s_int_bulk
!
        if  ( iflag_t_evo_4_velo .ge. 1 ) then
          call int_norm_div_v
          call int_rms_div_v
          call cal_stability_4_advect
        end if
        if  ( iflag_t_evo_4_vect_p.ge.1 ) then
          call int_norm_div_a
          call int_rms_div_a
        end if
        if  ( iflag_t_evo_4_magne.ge.1                                  &
     &         .or. iflag_t_evo_4_vect_p.ge.1 ) then
          call int_norm_div_b
          call int_rms_div_b
        end if
!
!
        call MPI_allREDUCE (bulk_local, bulk_global, num_bulk,          &
     &      MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
        call MPI_allREDUCE (rms_local, rms_global, num_rms,             &
     &      MPI_DOUBLE_PRECISION, MPI_SUM, SOLVER_COMM, ierr)
!
!
         do nd = 1, num_bulk
           bulk_global(nd) = bulk_global(nd) / rms_global(num_rms)
         end do
         do nd = 1, num_rms - 1
           if (nd .eq. i_rms%i_velo                                     &
     &    .or. nd .eq. i_rms%i_magne .or. nd .eq. ir_me_ic              &
     &    .or. nd .eq. i_rms%i_vort                                     &
     &    .or. nd .eq. i_rms%i_current .or.  nd .eq. ir_sqj_ic          &
     &    .or. nd .eq. i_rms%i_filter_velo                              &
     &    .or. nd .eq. i_rms%i_filter_magne                             &
     &    .or. nd.eq.ir_me_f_ic) then
            rms_global(nd) = rms_global(nd) / rms_global(num_rms)
          else
            rms_global(nd) = sqrt(rms_global(nd) / rms_global(num_rms))
          end if
        end do
!
        if ( my_rank .eq. 0 ) then
!
          write(time_step_data_code,'(i10,1p1000e20.11)')               &
     &     i_step_MHD, time, bulk_global(1:num_bulk)
          write(rms_data_code,'(i10,1p100e20.11)')                      &
     &     i_step_MHD, time, rms_global(1:num_rms)
!
        end if
      end if
!
      end subroutine output_time_step_control
!
!  ---------------------------------------------------------------------
!
      subroutine skip_time_step_data
!
      use m_parallel_var_dof
      use m_t_step_parameter
      use m_file_control_parameter
      use m_bulk_values
!
      integer (kind = kint) :: i, iflag, i_read_step
      real(kind = kreal) :: rtmp
!
!
      iflag = i_step_init - mod(istep_max_dt, i_step_check)
      if ( my_rank .eq. 0 ) then
!
        do
          read(time_step_data_code,*,err=99,end=99)                     &
     &            i_read_step, rtmp, (rtmp,i=1,num_bulk)
          if (i_read_step .ge. i_step_init) exit
        end do
 99     continue
!
        do
          read(rms_data_code,*,err=98,end=98)                           &
     &            i_read_step, rtmp, (rtmp,i=1,num_rms)
          if (i_read_step .ge. iflag) exit
        end do
 98     continue
!
      end if
!
      end subroutine skip_time_step_data
!
!  ---------------------------------------------------------------------
!
      end module time_step_data_IO
