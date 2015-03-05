!
!     module time_step_data_IO_control
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2007
!
!      subroutine output_time_step_control
!
      module time_step_data_IO_control
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
      use calypso_mpi
      use m_control_parameter
      use m_geometry_data
      use m_geometry_parameter
      use m_node_phys_address
      use m_t_step_parameter
      use m_t_int_parameter
      use m_bulk_values
!
      use int_norm_div_MHD
      use int_rms_div_MHD
      use estimate_stabilities
      use set_exit_flag_4_visualizer
      use int_bulk
      use time_step_file_IO
!
      integer (kind = kint) :: nd, ii
!
!
      call set_output_flag(ii, istep_max_dt, i_step_check)
!
      if ( ii .eq. 0 ) then
        if(my_rank .eq. 0) write(*,'(a10,i16,a10,e15.8)')               &
     &            'i_step=',i_step_MHD,'time=',time
!
        call s_int_bulk
!
        if  (iflag_t_evo_4_velo .gt. id_no_evolution) then
          call int_norm_div_v
          call int_rms_div_v
          call cal_stability_4_advect
        end if
        if  (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
          call int_norm_div_a
          call int_rms_div_a
        end if
        if  (iflag_t_evo_4_magne .gt. id_no_evolution                   &
     &         .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
          call int_norm_div_b
          call int_rms_div_b
        end if
!
!
        call MPI_allREDUCE (bulk_local, bulk_global, num_bulk,          &
     &      CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
        call MPI_allREDUCE (rms_local, rms_global, num_rms,             &
     &      CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
!
         do nd = 1, num_bulk
           bulk_global(nd) = bulk_global(nd) / rms_global(ivol)
         end do
         do nd = 1, num_rms - 1
           if (nd .eq. i_rms%i_velo                                     &
     &    .or. nd .eq. i_rms%i_magne .or. nd .eq. ir_me_ic              &
     &    .or. nd .eq. i_rms%i_vort                                     &
     &    .or. nd .eq. i_rms%i_current .or.  nd .eq. ir_sqj_ic          &
     &    .or. nd .eq. i_rms%i_filter_velo                              &
     &    .or. nd .eq. i_rms%i_filter_magne                             &
     &    .or. nd .eq. ir_me_f_ic) then
            rms_global(nd) = rms_global(nd) / rms_global(ivol)
          else
            rms_global(nd) = sqrt(rms_global(nd) / rms_global(ivol))
          end if
        end do
!
        call output_monitor_file(my_rank)
      end if
!
      end subroutine output_time_step_control
!
!  ---------------------------------------------------------------------
!
      end module time_step_data_IO_control
