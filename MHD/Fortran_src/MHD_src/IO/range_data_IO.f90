!
!      module range_data_IO
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2007
!
!      subroutine open_maximum_file(my_rank)
!      subroutine close_maximum_file(my_rank)
!
!      subroutine output_range_data
!      subroutine skip_range_data
!
      module range_data_IO
!
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine open_maximum_file(my_rank)
!
      use m_file_control_parameter
      use m_node_phys_data
      use m_cal_max_indices
!
      integer (kind = kint), intent(in) :: my_rank
!
      integer(kind = kint) :: i
!
!
      call allocate_phys_range
!
      if ( my_rank .gt. 0 ) return
!
      open (maximum_data_code,file = minmax_data_file_name,             &
     &      status='replace')
      open (maximum_position_code,file = minmax_posi_file_name,         &
     &      status='replace')
!
      write(maximum_data_code,'(a)')                                    &
     &    'ID step time x y z  min:  max: '
      write(maximum_position_code,'(a)')                                &
     &    'ID step time x y z  min_node:  max_node: '
!
      do i = 1, num_tot_nod_phys_vis
        write(maximum_data_code,'(a,a2)') trim(phys_nod_name(i)), ', '
        write(maximum_position_code,'(a,a2)')                           &
     &        trim(phys_nod_name(i)), ', '
      end do
!
      end subroutine open_maximum_file
!
! ----------------------------------------------------------------------
!
      subroutine close_maximum_file (my_rank)
!
      use m_file_control_parameter
      use m_cal_max_indices
!
      integer (kind = kint), intent(in) :: my_rank
!
!
      call deallocate_phys_range
!
      if ( my_rank .gt. 0 ) return
      close (maximum_data_code)
      close (maximum_position_code)
!
      end subroutine close_maximum_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine output_range_data
!
      use m_parallel_var_dof
      use m_file_control_parameter
      use m_node_phys_data
      use m_cal_max_indices
      use m_t_step_parameter
      use cal_max_indices
!
!
       call s_cal_max_indices
!
       if ( my_rank .eq. 0 ) then
!
         write(maximum_data_code,'(i10,1p250E25.15e3)')                 &
     &       ucd_step, time,                                            &
     &      phys_min(1:num_tot_nod_phys_vis),                           &
     &      phys_max(1:num_tot_nod_phys_vis)
!
         write(maximum_position_code,'(i10,1pE25.15e3,249i10)')         &
     &       ucd_step, time,                                            &
     &      node_min(1:num_tot_nod_phys_vis),                           &
     &      node_max(1:num_tot_nod_phys_vis)
!
       end if
!
      end subroutine output_range_data
!
!  ---------------------------------------------------------------------
!
      subroutine skip_range_data
!
      use m_parallel_var_dof
      use m_file_control_parameter
      use m_node_phys_data
      use m_t_step_parameter
!
      integer (kind = kint) :: iflag, i_read_step, i, itmp
      real(kind = kreal) :: rtmp
!
      iflag = i_step_init - mod(istep_max_dt, i_step_output_ucd)
      if ( my_rank .eq. 0 ) then
!
        do
          read(maximum_data_code,*,err=99,end=99)  i_read_step, rtmp,   &
     &      (rtmp,i=1,num_tot_nod_phys_vis),                            &
     &      (rtmp,i=1,num_tot_nod_phys_vis)
          if (i_read_step.ge.iflag) exit
        end do
  99    continue
!
        do
          read(maximum_position_code,*,err=98,end=98)                   &
     &      i_read_step, rtmp, (itmp,i=1,num_tot_nod_phys_vis),         &
     &      (itmp,i=1,num_tot_nod_phys_vis)
          if (i_read_step.ge.iflag) exit
        end do
  98    continue
!
       end if
!
      end subroutine skip_range_data
!
!  ---------------------------------------------------------------------
!
      end module range_data_IO
