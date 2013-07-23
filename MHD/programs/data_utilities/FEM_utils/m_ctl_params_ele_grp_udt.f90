!m_ctl_params_ele_grp_udt.f90
!      module m_ctl_params_ele_grp_udt
!
!      Written by H. Matsui on March, 2012
!
!      subroutine set_control_ele_grp_udt
!      subroutine find_start_element_group(num_mat, mat_name)
!
      module m_ctl_params_ele_grp_udt
!
      use m_precision
!
      implicit none
!
      character(len = kchara) :: layerd_mesh_head
      character(len = kchara) :: group_data_file_name
      character(len = kchara) :: grp_ucd_data_head
!
      character(len = kchara) :: tave_grp_udt_head
      character(len = kchara) :: tsig_grp_udt_head
!
      character(len=kchara) :: start_ele_grp_name
      integer(kind = kint) :: istart_ele_grp_drmd, iend_ele_grp_drmd
      integer(kind = kint) :: num_ele_grp_drmd
!
      integer(kind = kint) :: iflag_data_mode, iflag_time_averaged
      integer(kind = kint) :: iflag_sqrt_rms
!
      integer(kind = kint) :: istep_start, istep_end, istep_inc
      real(kind = kreal) :: start_time, delta_t
!
      character(len=kchara), parameter, private                         &
     &       :: sgs_cor_file_name =      'sgs_correlate.dat'
      character(len=kchara), parameter, private                         &
     &       :: sgs_ratio_file_name =    'sgs_rms_ratio.dat'
      character(len=kchara), parameter, private                         &
     &       :: sgs_rms_file_name =      'sgs_rms.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_cor_file_name =     'diff_correlate.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_ratio_file_name =   'diff_rms_ratio.dat'
      character(len=kchara), parameter, private                         &
     &       :: diff_rms_file_name =     'diff_rms.dat'
!
      private :: set_grp_data_mode_by_data_name
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_control_ele_grp_udt
!
      use m_ctl_data_4_time_steps
      use m_ctl_data_ele_grp_udt
      use set_parallel_file_name
!
      character(len = kchara) :: fhead_tmp
!
!
      if(i_group_mesh_head .gt. 0) then
        layerd_mesh_head = group_mesh_head_ctl
      end if
!
      if(i_group_data_name .gt. 0) then
        group_data_file_name = grp_evo_data_ctl
      else
        write(*,*) 'set grouping data file name'
        stop
      end if
!
      if(i_group_udt_head .gt. 0) then
        grp_ucd_data_head = grp_ucd_data_head_ctl
      else
        grp_ucd_data_head = group_data_file_name
      end if
!
      if(i_ngrp_ele_grp .gt. 0) then
        num_ele_grp_drmd = ngrp_ele_grp_ctl
      else
        num_ele_grp_drmd = -2
      end if
!
      iflag_time_averaged = 0
      if(i_time_average_data .gt. 0) then
        if(time_average_data_ctl .eq.     'yes'                         &
     &    .or. time_average_data_ctl .eq. 'Yes'                         &
     &    .or. time_average_data_ctl .eq. 'YES') then
          iflag_time_averaged = 1
        end if
      end if
!
      call set_grp_data_mode_by_data_name
!
      if(i_start_ele_grp_name .gt. 0) then
        start_ele_grp_name = start_ele_grp_name_ctl
      else
        write(start_ele_grp_name,'(a)') 'all'
      end if
      if(i_ngrp_ele_grp .gt. 0) then
        num_ele_grp_drmd = ngrp_ele_grp_ctl
      end if
!
!
!
      if(i_time_init .gt. 0) then
        start_time = time_init_ctl
      else
        start_time = 0.0d0
      end if
!
      if(i_dt .gt. 0) then
        delta_t = dt_ctl
      else
        delta_t = 0.0d0
      end if
!
      if(i_i_step_init .gt. 0) then
        istep_start = i_step_init_ctl
      else
        istep_start = 1
      end if
!
      if(i_i_step_number .gt. 0) then
        istep_end = i_step_number_ctl
      else
        istep_end = 1
      end if
!
      if(i_i_step_psf .gt. 0) then
        istep_inc = i_step_psf_ctl
      else
        istep_inc = 1
      end if
!
      call add_int_suffix(istep_start, grp_ucd_data_head, fhead_tmp)
      write(tave_grp_udt_head,'(6a,a)') 't_ave_', trim(fhead_tmp)
      write(tsig_grp_udt_head,'(6a,a)') 'sigma_', trim(fhead_tmp)
!
      write(*,*) 'grp_ucd_data_head: ', trim(grp_ucd_data_head)
      write(*,*) 'tsig_grp_udt_head: ', trim(tsig_grp_udt_head)
      write(*,*) 'tave_grp_udt_head: ', trim(tave_grp_udt_head)
!
      write(*,*) 'start_ele_grp_name: ', trim(start_ele_grp_name)
      write(*,*) 'num_ele_grp_drmd: ', num_ele_grp_drmd
!
      write(*,*) 'start step:   ', istep_start
      write(*,*) 'end step:     ', istep_end
      write(*,*) 'incrination:  ', istep_inc
      write(*,*) 'initial time: ', delta_t
      write(*,*) 'Delta t:      ', start_time

      end subroutine set_control_ele_grp_udt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_grp_data_mode_by_data_name
!
!
      iflag_sqrt_rms =      0
      if     (group_data_file_name .eq. sgs_rms_file_name               &
     &   .or. group_data_file_name .eq. sgs_ratio_file_name             &
     &   .or. group_data_file_name .eq. diff_rms_file_name              &
     &   .or. group_data_file_name .eq. diff_ratio_file_name) then
        iflag_sqrt_rms =      1
      end if
!
      end subroutine set_grp_data_mode_by_data_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine find_start_element_group(num_mat, mat_name)
!
      integer(kind = kint), intent(in) :: num_mat
      character(len = kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint) :: igrp
!
!
      if(      start_ele_grp_name .eq. 'all'                            &
     &    .or. start_ele_grp_name .eq. 'All'                            &
     &    .or. start_ele_grp_name .eq. 'ALL') then
        istart_ele_grp_drmd = 1
        iend_ele_grp_drmd = num_mat
        num_ele_grp_drmd = num_mat
      else
        do igrp = 1, num_mat
          if(trim(mat_name(igrp)) .eq. trim(start_ele_grp_name)) then
            istart_ele_grp_drmd = igrp
            iend_ele_grp_drmd = min(num_mat,igrp+num_ele_grp_drmd-1)
            num_ele_grp_drmd                                            &
     &          = iend_ele_grp_drmd - istart_ele_grp_drmd  + 1
            exit
          end if
        end do
      end if
      write(*,*) 'start group id: ', istart_ele_grp_drmd
      write(*,*) 'end group id:   ', iend_ele_grp_drmd
      write(*,*) 'num. of group:  ', num_ele_grp_drmd
!
      end subroutine find_start_element_group
!
!-----------------------------------------------------------------------
!
      end module m_ctl_params_ele_grp_udt
