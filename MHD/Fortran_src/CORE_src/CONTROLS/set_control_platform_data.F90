!>@file   set_control_platform_data.f90
!!@brief  module set_control_platform_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  Set file headers and number of processor and threds
!!        from control data
!!
!!@verbatim
!!      subroutine turn_off_debug_flag_by_ctl(my_rank)
!!      subroutine set_control_smp_def(my_rank)
!!      subroutine set_control_mesh_def
!!      subroutine set_control_sph_mesh
!!      subroutine set_control_restart_file_def
!!@endverbatim
!!
!!@param my_rank  preocess ID
!
      module set_control_platform_data
!
      use m_precision
!
      use m_constants
      use m_ctl_data_4_platforms
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine turn_off_debug_flag_by_ctl(my_rank)
!
      use m_machine_parameter
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(i_debug_flag_ctl .gt. 0) then
        if     (cmp_no_case(debug_flag_ctl,'Off') .gt. 0                &
     &     .or. cmp_no_case(debug_flag_ctl,'0') .gt. 0) then
          i_debug =     iflag_minimum_msg
        else if(cmp_no_case(debug_flag_ctl,'On') .gt. 0                 &
     &     .or. cmp_no_case(debug_flag_ctl,'1') .gt. 0) then
          i_debug =     iflag_routine_msg
        else if(cmp_no_case(debug_flag_ctl,'Full') .gt. 0               &
     &     .or. cmp_no_case(debug_flag_ctl,'2') .gt. 0) then
          i_debug =     iflag_full_msg
        end if
      end if
!
      if(my_rank .eq. 0) iflag_debug = i_debug
!
      end subroutine turn_off_debug_flag_by_ctl
!
! ----------------------------------------------------------------------
!
      subroutine set_control_smp_def(my_rank)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: my_rank
      integer, external :: omp_get_max_threads
      integer :: np_smp4
!
!
      if (i_num_smp .gt. 0) then
        np_smp = num_smp_ctl
      else
        np_smp = ione
      end if
!
#ifdef _OPENMP
      if (int(np_smp) .lt. omp_get_max_threads()) then
        if(my_rank .eq. 0) write(*,*)                                   &
     &               'Number of SMP threads is chenged to', np_smp
        np_smp4 = int(np_smp)
        call omp_set_num_threads(np_smp4)
      end if
#endif
!
      end subroutine set_control_smp_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_mesh_def
!
      use m_read_mesh_data
      use m_file_format_switch
      use skip_comment_f
!
!
      if(i_mesh_extension .gt. 0) then
        if     (cmp_no_case(mesh_extension_flags_ctl,'Off') .gt. 0      &
     &     .or. cmp_no_case(mesh_extension_flags_ctl,'0') .gt. 0) then
          iflag_mesh_file_ext = 0
        end if
      end if
!
      if (i_mesh_header .gt. 0) then
        mesh_file_head = mesh_file_prefix
      else
        mesh_file_head = def_mesh_file_head
      end if
!
!   set data format
!
      call choose_file_format(mesh_file_fmt_ctl, i_mesh_file_fmt,       &
     &          iflag_mesh_file_fmt)
!
      end subroutine set_control_mesh_def
!
! -----------------------------------------------------------------------
!
      subroutine set_control_sph_mesh
!
      use m_read_mesh_data
      use m_control_params_sph_data
      use m_node_id_spherical_IO
      use m_field_data_IO
      use m_file_format_switch
!
!   set data format
!
      call choose_file_format(sph_file_fmt_ctl, i_sph_files_fmt,        &
     &    iflag_sph_file_fmt)
      call choose_file_format(spectr_file_fmt_ctl, i_spect_files_fmt,   &
     &    iflag_sph_spectr_fmt)
!
!   set file header at once
!
      if(i_sph_files_header .gt. 0) then
        sph_file_head =  sph_file_prefix
        mesh_file_head = sph_file_prefix
        iflag_mesh_file_ext = 1
        iflag_mesh_file_fmt = iflag_sph_file_fmt
      end if
!
      iflag_sph_spec_output = i_spectr_header
      if(iflag_sph_spec_output .gt. 0) then
        spectr_file_head = spectr_file_head_ctl
      end if
!
      end subroutine set_control_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine set_control_restart_file_def
!
      use m_field_data_IO
      use m_file_format_switch
!
!
      if (i_rst_header .gt. 0) then
        phys_file_head = restart_file_prefix
      end if
!
      call choose_file_format(restart_file_fmt_ctl, i_rst_files_fmt,    &
     &    iflag_field_data_fmt)
!
      end subroutine set_control_restart_file_def
!
! -----------------------------------------------------------------------
!
      end module set_control_platform_data
