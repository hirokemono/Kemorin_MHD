!m_control_param_refine_para.f90
!      module m_control_param_refine_para
!
      module m_control_param_refine_para
!
!      Written by Kemorin on May, 2010
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
!
      type(field_IO_params), save ::  para_fine_mesh_file
      type(field_IO_params), save ::  para_course_mesh_file
!
      character (len = kchara) :: def_para_fine_mesh_head = 'mesh_fine'
      character (len = kchara)                                          &
     &         :: def_para_course_mesh_head= 'mesh_course'
!
      character (len = kchara) :: c2f_para_head = 'course_2_fine_p'
      character (len = kchara) :: f2c_para_head = 'fine_2_course_p'
      character (len = kchara) :: f2c_ele_para_head = 'fine_2_course_e'
!
!
      character(len=kchara) :: course_2_fine_head = 'course_2_fine'
      character(len=kchara) :: fine_2_course_head = 'fine_2_course'
      character(len=kchara) :: refine_info_head =   'refine_info'
!
!      subroutine set_control_param_refine_para
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_param_refine_para
!
      use m_constants
      use m_machine_parameter
      use m_file_format_switch
      use m_para_refine_itp_tables
      use m_ctl_data_4_platforms
      use m_control_data_4_refine
      use m_control_data_refine_para
      use set_control_platform_data
!
!
      call turn_off_debug_flag_by_ctl(izero, plt1)
!
      if(plt1%ndomain_ctl%iflag .gt. 0) then
        nprocs_fine = plt1%ndomain_ctl%intvalue
      end if
!
      if(plt1%mesh_file_prefix%iflag .gt. 0) then
        para_fine_mesh_file%file_prefix                                 &
     &         = plt1%mesh_file_prefix%charavalue
      else 
        para_fine_mesh_file%file_prefix = def_para_fine_mesh_head
      end if
      para_fine_mesh_file%iflag_format = id_ascii_file_fmt
!
!
      if(i_course_to_fine_ctl .gt. 0) then
        course_2_fine_head = coarse_2_fine_head_ctl
      end if
!
      if(i_fine_to_course_ctl .gt. 0) then
        fine_2_course_head = fine_2_course_head_ctl
      end if
!
      if(i_refine_info_ctl .gt. 0) then
        refine_info_head = refine_info_head_ctl
      end if
!
!
      if(i_num_course_subdomain .gt. 0) then
        nprocs_course = nprocs_course_ctl
      end if
!
      if(i_course_mesh_file_head .gt. 0) then
        para_course_mesh_file%file_prefix = course_mesh_file_head_ctl
      else
        para_course_mesh_file%file_prefix = def_para_course_mesh_head
      end if
!
      if(i_course_to_fine_p_head .gt. 0) then
        c2f_para_head = c2f_para_head_ctl
      end if
!
      if(i_fine_to_course_p_head .gt. 0) then
        f2c_para_head = f2c_para_head_ctl
      end if
!
      if(i_refine_info_ctl .gt. 0) then
        f2c_ele_para_head = refine_info_para_head_ctl
      end if
!
      nprocs_larger = max(nprocs_fine, nprocs_course)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'nprocs_fine, nprocs_course, nprocs_larger'
        write(*,*)  nprocs_fine, nprocs_course, nprocs_larger
        write(*,*) 'para_fine_mesh_head:   ',                           &
     &            trim(para_fine_mesh_file%file_prefix)
        write(*,*) 'para_course_mesh_head: ',                           &
     &           trim(para_course_mesh_file%file_prefix)
        write(*,*) 'c2f_para_head:         ', trim(c2f_para_head)
        write(*,*) 'f2c_para_head:         ', trim(f2c_para_head)
        write(*,*) 'f2c_ele_para_head:     ', trim(f2c_ele_para_head)
        write(*,*) 'course_2_fine_head:    ', trim(course_2_fine_head)
        write(*,*) 'fine_2_course_head:    ', trim(fine_2_course_head)
        write(*,*) 'refine_info_head:      ', trim(refine_info_head)
      end if
!
      end subroutine set_control_param_refine_para
!
! -----------------------------------------------------------------------
!
      end module m_control_param_refine_para
