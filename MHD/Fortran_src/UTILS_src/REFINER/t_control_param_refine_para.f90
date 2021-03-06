!t_control_param_refine_para.f90
!      module t_control_param_refine_para
!
!      Written by Kemorin on May, 2010
!
!!      subroutine set_control_param_refine_para                        &
!!     &         (refine_ctl, p_refine_ctl, p_refine_p, para_ref_itp)
!!        type(control_data_4_refine), intent(in) :: refine_ctl
!!        type(file_ctls_refine_para), intent(in) :: p_refine_ctl
!!        type(ctl_param_para_refiner), intent(inout) :: p_refine_p
!!        type(para_refine_itp_tables), intent(inout) :: para_ref_itp
!
      module t_control_param_refine_para
!
      use m_precision
      use t_file_IO_parameter
      use m_file_format_switch
!
      implicit none
!
!
      type ctl_param_para_refiner
        type(field_IO_params) ::  para_fine_mesh_file
        type(field_IO_params) ::  para_course_mesh_file
!
        type(field_IO_params) :: c2f_para_tbl_IO
        type(field_IO_params) :: f2c_para_tbl_IO
        type(field_IO_params) :: f2c_ele_para_tbl_IO
!
        type(field_IO_params) ::  c2f_table_IO
!
        type(field_IO_params) :: c2f_tbl_IO
        type(field_IO_params) :: f2c_tbl_IO

        type(field_IO_params) :: refine_tbl_IO
      end type ctl_param_para_refiner
!
      character(len=kchara), parameter, private                         &
     &              :: c2f_para_head = 'course_2_fine_p'
      character(len=kchara), parameter, private                         &
     &              :: f2c_para_head = 'fine_2_course_p'
      character(len=kchara), parameter, private                         &
     &              :: f2c_ele_para_head = 'fine_2_course_e'
!
      character(len=kchara), parameter, private                         &
     &              :: course_2_fine_head = 'course_2_fine'
      character(len=kchara), parameter, private                         &
     &              :: fine_2_course_head = 'fine_2_course'
      character(len=kchara), parameter, private                         &
     &              :: refine_info_head =   'refine_info'
!
      character(len = kchara), parameter                                &
     &            :: def_para_fine_mesh_head = 'mesh_fine'
      character(len = kchara) , parameter                               &
     &           :: def_para_course_mesh_head= 'mesh_course'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_param_refine_para                          &
     &         (refine_ctl, p_refine_ctl, p_refine_p, para_ref_itp)
!
      use m_constants
      use m_machine_parameter
      use t_para_refine_itp_tables
      use t_control_data_refine_para
      use t_control_data_4_refine
      use set_control_platform_item
!
      type(control_data_4_refine), intent(in) :: refine_ctl
      type(file_ctls_refine_para), intent(in) :: p_refine_ctl
!
      type(ctl_param_para_refiner), intent(inout) :: p_refine_p
      type(para_refine_itp_tables), intent(inout) :: para_ref_itp
!
!
      call turn_off_debug_flag_by_ctl(0, refine_ctl%source_plt)
!
      if(refine_ctl%source_plt%ndomain_ctl%iflag .gt. 0) then
        para_ref_itp%nprocs_fine                                        &
     &          = refine_ctl%source_plt%ndomain_ctl%intvalue
      end if
!
      if(refine_ctl%source_plt%mesh_file_prefix%iflag .gt. 0) then
        p_refine_p%para_fine_mesh_file%file_prefix                      &
     &         = refine_ctl%source_plt%mesh_file_prefix%charavalue
      else 
        p_refine_p%para_fine_mesh_file%file_prefix                      &
     &         = def_para_fine_mesh_head
      end if
      p_refine_p%para_fine_mesh_file%iflag_format                       &
     &          = id_ascii_file_fmt
!
!
      if(refine_ctl%coarse_2_fine_head_ctl%iflag .gt. 0) then
        p_refine_p%c2f_table_IO%file_prefix                             &
     &     = refine_ctl%coarse_2_fine_head_ctl%charavalue
      else
        p_refine_p%c2f_table_IO%file_prefix = course_2_fine_head
      end if
      p_refine_p%c2f_table_IO%iflag_format  = id_ascii_file_fmt         
!
      if(refine_ctl%fine_2_course_head_ctl%iflag .gt. 0) then
        p_refine_p%f2c_tbl_IO%file_prefix                               &
     &     = refine_ctl%fine_2_course_head_ctl%charavalue
      else
        p_refine_p%f2c_tbl_IO%file_prefix = fine_2_course_head
      end if
      p_refine_p%f2c_tbl_IO%iflag_format  = id_ascii_file_fmt         
!
      if(refine_ctl%refine_info_head_ctl%iflag .gt. 0) then
        p_refine_p%refine_tbl_IO%file_prefix                            &
     &      = refine_ctl%refine_info_head_ctl%charavalue
      else
        p_refine_p%refine_tbl_IO%file_prefix = refine_info_head
      end if
      p_refine_p%refine_tbl_IO%iflag_format = id_ascii_file_fmt         
!
!
      para_ref_itp%nprocs_course = 1
      if(p_refine_ctl%nprocs_course_ctl%iflag .gt. 0) then
        para_ref_itp%nprocs_course                                      &
     &         = p_refine_ctl%nprocs_course_ctl%intvalue
      end if
!
      p_refine_p%para_course_mesh_file%file_prefix                      &
     &         = def_para_course_mesh_head
      if(p_refine_ctl%course_mesh_file_head_ctl%iflag .gt. 0) then
        p_refine_p%para_course_mesh_file%file_prefix                    &
     &          = p_refine_ctl%course_mesh_file_head_ctl%charavalue
      end if
!
      if(p_refine_ctl%c2f_para_head_ctl%iflag .gt. 0) then
        p_refine_p%c2f_para_tbl_IO%file_prefix                          &
     &      = p_refine_ctl%c2f_para_head_ctl%charavalue
      else
        p_refine_p%c2f_para_tbl_IO%file_prefix = c2f_para_head
      end if
      p_refine_p%c2f_para_tbl_IO%iflag_format  = id_ascii_file_fmt         
!
      if(p_refine_ctl%f2c_para_head_ctl%iflag .gt. 0) then
        p_refine_p%f2c_para_tbl_IO%file_prefix                          &
     &      = p_refine_ctl%f2c_para_head_ctl%charavalue
      else
        p_refine_p%f2c_para_tbl_IO%file_prefix = f2c_para_head
      end if
      p_refine_p%f2c_para_tbl_IO%iflag_format  = id_ascii_file_fmt         
!
      if(p_refine_ctl%refine_info_para_head_ctl%iflag .gt. 0) then
        p_refine_p%f2c_ele_para_tbl_IO%file_prefix                      &
     &     = p_refine_ctl%refine_info_para_head_ctl%charavalue
      else
        p_refine_p%f2c_ele_para_tbl_IO%file_prefix = f2c_ele_para_head
      end if
      p_refine_p%f2c_ele_para_tbl_IO%iflag_format = id_ascii_file_fmt         
!
      para_ref_itp%nprocs_larger                                        &
     &      = max(para_ref_itp%nprocs_fine, para_ref_itp%nprocs_course)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'nprocs_fine, nprocs_course, nprocs_larger'
        write(*,*)  para_ref_itp%nprocs_fine,                           &
     &        para_ref_itp%nprocs_course, para_ref_itp%nprocs_larger
        write(*,*) 'para_fine_mesh_head:   ',                           &
     &            trim(p_refine_p%para_fine_mesh_file%file_prefix)
        write(*,*) 'para_course_mesh_head: ',                           &
     &           trim(p_refine_p%para_course_mesh_file%file_prefix)
        write(*,*) 'c2f_para_head:         ',                           &
     &       trim(p_refine_p%c2f_para_tbl_IO%file_prefix)
        write(*,*) 'f2c_para_head:         ',                           &
     &       trim(p_refine_p%f2c_para_tbl_IO%file_prefix)
        write(*,*) 'f2c_ele_para_head:     ',                           &
     &       trim(p_refine_p%f2c_ele_para_tbl_IO%file_prefix)
!        write(*,*) 'course_2_fine_head:    ',                          &
!     &       trim(p_refine_p%f2c_tbl_IO%file_prefix)
        write(*,*) 'fine_2_course_head:    ',                           &
     &       trim(p_refine_p%f2c_tbl_IO%file_prefix)
        write(*,*) 'refine_info_head:      ',                           &
     &       trim(p_refine_p%refine_tbl_IO%file_prefix)
      end if
!
      end subroutine set_control_param_refine_para
!
! -----------------------------------------------------------------------
!
      end module t_control_param_refine_para
