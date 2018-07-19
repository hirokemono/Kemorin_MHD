!set_control_add_2d_egrp.f90
!      module set_control_add_2d_egrp
!
!      Written by Kemorin on Mar., 2008
!
!      subroutine s_set_control_add_2d_egrp
!
      module set_control_add_2d_egrp
!
      use m_precision
      use m_error_IDs
      use t_file_IO_parameter
!
      implicit    none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_add_2d_egrp(addgrp_c)
!
      use calypso_mpi
      use m_constants
      use m_default_file_prefix
      use m_add_ele_grp_parameter
      use t_control_data_add_ele_grp
      use set_ctl_parallel_platform
      use set_control_platform_data
      use skip_comment_f
!
      type(control_data_add_ele_grp), intent(in) :: addgrp_c
!
      real(kind = kreal) :: pi
      character(len=kchara) :: tmpchara
!
!
      call check_control_num_domains(addgrp_c%source_plt)
      call turn_off_debug_flag_by_ctl(my_rank, addgrp_c%source_plt)
      call set_control_mesh_def                                         &
     &   (addgrp_c%source_plt, original_mesh_file)
      call set_control_mesh_file_def                                    &
     &   (def_new_mesh_head, addgrp_c%added_plt, modified_mesh_file)
!
      if (addgrp_c%sph_grp_direction_ctl%iflag .gt. 0) then
        tmpchara = addgrp_c%sph_grp_direction_ctl%charavalue
        if     (cmp_no_case(tmpchara, 'sphere')                         &
     &     .or. cmp_no_case(tmpchara, 'r_theta')                        &
     &     .or. cmp_no_case(tmpchara, 'theta')                          &
     &     .or. cmp_no_case(tmpchara, 'elevation'))then
          iflag_grping_direction = 0
        else if(cmp_no_case(tmpchara, 'cylindrical_r_z')                &
     &     .or. cmp_no_case(tmpchara, 's_z')                            &
     &     .or. cmp_no_case(tmpchara, 'cyrindrical')                    &
     &     .or. cmp_no_case(tmpchara, 's')) then
          iflag_grping_direction = 2
        else if(cmp_no_case(tmpchara, 'z_theta')) then
          iflag_grping_direction = 3
        else if(cmp_no_case(tmpchara, 'r_cylindrical_r')                &
     &     .or. cmp_no_case(tmpchara, 'r_s')) then
          iflag_grping_direction = 1
        else
          call calypso_MPI_abort                                        &
     &       (ierr_mesh, 'set correct grouping mode')
        end if
      else
        call calypso_MPI_abort(ierr_mesh, 'set correct grouping mode')
      end if
!
      if (addgrp_c%r_ele_grouping_ctl%icou .gt. 0) then
        num_r_ele_grp = addgrp_c%r_ele_grouping_ctl%num
        call allocate_add_r_ele_grping
!
        r_ele_grp_name(1:num_r_ele_grp)                                 &
     &        = addgrp_c%r_ele_grouping_ctl%c_tbl(1:num_r_ele_grp)
        minmax_r_ele_grping(1:num_r_ele_grp,1)                          &
     &        = addgrp_c%r_ele_grouping_ctl%vec1(1:num_r_ele_grp)
        minmax_r_ele_grping(1:num_r_ele_grp,2)                          &
     &        = addgrp_c%r_ele_grouping_ctl%vec2(1:num_r_ele_grp)
      end if
!
      if (addgrp_c%s_ele_grouping_ctl%icou .gt. 0) then
        num_s_ele_grp = addgrp_c%s_ele_grouping_ctl%num
        call allocate_add_s_ele_grping
!
        s_ele_grp_name(1:num_s_ele_grp)                                 &
     &        = addgrp_c%s_ele_grouping_ctl%c_tbl(1:num_s_ele_grp)
        minmax_s_ele_grping(1:num_s_ele_grp,1)                          &
     &        = addgrp_c%s_ele_grouping_ctl%vec1(1:num_s_ele_grp)
        minmax_s_ele_grping(1:num_s_ele_grp,2)                          &
     &        = addgrp_c%s_ele_grouping_ctl%vec2(1:num_s_ele_grp)
      end if
!
      if (addgrp_c%t_ele_grouping_ctl%icou .gt. 0) then
        num_t_ele_grp = addgrp_c%t_ele_grouping_ctl%num
        call allocate_add_t_ele_grping
!
        t_ele_grp_name(1:num_t_ele_grp)                                 &
     &        = addgrp_c%t_ele_grouping_ctl%c_tbl(1:num_t_ele_grp)
        minmax_t_ele_grping(1:num_t_ele_grp,1)                          &
     &        = addgrp_c%t_ele_grouping_ctl%vec1(1:num_t_ele_grp)
        minmax_t_ele_grping(1:num_t_ele_grp,2)                          &
     &        = addgrp_c%t_ele_grouping_ctl%vec2(1:num_t_ele_grp)
      end if
!
      if (addgrp_c%z_ele_grouping_ctl%icou .gt. 0) then
        num_z_ele_grp = addgrp_c%z_ele_grouping_ctl%num
        call allocate_add_z_ele_grping
!
        z_ele_grp_name(1:num_z_ele_grp)                                 &
     &        = addgrp_c%z_ele_grouping_ctl%c_tbl(1:num_z_ele_grp)
        minmax_z_ele_grping(1:num_z_ele_grp,1)                          &
     &        = addgrp_c%z_ele_grouping_ctl%vec1(1:num_z_ele_grp)
        minmax_z_ele_grping(1:num_z_ele_grp,2)                          &
     &        = addgrp_c%z_ele_grouping_ctl%vec2(1:num_z_ele_grp)
      end if
!
!
      if (iflag_grping_direction .eq. 0                                 &
     &    .or. iflag_grping_direction .eq. 3) then
!
        if (num_t_ele_grp .gt. 0) then
          pi = four * atan(one)
          minmax_t_ele_grping(1:num_t_ele_grp,1)                        &
     &        = minmax_t_ele_grping(1:num_t_ele_grp,1) * pi
          minmax_t_ele_grping(1:num_t_ele_grp,2)                        &
     &        = minmax_t_ele_grping(1:num_t_ele_grp,2) * pi
        end if
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'iflag_grping_direction', iflag_grping_direction
        write(*,*) 'num_r_ele_grp', num_r_ele_grp
        write(*,*) 'num_t_ele_grp', num_t_ele_grp
        write(*,*) 'num_s_ele_grp', num_s_ele_grp
        write(*,*) 'num_z_ele_grp', num_z_ele_grp
      end if
!
      end subroutine s_set_control_add_2d_egrp
!
! -----------------------------------------------------------------------
!
      end module set_control_add_2d_egrp
