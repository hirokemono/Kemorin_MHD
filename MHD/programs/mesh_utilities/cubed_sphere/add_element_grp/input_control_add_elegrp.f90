!input_control_add_elegrp.f90
!      module input_control_add_elegrp
!
!      Written by H. Matsui on Mar., 2008
!
!!      subroutine s_input_control_add_elegrp(add_egrp_param)
!!        type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
      module input_control_add_elegrp
!
      use m_precision
!
      use calypso_mpi
      use t_add_ele_grp_parameter
      use t_file_IO_parameter
      use t_control_data_add_ele_grp
!
      implicit none
!
      character (len = kchara), parameter, private                      &
     &         :: control_file_name = 'ctl_add_ele_grp'
!
      private :: bcast_control_add_elegrp, bcast_ctl_data_4_add_2d_egrp
      private :: set_control_add_2d_egrp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_input_control_add_elegrp(add_egrp_param)
!
      type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
      type(control_data_add_ele_grp) :: addgrp_c
!
!
      if(my_rank .eq. 0) then
        call read_control_add_elegrp(control_file_name, addgrp_c)
      end if
      call bcast_control_add_elegrp(addgrp_c)
!
      if(addgrp_c%i_add_ele_grp_ctl .ne. 1) then
        call calypso_MPI_abort(addgrp_c%i_add_ele_grp_ctl,              &
     &                             'control file is broken')
      end if
!
      call set_control_add_2d_egrp(addgrp_c, add_egrp_param)
      call dealloc_control_add_elegrp(addgrp_c)
!
      end subroutine s_input_control_add_elegrp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_control_add_elegrp(addgrp_c)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      call bcast_ctl_data_4_add_2d_egrp(addgrp_c)
!
      call bcast_ctl_data_4_platform(addgrp_c%source_plt)
      call bcast_ctl_data_4_platform(addgrp_c%added_plt)
!
      call calypso_mpi_bcast_one_int(addgrp_c%i_add_ele_grp_ctl, 0)
!
      end subroutine bcast_control_add_elegrp
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_4_add_2d_egrp(addgrp_c)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(control_data_add_ele_grp), intent(inout) :: addgrp_c
!
!
      call bcast_ctl_array_cr2(addgrp_c%r_ele_grouping_ctl)
      call bcast_ctl_array_cr2(addgrp_c%s_ele_grouping_ctl)
      call bcast_ctl_array_cr2(addgrp_c%t_ele_grouping_ctl)
      call bcast_ctl_array_cr2(addgrp_c%z_ele_grouping_ctl)
!
      call bcast_ctl_type_c1(addgrp_c%sph_grp_direction_ctl)
!
      call calypso_mpi_bcast_one_int(addgrp_c%i_add_ele_grp_para, 0)
!
      end subroutine bcast_ctl_data_4_add_2d_egrp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_add_2d_egrp(addgrp_c, add_egrp_param)
!
      use calypso_mpi
      use m_error_IDs
      use m_constants
      use m_default_file_prefix
      use t_control_data_add_ele_grp
      use set_ctl_parallel_platform
      use set_control_platform_item
      use set_control_platform_data
      use skip_comment_f
!
      type(control_data_add_ele_grp), intent(in) :: addgrp_c
      type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
      real(kind = kreal) :: pi
      character(len=kchara) :: tmpchara
      integer(kind = kint) :: num_grp
!
!
      call check_control_num_domains(addgrp_c%source_plt)
      call turn_off_debug_flag_by_ctl(my_rank, addgrp_c%source_plt)
      call set_control_parallel_mesh(addgrp_c%source_plt,               &
     &                               add_egrp_param%original_mesh_file)
      call set_ctl_parallel_file_w_def(def_new_mesh_head,               &
     &    addgrp_c%added_plt%mesh_file_prefix,                          &
     &    addgrp_c%added_plt%mesh_file_fmt_ctl,                         &
     &                               add_egrp_param%modified_mesh_file)
!
      if (addgrp_c%sph_grp_direction_ctl%iflag .gt. 0) then
        tmpchara = addgrp_c%sph_grp_direction_ctl%charavalue
        if     (cmp_no_case(tmpchara, 'sphere')                         &
     &     .or. cmp_no_case(tmpchara, 'r_theta')                        &
     &     .or. cmp_no_case(tmpchara, 'theta')                          &
     &     .or. cmp_no_case(tmpchara, 'elevation'))then
          add_egrp_param%iflag_grping_direction = 0
        else if(cmp_no_case(tmpchara, 'cylindrical_r_z')                &
     &     .or. cmp_no_case(tmpchara, 's_z')                            &
     &     .or. cmp_no_case(tmpchara, 'cyrindrical')                    &
     &     .or. cmp_no_case(tmpchara, 's')) then
          add_egrp_param%iflag_grping_direction = 2
        else if(cmp_no_case(tmpchara, 'z_theta')) then
          add_egrp_param%iflag_grping_direction = 3
        else if(cmp_no_case(tmpchara, 'r_cylindrical_r')                &
     &     .or. cmp_no_case(tmpchara, 'r_s')) then
          add_egrp_param%iflag_grping_direction = 1
        else
          call calypso_MPI_abort                                        &
     &       (ierr_mesh, 'set correct grouping mode')
        end if
      else
        call calypso_MPI_abort(ierr_mesh, 'set correct grouping mode')
      end if
!
      if (addgrp_c%r_ele_grouping_ctl%icou .gt. 0) then
        num_grp = addgrp_c%r_ele_grouping_ctl%num
        call alloc_add_r_ele_grping(num_grp, add_egrp_param)
!
        add_egrp_param%r_ele_grp_name(1:num_grp)                        &
     &        = addgrp_c%r_ele_grouping_ctl%c_tbl(1:num_grp)
        add_egrp_param%minmax_r_ele_grping(1:num_grp,1)                 &
     &        = addgrp_c%r_ele_grouping_ctl%vec1(1:num_grp)
        add_egrp_param%minmax_r_ele_grping(1:num_grp,2)                 &
     &        = addgrp_c%r_ele_grouping_ctl%vec2(1:num_grp)
      end if
!
      if (addgrp_c%s_ele_grouping_ctl%icou .gt. 0) then
        num_grp = addgrp_c%s_ele_grouping_ctl%num
        call alloc_add_s_ele_grping(num_grp, add_egrp_param)
!
        add_egrp_param%s_ele_grp_name(1:num_grp)                        &
     &        = addgrp_c%s_ele_grouping_ctl%c_tbl(1:num_grp)
        add_egrp_param%minmax_s_ele_grping(1:num_grp,1)                 &
     &        = addgrp_c%s_ele_grouping_ctl%vec1(1:num_grp)
        add_egrp_param%minmax_s_ele_grping(1:num_grp,2)                 &
     &        = addgrp_c%s_ele_grouping_ctl%vec2(1:num_grp)
      end if
!
      if (addgrp_c%t_ele_grouping_ctl%icou .gt. 0) then
        num_grp = addgrp_c%t_ele_grouping_ctl%num
        call alloc_add_t_ele_grping(num_grp, add_egrp_param)
!
        add_egrp_param%t_ele_grp_name(1:num_grp)                        &
     &        = addgrp_c%t_ele_grouping_ctl%c_tbl(1:num_grp)
        add_egrp_param%minmax_t_ele_grping(1:num_grp,1)                 &
     &        = addgrp_c%t_ele_grouping_ctl%vec1(1:num_grp)
        add_egrp_param%minmax_t_ele_grping(1:num_grp,2)                 &
     &        = addgrp_c%t_ele_grouping_ctl%vec2(1:num_grp)
      end if
!
      if (addgrp_c%z_ele_grouping_ctl%icou .gt. 0) then
        num_grp = addgrp_c%z_ele_grouping_ctl%num
        call alloc_add_z_ele_grping(num_grp, add_egrp_param)
!
        add_egrp_param%z_ele_grp_name(1:num_grp)                        &
     &        = addgrp_c%z_ele_grouping_ctl%c_tbl(1:num_grp)
        add_egrp_param%minmax_z_ele_grping(1:num_grp,1)                 &
     &        = addgrp_c%z_ele_grouping_ctl%vec1(1:num_grp)
        add_egrp_param%minmax_z_ele_grping(1:num_grp,2)                 &
     &        = addgrp_c%z_ele_grouping_ctl%vec2(1:num_grp)
      end if
!
!
      if (add_egrp_param%iflag_grping_direction .eq. 0                  &
     &    .or. add_egrp_param%iflag_grping_direction .eq. 3) then
!
        if (add_egrp_param%num_t_ele_grp .gt. 0) then
          pi = four * atan(one)
          num_grp = add_egrp_param%num_t_ele_grp
          add_egrp_param%minmax_t_ele_grping(1:num_grp,1)               &
     &         = add_egrp_param%minmax_t_ele_grping(1:num_grp,1) * pi
          add_egrp_param%minmax_t_ele_grping(1:num_grp,2)               &
     &         = add_egrp_param%minmax_t_ele_grping(1:num_grp,2) * pi
        end if
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'iflag_grping_direction',                            &
     &            add_egrp_param%iflag_grping_direction
        write(*,*) 'num_r_ele_grp', add_egrp_param%num_r_ele_grp
        write(*,*) 'num_t_ele_grp', add_egrp_param%num_t_ele_grp
        write(*,*) 'num_s_ele_grp', add_egrp_param%num_s_ele_grp
        write(*,*) 'num_z_ele_grp', add_egrp_param%num_z_ele_grp
      end if
!
      end subroutine set_control_add_2d_egrp
!
! -----------------------------------------------------------------------
!
      end module input_control_add_elegrp

