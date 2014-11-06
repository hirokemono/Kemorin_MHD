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
!
      implicit    none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_add_2d_egrp
!
      use calypso_mpi
      use m_constants
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use m_control_data_add_ele_grp
      use m_read_mesh_data
      use m_add_ele_grp_parameter
      use set_ctl_parallel_platform
      use set_control_platform_data
!
      real(kind = kreal) :: pi
!
!
      call check_control_num_domains
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_mesh_def
      original_mesh_head = mesh_file_head
!
      if (i_new_mesh_head .gt. 0) then
        modified_mesh_head = new_mesh_prefix
      else
        call calypso_MPI_abort(ierr_mesh, 'set modifield mesh prefix')
      end if
!
      if (i_2nd_grp_direction .gt. 0) then
        if    (   sph_grp_direction_ctl .eq. 'sphere'                   &
     &       .or. sph_grp_direction_ctl .eq. 'Sphere'                   &
     &       .or. sph_grp_direction_ctl .eq. 'SPHERE'                   &
     &       .or. sph_grp_direction_ctl .eq. 'r_theta'                  &
     &       .or. sph_grp_direction_ctl .eq. 'R_Theta'                  &
     &       .or. sph_grp_direction_ctl .eq. 'R_THETA'                  &
     &       .or. sph_grp_direction_ctl .eq. 'Theta'                    &
     &       .or. sph_grp_direction_ctl .eq. 'theta'                    &
     &       .or. sph_grp_direction_ctl .eq. 'Theta'                    &
     &       .or. sph_grp_direction_ctl .eq. 'THETA'                    &
     &       .or. sph_grp_direction_ctl .eq. 'elevation'                &
     &       .or. sph_grp_direction_ctl .eq. 'Elevation'                &
     &       .or. sph_grp_direction_ctl .eq. 'ELEVATION') then
          iflag_grping_direction = 0
        else if(  sph_grp_direction_ctl .eq. 'cylindrical_r_z'          &
     &       .or. sph_grp_direction_ctl .eq. 'Cylindrical_R_Z'          &
     &       .or. sph_grp_direction_ctl .eq. 'CYLINDRICAL_R_Z'          &
     &       .or. sph_grp_direction_ctl .eq. 's_z'                      &
     &       .or. sph_grp_direction_ctl .eq. 'S_Z'                      &
     &       .or. sph_grp_direction_ctl .eq. 'cyrindrical'              &
     &       .or. sph_grp_direction_ctl .eq. 'Cyrindrical'              &
     &       .or. sph_grp_direction_ctl .eq. 'CYRINDRICAL'              &
     &       .or. sph_grp_direction_ctl .eq. 's'                        &
     &       .or. sph_grp_direction_ctl .eq. 'S') then
          iflag_grping_direction = 2
        else if(  sph_grp_direction_ctl .eq. 'z_theta'                  &
     &       .or. sph_grp_direction_ctl .eq. 'Z_Theta'                  &
     &       .or. sph_grp_direction_ctl .eq. 'Z_THETA') then
          iflag_grping_direction = 3
        else if(  sph_grp_direction_ctl .eq. 'r_cylindrical_r'          &
     &       .or. sph_grp_direction_ctl .eq. 'R_Cylindrical_R'          &
     &       .or. sph_grp_direction_ctl .eq. 'R_CYLINDRICAL_R'          &
     &       .or. sph_grp_direction_ctl .eq. 'r_s'                      &
     &       .or. sph_grp_direction_ctl .eq. 'r_s') then
          iflag_grping_direction = 1
        else
          call calypso_MPI_abort                                        &
     &       (ierr_mesh, 'set correct grouping mode')
        end if
      else
        call calypso_MPI_abort(ierr_mesh, 'set correct grouping mode')
      end if
!
      if (r_ele_grouping_ctl%icou .gt. 0) then
        num_r_ele_grp = r_ele_grouping_ctl%num
        call allocate_add_r_ele_grping
!
        r_ele_grp_name(1:num_r_ele_grp)                                 &
     &        = r_ele_grouping_ctl%c_tbl(1:num_r_ele_grp)
        minmax_r_ele_grping(1:num_r_ele_grp,1)                          &
     &        = r_ele_grouping_ctl%vec1(1:num_r_ele_grp)
        minmax_r_ele_grping(1:num_r_ele_grp,2)                          &
     &        = r_ele_grouping_ctl%vec2(1:num_r_ele_grp)
        call dealloc_control_array_c_r2(r_ele_grouping_ctl)
      end if
!
!
      if (s_ele_grouping_ctl%icou .gt. 0) then
        num_s_ele_grp = s_ele_grouping_ctl%num
        call allocate_add_s_ele_grping
!
        s_ele_grp_name(1:num_s_ele_grp)                                 &
     &        = s_ele_grouping_ctl%c_tbl(1:num_s_ele_grp)
        minmax_s_ele_grping(1:num_s_ele_grp,1)                          &
     &        = s_ele_grouping_ctl%vec1(1:num_s_ele_grp)
        minmax_s_ele_grping(1:num_s_ele_grp,2)                          &
     &        = s_ele_grouping_ctl%vec2(1:num_s_ele_grp)
        call dealloc_control_array_c_r2(s_ele_grouping_ctl)
      end if
!
!
      if (t_ele_grouping_ctl%icou .gt. 0) then
        num_t_ele_grp = t_ele_grouping_ctl%num
        call allocate_add_t_ele_grping
!
        t_ele_grp_name(1:num_t_ele_grp)                                 &
     &        = t_ele_grouping_ctl%c_tbl(1:num_t_ele_grp)
        minmax_t_ele_grping(1:num_t_ele_grp,1)                          &
     &        = t_ele_grouping_ctl%vec1(1:num_t_ele_grp)
        minmax_t_ele_grping(1:num_t_ele_grp,2)                          &
     &        = t_ele_grouping_ctl%vec2(1:num_t_ele_grp)
        call dealloc_control_array_c_r2(t_ele_grouping_ctl)
      end if
!
!
      if (z_ele_grouping_ctl%icou .gt. 0) then
        num_z_ele_grp = z_ele_grouping_ctl%num
        call allocate_add_z_ele_grping
!
        z_ele_grp_name(1:num_z_ele_grp)                                 &
     &        = z_ele_grouping_ctl%c_tbl(1:num_z_ele_grp)
        minmax_z_ele_grping(1:num_z_ele_grp,1)                          &
     &        = z_ele_grouping_ctl%vec1(1:num_z_ele_grp)
        minmax_z_ele_grping(1:num_z_ele_grp,2)                          &
     &        = z_ele_grouping_ctl%vec2(1:num_z_ele_grp)
        call dealloc_control_array_c_r2(z_ele_grouping_ctl)
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
