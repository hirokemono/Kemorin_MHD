!read_control_cubed_sph
!      module read_control_cubed_sph
!
!        programmed by H.Matsui on March. 2006
!
!      subroutine read_control_4_shell
!
      module read_control_cubed_sph
!
      use m_precision
!
      use m_read_control_elements
      use m_control_data_cubed_sph
!
      use skip_comment_f
!
      implicit  none
!
!   2nd level for make_shell
!
      character(len=kchara), parameter                             &
     &             :: hd_cubed_sph_def = 'cubed_sphere_def'
      character(len=kchara), parameter                             &
     &             :: hd_boundaries =   'boundaries_ctl'
      character(len=kchara), parameter                             &
     &             :: hd_coarse_shell = 'coarse_shell_ctl'
      integer (kind=kint) :: i_cubed_sph_def = 0
      integer (kind=kint) :: i_boundaries =   0
      integer (kind=kint) :: i_coarse_shell = 0
!
!   3rd level for boundary define
!
      character(len=kchara), parameter                             &
     &             :: hd_node_grp_def = 'node_group_ctl'
      character(len=kchara), parameter                             &
     &             :: hd_ele_grp_def =  'element_group_ctl'
      character(len=kchara), parameter                             &
     &             :: hd_surf_grp_def = 'surface_group_ctl'
      integer (kind=kint) :: i_node_grp_def = 0
      integer (kind=kint) :: i_ele_grp_def =  0
      integer (kind=kint) :: i_surf_grp_def = 0
!
!
      private :: hd_cubed_sph_def, i_cubed_sph_def
      private :: hd_boundaries,    i_boundaries
      private :: hd_coarse_shell,  i_coarse_shell
!
      private :: hd_node_grp_def, i_node_grp_def
      private :: hd_ele_grp_def,  i_ele_grp_def
      private :: hd_surf_grp_def, i_surf_grp_def
!
      private :: read_control_data_4_shell
      private :: read_ctl_4_shell_def, read_ctl_shell_boundary
      private :: read_ctl_4_coarse_shell
      private :: read_ctl_nod_bc_4_shell, read_ctl_ele_bc_4_shell
      private :: read_ctl_surf_bc_4_shell
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_shell
!
      open (ctl_file_code, file=name_ctl_shell)
!
      call read_control_data_4_shell
!
      close(ctl_file_code)
!
      end subroutine read_control_4_shell
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_data_4_shell
!
!
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_shell_ctl, i_shell_ctl)
        if(i_shell_ctl .gt. 0) exit
!
        call read_ctl_4_shell_def
        call read_ctl_shell_boundary
        call read_ctl_4_coarse_shell
      end do
!
      end subroutine read_control_data_4_shell
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_4_shell_def
!
!
      if(right_begin_flag(hd_cubed_sph_def) .eq. 0) return
      if (i_cubed_sph_def .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_cubed_sph_def, i_cubed_sph_def)
        if(i_cubed_sph_def .gt. 0) exit
!
        call find_control_array_flag(hd_numlayer_shell,                 &
     &        numlayer_shell_ctl)
        if(numlayer_shell_ctl.gt.0 .and. i_numlayer_shell.eq.0) then
          call allocate_layers
          call read_control_array_vect_list(hd_numlayer_shell,          &
     &       numlayer_shell_ctl, i_numlayer_shell, name_layer, r_layer)
        end if
!
        call find_control_array_flag(hd_edge_latitude,                  &
     &        num_edge_latitude_ctl)
        if(num_edge_latitude_ctl.gt.0 .and. i_edge_latitude.eq.0) then
          call allocate_edge_latitude_ctl
          call read_control_array_int_r_list(hd_edge_latitude,          &
     &        num_edge_latitude_ctl, i_edge_latitude,                   &
     &        kr_edge_latitude_ctl, edge_latitude_ctl)
        end if
!
!
        call read_character_ctl_item(hd_domain_shape,                   &
     &        i_domain_shape, domain_shape_ctl)
        call read_character_ctl_item(hd_divide_def,                     &
     &        i_divide_def, divide_type_ctl)
        call read_character_ctl_item(hd_high_ele_type,                  &
     &        i_high_ele_type, high_ele_type_ctl)
!
        call read_integer_ctl_item(hd_numele_4_90deg,                   &
     &        i_numele_4_90deg, numele_4_90deg)
        call read_integer_ctl_item(hd_numele_4_vert,                    &
     &        i_numele_4_vert, numele_4_vertical_ctl)
        call read_integer_ctl_item(hd_nend_adjust,                      &
     &        i_nend_adjust, nend_adjust_ctl)
        call read_integer_ctl_item(hd_nstart_cube,                      &
     &        i_nstart_cube, nstart_cube_ctl)
      end do
!
      end subroutine read_ctl_4_shell_def
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_shell_boundary
!
!
      if(right_begin_flag(hd_boundaries) .eq. 0) return
      if (i_boundaries .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_boundaries, i_boundaries)
        if(i_boundaries .gt. 0) exit
!
        call read_ctl_nod_bc_4_shell
        call read_ctl_ele_bc_4_shell
        call read_ctl_surf_bc_4_shell
!
!
        call read_integer_ctl_item(hd_nlayer_ICB,                       &
     &        i_nlayer_ICB, nlayer_ICB_ctl)
        call read_integer_ctl_item(hd_nlayer_CMB,                       &
     &        i_nlayer_CMB, nlayer_CMB_ctl)
      end do
!
      end subroutine read_ctl_shell_boundary
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_nod_bc_4_shell
!
!
      if(right_begin_flag(hd_node_grp_def) .eq. 0) return
      if (i_node_grp_def .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_node_grp_def, i_node_grp_def)
        if(i_node_grp_def .gt. 0) exit
!
        call find_control_array_flag(hd_num_nod_grp, num_node_grp_ctl)
        if(num_node_grp_ctl.gt.0 .and. i_num_nod_grp.eq.0) then
            call allocate_nod_grp_name_ctl
            call read_control_array_int_v_list(hd_num_nod_grp,          &
     &          num_node_grp_ctl, i_num_nod_grp,                        &
     &          nod_grp_name_ctl, istack_nod_grp_layer_ctl(1))
        end if
!
        call find_control_array_flag(hd_num_nod_layer,                  &
     &        num_nod_layer_ctl)
        if(num_nod_layer_ctl.gt.0 .and. i_num_nod_layer.eq.0) then
          call allocate_nod_grp_layer_ctl
          call read_control_array_int_list(hd_num_nod_layer,            &
     &          num_nod_layer_ctl, i_num_nod_layer,                     &
     &          id_nod_grp_layer_ctl)
        end if
      end do
!
      end subroutine read_ctl_nod_bc_4_shell
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_ele_bc_4_shell
!
!
      if(right_begin_flag(hd_ele_grp_def) .eq. 0) return
      if (i_ele_grp_def .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_ele_grp_def, i_ele_grp_def)
        if(i_ele_grp_def .gt. 0) exit
!
        call find_control_array_flag(hd_num_ele_grp, num_ele_grp_ctl)
        if(num_ele_grp_ctl.gt.0 .and. i_num_ele_grp.eq.0) then
            call allocate_ele_grp_name_ctl
            call read_control_array_int_v_list(hd_num_ele_grp,          &
     &          num_ele_grp_ctl, i_num_ele_grp,                         &
     &          ele_grp_name_ctl, istack_ele_grp_layer_ctl(1))
        end if
!
        call find_control_array_flag(hd_num_ele_layer,                  &
     &      num_ele_layer_ctl)
        if(num_ele_layer_ctl.gt.0 .and. i_num_ele_layer.eq.0) then
            call allocate_ele_grp_layer_ctl
            call read_control_array_int_list(hd_num_ele_layer,          &
     &          num_ele_layer_ctl, i_num_ele_layer,                     &
     &          id_ele_grp_layer_ctl)
        end if
      end do
!
      end subroutine read_ctl_ele_bc_4_shell
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_surf_bc_4_shell
!
!
      if(right_begin_flag(hd_surf_grp_def) .eq. 0) return
      if (i_surf_grp_def .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_surf_grp_def, i_surf_grp_def)
        if(i_surf_grp_def .gt. 0) exit
!
        call find_control_array_flag(hd_num_sf_grp, num_surf_grp_ctl)
        if(num_surf_grp_ctl.gt.0 .and. i_num_sf_grp.eq.0) then
            call allocate_surf_grp_name_ctl
            call read_control_array_int_v_list(hd_num_sf_grp,           &
     &          num_surf_grp_ctl, i_num_sf_grp,                         &
     &          surf_grp_name_ctl, istack_surf_grp_layer_ctl(1))
        end if
!
        call find_control_array_flag(hd_num_sf_layer,                   &
     &        num_surf_layer_ctl)
        if(num_surf_layer_ctl.gt.0 .and. i_num_sf_layer.eq.0) then
            call allocate_surf_grp_layer_ctl
            call read_control_array_int_v_list(hd_num_sf_layer,         &
     &          num_surf_layer_ctl, i_num_sf_layer,                     &
     &          surf_grp_layer_type_ctl, id_surf_grp_layer_ctl)
        end if
      end do
!
      end subroutine read_ctl_surf_bc_4_shell
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_4_coarse_shell
!
!
      if(right_begin_flag(hd_coarse_shell) .eq. 0) return
      if (i_coarse_shell .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_coarse_shell, i_coarse_shell)
        if(i_coarse_shell .gt. 0) exit
!
        call find_control_array_flag(hd_num_level_coarse,               &
     &      num_level_coarse)
        if(num_level_coarse.gt.0 .and. i_num_level_coarse.eq.0) then
            call allocate_coarse_level_ctl
            call read_control_array_int2_list(hd_num_level_coarse,      &
     &          num_level_coarse, i_num_level_coarse,                   &
     &            sp_r_coarse_ratio(1,1), sp_r_coarse_ratio(1,2) )
        end if
      end do
!
      end subroutine read_ctl_4_coarse_shell
!
!   --------------------------------------------------------------------
!
      end module read_control_cubed_sph
