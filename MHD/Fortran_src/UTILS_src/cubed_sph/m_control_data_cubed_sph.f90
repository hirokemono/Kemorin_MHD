!
!      module m_control_data_cubed_sph
!
!      Written by H. Matsui on Apr., 2006
!
      module m_control_data_cubed_sph
!
      use m_precision
      use m_read_control_elements
      use skip_comment_f
      use t_read_control_arrays
!
      implicit none
!
      character(len = kchara) :: name_ctl_shell = 'ctl_shell'
!
      character(len = kchara) :: domain_shape_ctl =  'sphere'
      character(len = kchara) :: divide_type_ctl =   'sphere'
      character(len = kchara) :: high_ele_type_ctl = 'quad'
      integer(kind = kint) :: numele_4_90deg
      integer(kind = kint) :: numele_4_vertical_ctl
      integer(kind = kint) :: nend_adjust_ctl
      integer(kind = kint) :: nstart_cube_ctl
!
!>      Structure for radial points
!!@n      radial_pnt_ctl%ivec:  radial address
!!@n      radial_pnt_ctl%vect:  radius
      type(ctl_array_ir), save :: radial_pnt_ctl
!
!
      integer(kind = kint) :: nlayer_ICB_ctl = 0
      integer(kind = kint) :: nlayer_CMB_ctl = 0
!
!>      Structure for node group name and stack
!!@n      node_grp_name_ctl%num:    Number of node group
!!@n      node_grp_name_ctl%c_tbl:  Node group name
!!@n      node_grp_name_ctl%ivec:   Stack for each node group
      type(ctl_array_ci), save :: node_grp_name_ctl
!>      Structure for node group name and stack
!!@n      node_grp_layer_ctl%num:  Number of total layers for node group
!!@n      node_grp_layer_ctl%ivec: List of radial layer
      type(ctl_array_int), save :: node_grp_layer_ctl
!
!>      Structure for element group name and stack
!!@n      elem_grp_name_ctl%num:    Number of element group
!!@n      elem_grp_name_ctl%c_tbl:  element group name
!!@n      elem_grp_name_ctl%ivec:   Stack for each element group
      type(ctl_array_ci), save :: elem_grp_name_ctl
!>      Structure for node group name and stack
!!@n      elem_grp_layer_ctl%num:  Number of total layers
!!                                for element group
!!@n      elem_grp_layer_ctl%ivec: List of radial layer
      type(ctl_array_int), save :: elem_grp_layer_ctl
!
!>      Structure for surface group name and stack
!!@n      surf_grp_name_ctl%num:    Number of surface group
!!@n      surf_grp_name_ctl%c_tbl:  surface group name
!!@n      surf_grp_name_ctl%ivec:   Stack for each surface group
      type(ctl_array_ci), save :: surf_grp_name_ctl
!>      Structure for node group name and stack
!!@n      surf_grp_layer_ctl%num:  Number of total layers
!!                                for element group
!!@n      surf_grp_layer_ctl%c_tbl: List of surface type name
!!@n      surf_grp_layer_ctl%ivec: List of radial layer
      type(ctl_array_ci), save :: surf_grp_layer_ctl
!
!>      Structure for radial points
!!@n      edge_latitude_ctl%ivec:  radial address
!!@n      edge_latitude_ctl%vect:  radius
      type(ctl_array_ir), save :: edge_latitude_ctl
!
!>      Structure for coarsing level
!!@n      sph_coarsing_ctl%int1: Coarsing level for radial direction
!!@n      sph_coarsing_ctl%int2: Coarsing level on sphere
      type(ctl_array_i2), save :: sph_coarsing_ctl
!
!   Top level
!
      character(len=kchara) :: hd_shell_ctl = 'make_shell'
      integer (kind=kint) :: i_shell_ctl = 0
!
!   3rd level for shell define
!
      character(len=kchara), parameter                             &
     &             :: hd_domain_shape =   'domain_shape'
      character(len=kchara), parameter                             &
     &             :: hd_divide_def =     'divide_mode'
      character(len=kchara), parameter                             &
     &             :: hd_high_ele_type =  'element_type'
      character(len=kchara), parameter                             &
     &             :: hd_numele_4_90deg = 'numele_4_90deg'
      character(len=kchara), parameter                             &
     &             :: hd_numele_4_vert =  'numele_4_vertical'
      character(len=kchara), parameter                             &
     &             :: hd_nend_adjust =    'nend_adjust_ctl'
      character(len=kchara), parameter                             &
     &             :: hd_nstart_cube =    'nstart_square_ctl'
      character(len=kchara), parameter                             &
     &             :: hd_cubed_sph_radius = 'r_layer'
      character(len=kchara), parameter                             &
     &             :: hd_edge_latitude =  'edge_latitude_ctl'
      integer (kind=kint) :: i_domain_shape =   0
      integer (kind=kint) :: i_divide_def =     0
      integer (kind=kint) :: i_high_ele_type =  0
      integer (kind=kint) :: i_numele_4_90deg = 0
      integer (kind=kint) :: i_numele_4_vert =  0
      integer (kind=kint) :: i_nend_adjust =    0
      integer (kind=kint) :: i_nstart_cube =    0
!
!   3rd level for boundary define
!
      character(len=kchara), parameter                             &
     &             :: hd_nlayer_ICB = 'nlayer_ICB'
      character(len=kchara), parameter                             &
     &             :: hd_nlayer_CMB = 'nlayer_CMB'
!
      integer (kind=kint) :: i_nlayer_ICB =     0
      integer (kind=kint) :: i_nlayer_CMB =     0
!
!   3rd level for coarse grid
!
      character(len=kchara), parameter                                  &
     &             :: hd_num_level_coarse ='sp_r_coarse_ratio'
!
!   2nd level for make_shell
!
      character(len=kchara), parameter                                  &
     &             :: hd_cubed_sph_def = 'cubed_sphere_def'
      character(len=kchara), parameter                                  &
     &             :: hd_boundaries =   'boundaries_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_coarse_shell = 'coarse_shell_ctl'
      integer (kind=kint) :: i_cubed_sph_def = 0
      integer (kind=kint) :: i_boundaries =   0
      integer (kind=kint) :: i_coarse_shell = 0
!
!   3rd level for boundary define
!
      character(len=kchara), parameter                                  &
     &             :: hd_node_grp_def = 'node_group_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_ele_grp_def =  'element_group_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_surf_grp_def = 'surface_group_ctl'
      integer (kind=kint) :: i_node_grp_def = 0
      integer (kind=kint) :: i_ele_grp_def =  0
      integer (kind=kint) :: i_surf_grp_def = 0
!
!   4th level for node group def
!
      character(len=kchara), parameter                                  &
     &             :: hd_num_nod_grp =   'nod_grp_name_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_num_nod_layer = 'nod_layer_id_ctl'
!
!   4th level for element group def
!
      character(len=kchara), parameter                                  &
     &             :: hd_num_ele_grp =   'ele_grp_name_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_num_ele_layer = 'ele_layer_id_ctl'
!
!   4th level for surface group def
!
      character(len=kchara), parameter                                  &
     &             :: hd_num_sf_grp =   'surf_grp_name_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_num_sf_layer = 'surf_layer_id_ctl'
!
      private :: hd_shell_ctl, hd_domain_shape
      private :: hd_divide_def, hd_high_ele_type, hd_numele_4_90deg
      private :: hd_numele_4_vert, hd_nend_adjust, hd_nstart_cube
      private :: hd_cubed_sph_radius, hd_edge_latitude
      private :: hd_nlayer_ICB, hd_nlayer_CMB
      private :: hd_num_level_coarse
!
      private :: hd_cubed_sph_def, i_cubed_sph_def
      private :: hd_boundaries,    i_boundaries
      private :: hd_coarse_shell,  i_coarse_shell
!
      private :: hd_node_grp_def, i_node_grp_def
      private :: hd_ele_grp_def,  i_ele_grp_def
      private :: hd_surf_grp_def, i_surf_grp_def
!
      private :: hd_num_nod_grp, hd_num_ele_grp, hd_num_sf_grp
      private :: hd_num_nod_layer, hd_num_ele_layer, hd_num_sf_layer
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
        call read_control_array_i_r                                     &
       &   (hd_cubed_sph_radius, radial_pnt_ctl)
        call read_control_array_i_r                                     &
       &   (hd_edge_latitude, edge_latitude_ctl)
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
        call read_control_array_c_i(hd_num_nod_grp, node_grp_name_ctl)
        call read_control_array_i1                                      &
     &     (hd_num_nod_layer, node_grp_layer_ctl)
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
        call read_control_array_c_i(hd_num_ele_grp, elem_grp_name_ctl)
        call read_control_array_i1                                      &
     &     (hd_num_ele_layer, elem_grp_layer_ctl)
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
        call read_control_array_c_i(hd_num_sf_grp, surf_grp_name_ctl)
        call read_control_array_c_i                                     &
     &     (hd_num_sf_layer, surf_grp_layer_ctl)
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
        call read_control_array_i2                                      &
     &     (hd_num_level_coarse, sph_coarsing_ctl)
      end do
!
      end subroutine read_ctl_4_coarse_shell
!
!   --------------------------------------------------------------------
!
      end module m_control_data_cubed_sph
