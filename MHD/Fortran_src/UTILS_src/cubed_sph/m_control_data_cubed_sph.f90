!
!      module m_control_data_cubed_sph
!
      module m_control_data_cubed_sph
!
!      Written by H. Matsui on Apr., 2006
!
      use m_precision
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
      integer(kind = kint) :: numlayer_shell_ctl
      real(kind = kreal), allocatable :: r_layer(:)
      character(len = kchara), allocatable :: name_layer(:)
!
!
      integer(kind = kint) :: num_node_grp_ctl
      integer(kind = kint) :: num_nod_layer_ctl
      character(len = kchara), allocatable :: nod_grp_name_ctl(:)
      integer(kind = kint), allocatable :: istack_nod_grp_layer_ctl(:)
      integer(kind = kint), allocatable :: id_nod_grp_layer_ctl(:)
!
      integer(kind = kint) :: num_ele_grp_ctl
      integer(kind = kint) :: num_ele_layer_ctl
      character(len = kchara), allocatable :: ele_grp_name_ctl(:)
      integer(kind = kint), allocatable :: istack_ele_grp_layer_ctl(:)
      integer(kind = kint), allocatable :: id_ele_grp_layer_ctl(:)
!
      integer(kind = kint) :: num_surf_grp_ctl
      integer(kind = kint) :: num_surf_layer_ctl
      character(len = kchara), allocatable :: surf_grp_name_ctl(:)
      integer(kind = kint), allocatable :: istack_surf_grp_layer_ctl(:)
      integer(kind = kint), allocatable :: id_surf_grp_layer_ctl(:)
      character(len = kchara), allocatable :: surf_grp_layer_type_ctl(:)
!
!
      integer(kind = kint) :: nlayer_ICB_ctl = 0
      integer(kind = kint) :: nlayer_CMB_ctl = 0
!
      integer(kind = kint) :: num_level_coarse = 0
      integer(kind = kint), allocatable :: sp_r_coarse_ratio(:,:)
!
!
      integer(kind = kint) :: num_edge_latitude_ctl = 0
      integer(kind = kint), allocatable :: kr_edge_latitude_ctl(:)
      real(kind = kreal), allocatable :: edge_latitude_ctl(:)
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
     &             :: hd_numlayer_shell = 'r_layer'
      character(len=kchara), parameter                             &
     &             :: hd_edge_latitude =  'edge_latitude_ctl'
      integer (kind=kint) :: i_domain_shape =   0
      integer (kind=kint) :: i_divide_def =     0
      integer (kind=kint) :: i_high_ele_type =  0
      integer (kind=kint) :: i_numele_4_90deg = 0
      integer (kind=kint) :: i_numele_4_vert =  0
      integer (kind=kint) :: i_nend_adjust =    0
      integer (kind=kint) :: i_nstart_cube =    0
      integer (kind=kint) :: i_numlayer_shell = 0
      integer (kind=kint) :: i_edge_latitude =  0
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
!   4th level for node group def
!
      character(len=kchara), parameter                             &
     &             :: hd_num_nod_grp =   'nod_grp_name_ctl'
      character(len=kchara), parameter                             &
     &             :: hd_num_nod_layer = 'nod_layer_id_ctl'
      integer (kind=kint) :: i_num_nod_grp =   0
      integer (kind=kint) :: i_num_nod_layer = 0
!
!   4th level for element group def
!
      character(len=kchara), parameter                             &
     &             :: hd_num_ele_grp =   'ele_grp_name_ctl'
      character(len=kchara), parameter                             &
     &             :: hd_num_ele_layer = 'ele_layer_id_ctl'
      integer (kind=kint) :: i_num_ele_grp =   0
      integer (kind=kint) :: i_num_ele_layer = 0
!
!   4th level for surface group def
!
      character(len=kchara), parameter                             &
     &             :: hd_num_sf_grp =   'surf_grp_name_ctl'
      character(len=kchara), parameter                             &
     &             :: hd_num_sf_layer = 'surf_layer_id_ctl'
      integer (kind=kint) :: i_num_sf_grp =   0
      integer (kind=kint) :: i_num_sf_layer = 0
!
!   3rd level for coarse grid
!
      character(len=kchara), parameter                             &
     &             :: hd_num_level_coarse ='sp_r_coarse_ratio'
      integer (kind=kint) :: i_num_level_coarse = 0
!
!      subroutine allocate_layers
!      subroutine allocate_edge_latitude_ctl
!      subroutine allocate_coarse_level_ctl
!
!      subroutine deallocate_layers
!      subroutine deallocate_edge_latitude_ctl
!      subroutine deallocate_coarse_level_ctl
!
!      subroutine deallocate_nod_grp_name_ctl
!      subroutine deallocate_nod_grp_layer_ctl
!      subroutine deallocate_ele_grp_name_ctl
!      subroutine deallocate_ele_grp_layer_ctl
!      subroutine deallocate_surf_grp_name_ctl
!      subroutine deallocate_surf_grp_layer_ctl
!
!      subroutine allocate_nod_grp_name_ctl
!      subroutine allocate_ele_grp_name_ctl
!      subroutine allocate_surf_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_layers
!
      allocate( name_layer(numlayer_shell_ctl) )
      allocate( r_layer(numlayer_shell_ctl) )
      r_layer = 0.0d0
!
      end subroutine allocate_layers
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_edge_latitude_ctl
!
      allocate( kr_edge_latitude_ctl(num_edge_latitude_ctl) )
      allocate( edge_latitude_ctl(num_edge_latitude_ctl) )
      kr_edge_latitude_ctl = 0
      edge_latitude_ctl = 0.0d0
!
      end subroutine allocate_edge_latitude_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_coarse_level_ctl
!
      allocate( sp_r_coarse_ratio(num_level_coarse,2) )
      sp_r_coarse_ratio = 0
!
      end subroutine allocate_coarse_level_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_layers
!
      deallocate( name_layer )
      deallocate( r_layer    )
!
      end subroutine deallocate_layers
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_edge_latitude_ctl
!
      deallocate( kr_edge_latitude_ctl)
      deallocate( edge_latitude_ctl )
!
      end subroutine deallocate_edge_latitude_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_coarse_level_ctl
!
      deallocate( sp_r_coarse_ratio )
!
      end subroutine deallocate_coarse_level_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_nod_grp_name_ctl
!
      allocate( nod_grp_name_ctl(num_node_grp_ctl) )
      allocate( istack_nod_grp_layer_ctl(0:num_node_grp_ctl) )
      istack_nod_grp_layer_ctl = 0
!
      end subroutine allocate_nod_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_nod_grp_layer_ctl
!
      allocate( id_nod_grp_layer_ctl(num_nod_layer_ctl) )
      id_nod_grp_layer_ctl = 0
!
      end subroutine allocate_nod_grp_layer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ele_grp_name_ctl
!
      allocate( ele_grp_name_ctl(num_ele_grp_ctl) )
      allocate( istack_ele_grp_layer_ctl(0:num_ele_grp_ctl) )
      istack_ele_grp_layer_ctl = 0
!
      end subroutine allocate_ele_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ele_grp_layer_ctl
!
      allocate( id_ele_grp_layer_ctl(num_ele_layer_ctl) )
      id_ele_grp_layer_ctl = 0
!
      end subroutine allocate_ele_grp_layer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surf_grp_name_ctl
!
      allocate( surf_grp_name_ctl(num_surf_grp_ctl) )
      allocate( istack_surf_grp_layer_ctl(0:num_surf_grp_ctl) )
      istack_surf_grp_layer_ctl = 0
!
      end subroutine allocate_surf_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surf_grp_layer_ctl
!
      allocate( id_surf_grp_layer_ctl(num_surf_layer_ctl) )
      allocate( surf_grp_layer_type_ctl(num_surf_layer_ctl) )
      id_surf_grp_layer_ctl = 0
!
      end subroutine allocate_surf_grp_layer_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_nod_grp_name_ctl
!
      deallocate( nod_grp_name_ctl )
      deallocate( istack_nod_grp_layer_ctl )
      deallocate( id_nod_grp_layer_ctl )
!
      end subroutine deallocate_nod_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ele_grp_name_ctl
!
      deallocate( ele_grp_name_ctl )
      deallocate( istack_ele_grp_layer_ctl )
      deallocate( id_ele_grp_layer_ctl )
!
      end subroutine deallocate_ele_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_surf_grp_name_ctl
!
      deallocate( surf_grp_name_ctl )
      deallocate( istack_surf_grp_layer_ctl )
      deallocate( id_surf_grp_layer_ctl )
      deallocate( surf_grp_layer_type_ctl )
!
      end subroutine deallocate_surf_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      end module m_control_data_cubed_sph
