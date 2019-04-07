!t_cubed_sph_grp_param.f90
!      module t_cubed_sph_grp_param
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine allocate_nod_grp_name_csp
!      subroutine allocate_nod_grp_layer_csp
!      subroutine allocate_ele_grp_name_csp
!      subroutine allocate_ele_grp_layer_csp
!      subroutine allocate_surf_grp_name_csp
!      subroutine allocate_surf_grp_layer_csp
!
!      subroutine deallocate_nod_grp_name_csp
!      subroutine deallocate_ele_grp_name_csp
!      subroutine deallocate_surf_grp_name_csp
!
      module t_cubed_sph_grp_param
!
      use m_precision
      use t_group_data
!
      implicit none
!
      integer(kind = kint) :: nlayer_ICB =    0
      integer(kind = kint) :: nlayer_CMB =    0
      integer(kind = kint) :: nlayer_EXT =    0
!
      integer(kind = kint) :: nr_icb, nr_cmb
!
      integer(kind = kint) :: num_node_grp_csp
      integer(kind = kint) :: num_nod_layer_csp
      character(len = kchara), allocatable :: nod_grp_name_csp(:)
      integer(kind = kint), allocatable :: istack_nod_grp_layer_csp(:)
      integer(kind = kint), allocatable :: id_nod_grp_layer_csp(:)
!
      integer(kind = kint) :: num_ele_grp_csp
      integer(kind = kint) :: num_ele_layer_csp
      character(len = kchara), allocatable :: ele_grp_name_csp(:)
      integer(kind = kint), allocatable :: istack_ele_grp_layer_csp(:)
      integer(kind = kint), allocatable :: id_ele_grp_layer_csp(:)
!
      integer(kind = kint) :: num_surf_grp_csp
      integer(kind = kint) :: num_surf_layer_csp
      character(len = kchara), allocatable :: surf_grp_name_csp(:)
      integer(kind = kint), allocatable :: istack_surf_grp_layer_csp(:)
      integer(kind = kint), allocatable :: id_surf_grp_layer_csp(:,:)
!
      type(group_data) :: csp_nod_grp
      type(group_data) :: csp_ele_grp
      type(surface_group_data) :: csp_surf_grp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_nod_grp_name_csp
!
      call alloc_group_num(csp_nod_grp)
!
      end subroutine allocate_nod_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_nod_grp_layer_csp
!
      call alloc_group_item(csp_nod_grp)
!
      end subroutine allocate_nod_grp_layer_csp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ele_grp_name_csp
!
      call alloc_group_num(csp_ele_grp)
!
      end subroutine allocate_ele_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ele_grp_layer_csp
!
      call alloc_group_item(csp_ele_grp)
!
      end subroutine allocate_ele_grp_layer_csp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surf_grp_name_csp
!
      call alloc_sf_group_num(csp_surf_grp)
!
      end subroutine allocate_surf_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surf_grp_layer_csp
!
      call alloc_sf_group_item(csp_surf_grp)
!
      end subroutine allocate_surf_grp_layer_csp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_nod_grp_name_csp
!
      call dealloc_group(csp_nod_grp)
!
      end subroutine deallocate_nod_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ele_grp_name_csp
!
      call dealloc_group(csp_ele_grp)
!
      end subroutine deallocate_ele_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_surf_grp_name_csp
!
      call dealloc_sf_group(csp_surf_grp)
!
      end subroutine deallocate_surf_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      end module t_cubed_sph_grp_param
