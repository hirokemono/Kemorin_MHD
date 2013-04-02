!m_cubed_sph_grp_param.f90
!      module m_cubed_sph_grp_param
!
      module m_cubed_sph_grp_param
!
!      Written by H. Matsui on Apr., 2006
!
      use m_precision
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_nod_grp_name_csp
!
      allocate( nod_grp_name_csp(num_node_grp_csp) )
      allocate( istack_nod_grp_layer_csp(0:num_node_grp_csp) )
      istack_nod_grp_layer_csp = -1
!
      end subroutine allocate_nod_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_nod_grp_layer_csp
!
      allocate( id_nod_grp_layer_csp(num_nod_layer_csp) )
      id_nod_grp_layer_csp = 0
!
      end subroutine allocate_nod_grp_layer_csp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ele_grp_name_csp
!
      allocate( ele_grp_name_csp(num_ele_grp_csp) )
      allocate( istack_ele_grp_layer_csp(0:num_ele_grp_csp) )
      istack_ele_grp_layer_csp = -1
!
      end subroutine allocate_ele_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ele_grp_layer_csp
!
      allocate( id_ele_grp_layer_csp(num_ele_layer_csp) )
      id_ele_grp_layer_csp = 0
!
      end subroutine allocate_ele_grp_layer_csp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surf_grp_name_csp
!
      allocate( surf_grp_name_csp(num_surf_grp_csp) )
      allocate( istack_surf_grp_layer_csp(0:num_surf_grp_csp) )
      istack_surf_grp_layer_csp = -1
!
      end subroutine allocate_surf_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surf_grp_layer_csp
!
      allocate( id_surf_grp_layer_csp(2,num_surf_layer_csp) )
      id_surf_grp_layer_csp = 0
!
      end subroutine allocate_surf_grp_layer_csp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_nod_grp_name_csp
!
      deallocate( nod_grp_name_csp )
      deallocate( istack_nod_grp_layer_csp )
      deallocate( id_nod_grp_layer_csp )
!
      end subroutine deallocate_nod_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ele_grp_name_csp
!
      deallocate( ele_grp_name_csp )
      deallocate( istack_ele_grp_layer_csp )
      deallocate( id_ele_grp_layer_csp )
!
      end subroutine deallocate_ele_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_surf_grp_name_csp
!
      deallocate( surf_grp_name_csp )
      deallocate( istack_surf_grp_layer_csp )
      deallocate( id_surf_grp_layer_csp )
!
      end subroutine deallocate_surf_grp_name_csp
!
!  ---------------------------------------------------------------------
!
      end module m_cubed_sph_grp_param
