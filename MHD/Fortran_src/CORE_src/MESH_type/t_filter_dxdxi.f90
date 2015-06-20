!>@file  t_filter_dxdxi.f90
!!       module t_filter_dxdxi
!!
!!@author H. Matsui
!!@date   Programmed on March, 2009
!!@n      Modified by H. Matsui on Feb., 2012
!
!> @brief Strucure for dxdxi for SGS model
!!
!!@verbatim
!!      subroutine alloc_jacobians_node(nnod_filter_mom, filter_dxi)
!!        integer (kind = kint), intent(in) :: nnod_filter_mom
!!      subroutine alloc_jacobians_ele(nele_filter_mom, filter_dxi)
!!        integer (kind = kint), intent(in) :: nele_filter_mom
!!      subroutine dealloc_jacobians_node(filter_dxi)
!!      subroutine dealloc_jacobians_ele(filter_dxi)
!!        type(dxdxi_data_type), intent(inout) :: filter_dxi
!!
!!      subroutine alloc_dxidxs_node(nnod_filter_mom, dxidxs)
!!        integer (kind = kint), intent(in) :: nnod_filter_mom
!!      subroutine alloc_dxidxs_ele(nele_filter_mom, dxidxs)
!!        integer (kind = kint), intent(in) :: nele_filter_mom
!!      subroutine dealloc_dxidxs_node(dxidxs)
!!      subroutine dealloc_dxidxs_ele(dxidxs)
!!        type(dxidx_data_type), intent(inout) :: dxidxs
!!
!!          1st difference of elengh_nod
!!              (node ID, direction of diffrence)
!!      dxdxi_nod(:)... dxi_nod%dx%df_dxi(:)
!!      dxdei_nod(:)... dxi_nod%dx%df_dei(:)
!!      dxdzi_nod(:)... dxi_nod%dx%df_dzi(:)
!!      dydxi_nod(:)... dxi_nod%dy%df_dxi(:)
!!      dydei_nod(:)... dxi_nod%dy%df_dei(:)
!!      dydzi_nod(:)... dxi_nod%dy%df_dzi(:)
!!      dzdxi_nod(:)... dxi_nod%dz%df_dxi(:)
!!      dzdei_nod(:)... dxi_nod%dz%df_dei(:)
!!      dzdzi_nod(:)... dxi_nod%dz%df_dzi(:)
!!
!!          1st difference of elengh_nod
!!              (node ID, direction of diffrence)
!!      dxdxi_ele(:)... dxi_ele%dx%df_dxi(:)
!!      dxdei_ele(:)... dxi_ele%dx%df_dei(:)
!!      dxdzi_ele(:)... dxi_ele%dx%df_dzi(:)
!!      dydxi_ele(:)... dxi_ele%dy%df_dxi(:)
!!      dydei_ele(:)... dxi_ele%dy%df_dei(:)
!!      dydzi_ele(:)... dxi_ele%dy%df_dzi(:)
!!      dzdxi_ele(:)... dxi_ele%dz%df_dxi(:)
!!      dzdei_ele(:)... dxi_ele%dz%df_dei(:)
!!      dzdzi_ele(:)... dxi_ele%dz%df_dzi(:)
!!
!!          dxi/dx on node
!!              (node ID, direction of diffrence)
!!      dxidx_nod(:)... dx_nod%dxi%df_dx(:)
!!      dxidy_nod(:)... dx_nod%dxi%df_dy(:)
!!      dxidz_nod(:)... dx_nod%dxi%df_dz(:)
!!      deidx_nod(:)... dx_nod%dei%df_dx(:)
!!      deidy_nod(:)... dx_nod%dei%df_dy(:)
!!      deidz_nod(:)... dx_nod%dei%df_dz(:)
!!      dzidx_nod(:)... dx_nod%dzi%df_dx(:)
!!      dzidy_nod(:)... dx_nod%dzi%df_dy(:)
!!      dzidz_nod(:)... dx_nod%dzi%df_dz(:)
!!
!!          dxi/dx on element
!!              (node ID, direction of diffrence)
!!      dxdxi_ele(:)... dxi_ele%dx%df_dxi(:)
!!      dxdei_ele(:)... dxi_ele%dx%df_dei(:)
!!      dxdzi_ele(:)... dxi_ele%dx%df_dzi(:)
!!      dydxi_ele(:)... dxi_ele%dy%df_dxi(:)
!!      dydei_ele(:)... dxi_ele%dy%df_dei(:)
!!      dydzi_ele(:)... dxi_ele%dy%df_dzi(:)
!!      dzdxi_ele(:)... dxi_ele%dz%df_dxi(:)
!!      dzdei_ele(:)... dxi_ele%dz%df_dei(:)
!!      dzdzi_ele(:)... dxi_ele%dz%df_dzi(:)
!!@endverbatim
!
module t_filter_dxdxi
!
      use m_precision
!
      implicit none
!
!
!>        Structure for d/dxi
      type dxdxi_diff_type
!>        delivative in \xi-diretion
        real(kind=kreal), pointer :: df_dxi(:)
!>        delivative in \eta-diretion
        real(kind=kreal), pointer :: df_dei(:)
!>        delivative in \zeta-diretion
        real(kind=kreal), pointer :: df_dzi(:)
      end  type dxdxi_diff_type
!
!>        Structure for d/dx
      type dxidx_diff_type
!>        delivative in x-diretion
        real(kind=kreal), pointer :: df_dx(:)
!>        delivative in y-diretion
        real(kind=kreal), pointer :: df_dy(:)
!>        delivative in z-diretion
        real(kind=kreal), pointer :: df_dz(:)
      end  type dxidx_diff_type
!
!
!>        Structure for dx
      type dxdxi_direction_type
!>        delivative for x (dx/dxi)
        type(dxdxi_diff_type) :: dx
!>        delivative for y (dy/dxi)
        type(dxdxi_diff_type) :: dy
!>        delivative for z (dz/dxi)
        type(dxdxi_diff_type) :: dz
      end type dxdxi_direction_type
!
!>        Structure for dxi
      type dxidx_direction_type
!>        delivative for \xi (dxi/dx)
        type(dxidx_diff_type) :: dxi
!>        delivative for \eta (deta/dx)
        type(dxidx_diff_type) :: dei
!>        delivative for \zeta (dzeta/dx)
        type(dxidx_diff_type) :: dzi
      end type dxidx_direction_type
!
!>        Structure for dx/dxi at nodes and elements
      type dxdxi_data_type
!>        Structure for dx/dxi at nodes
        type(dxdxi_direction_type) :: dxi_nod
!>        Structure for dx/dxi in elements
        type(dxdxi_direction_type) :: dxi_ele
      end type dxdxi_data_type
!
!>        Structure for dx/dxi at nodes and elements
      type dxidx_data_type
!>        Structure for dx/dxi at nodes
        type(dxidx_direction_type) :: dx_nod
!>        Structure for dx/dxi in elements
        type(dxidx_direction_type) :: dx_ele
      end type dxidx_data_type
!
      private :: alloc_dxi_diff_type, dealloc_dxi_diff_type
      private :: alloc_dx_diff_type,  dealloc_dx_diff_type
      private :: alloc_dxdxi_diff_type, dealloc_dxdxi_diff_type
      private :: alloc_dxidx_diff_type, dealloc_dxidx_diff_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dxi_diff_type(num_fmom, dx)
!
      integer (kind = kint), intent(in) :: num_fmom
      type(dxdxi_diff_type), intent(inout) :: dx
!
!
      allocate(dx%df_dxi(num_fmom))
      allocate(dx%df_dei(num_fmom))
      allocate(dx%df_dzi(num_fmom))
!
      if(num_fmom .gt. 0) then
        dx%df_dxi = 0.0d0
        dx%df_dei = 0.0d0
        dx%df_dzi = 0.0d0
      end if
!
      end subroutine alloc_dxi_diff_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dx_diff_type(num_fmom, dxi)
!
      integer (kind = kint), intent(in) :: num_fmom
      type(dxidx_diff_type), intent(inout) :: dxi
!
!
      allocate(dxi%df_dx(num_fmom))
      allocate(dxi%df_dy(num_fmom))
      allocate(dxi%df_dz(num_fmom))
!
      if(num_fmom .gt. 0) then
        dxi%df_dx = 0.0d0
        dxi%df_dy = 0.0d0
        dxi%df_dz = 0.0d0
      end if
!
      end subroutine alloc_dx_diff_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_dxi_diff_type(dx)
!
      type(dxdxi_diff_type), intent(inout) :: dx
!
!
      deallocate(dx%df_dxi, dx%df_dei, dx%df_dzi)
!
      end subroutine dealloc_dxi_diff_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_dx_diff_type(dxi)
!
      type(dxidx_diff_type), intent(inout) :: dxi
!
!
      deallocate(dxi%df_dx, dxi%df_dy, dxi%df_dz)
!
      end subroutine dealloc_dx_diff_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_dxdxi_diff_type(num_fmom, dxi)
!
      integer (kind = kint), intent(in) :: num_fmom
      type(dxdxi_direction_type), intent(inout) :: dxi
!
!
      call alloc_dxi_diff_type(num_fmom, dxi%dx)
      call alloc_dxi_diff_type(num_fmom, dxi%dy)
      call alloc_dxi_diff_type(num_fmom, dxi%dz)
!
      end subroutine alloc_dxdxi_diff_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dxidx_diff_type(num_fmom, dx)
!
      integer (kind = kint), intent(in) :: num_fmom
      type(dxidx_direction_type), intent(inout) :: dx
!
!
      call alloc_dx_diff_type(num_fmom, dx%dxi)
      call alloc_dx_diff_type(num_fmom, dx%dei)
      call alloc_dx_diff_type(num_fmom, dx%dzi)
!
      end subroutine alloc_dxidx_diff_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_dxdxi_diff_type(dxi)
!
      type(dxdxi_direction_type), intent(inout) :: dxi
!
!
      call dealloc_dxi_diff_type(dxi%dx)
      call dealloc_dxi_diff_type(dxi%dy)
      call dealloc_dxi_diff_type(dxi%dz)
!
      end subroutine dealloc_dxdxi_diff_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_dxidx_diff_type(dx)
!
      type(dxidx_direction_type), intent(inout) :: dx
!
!
      call dealloc_dx_diff_type(dx%dxi)
      call dealloc_dx_diff_type(dx%dei)
      call dealloc_dx_diff_type(dx%dzi)
!
      end subroutine dealloc_dxidx_diff_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_jacobians_node(nnod_filter_mom, filter_dxi)
!
      integer (kind = kint), intent(in) :: nnod_filter_mom
      type(dxdxi_data_type), intent(inout) :: filter_dxi
!
!
      call alloc_dxdxi_diff_type(nnod_filter_mom, filter_dxi%dxi_nod)
!
      end subroutine alloc_jacobians_node
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_jacobians_ele(nele_filter_mom, filter_dxi)
!
      integer (kind = kint), intent(in) :: nele_filter_mom
      type(dxdxi_data_type), intent(inout) :: filter_dxi
!
!
      call alloc_dxdxi_diff_type(nele_filter_mom, filter_dxi%dxi_ele)
!
      end subroutine alloc_jacobians_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_jacobians_node(filter_dxi)
!
      type(dxdxi_data_type), intent(inout) :: filter_dxi
!
!
      call dealloc_dxdxi_diff_type(filter_dxi%dxi_nod)
!
      end subroutine dealloc_jacobians_node
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_jacobians_ele(filter_dxi)
!
      type(dxdxi_data_type), intent(inout) :: filter_dxi
!
!
      call dealloc_dxdxi_diff_type(filter_dxi%dxi_ele)
!
      end subroutine dealloc_jacobians_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_dxidxs_node(nnod_filter_mom, dxidxs)
!
      integer (kind = kint), intent(in) :: nnod_filter_mom
      type(dxidx_data_type), intent(inout) :: dxidxs
!
!
      call alloc_dxidx_diff_type(nnod_filter_mom, dxidxs%dx_nod)
!
      end subroutine alloc_dxidxs_node
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dxidxs_ele(nele_filter_mom, dxidxs)
!
      integer (kind = kint), intent(in) :: nele_filter_mom
      type(dxidx_data_type), intent(inout) :: dxidxs
!
!
      call alloc_dxidx_diff_type(nele_filter_mom, dxidxs%dx_ele)
!
      end subroutine alloc_dxidxs_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_dxidxs_node(dxidxs)
!
      type(dxidx_data_type), intent(inout) :: dxidxs
!
!
      call dealloc_dxidx_diff_type(dxidxs%dx_nod)
!
      end subroutine dealloc_dxidxs_node
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_dxidxs_ele(dxidxs)
!
      type(dxidx_data_type), intent(inout) :: dxidxs
!
!
      call dealloc_dxidx_diff_type(dxidxs%dx_ele)
!
      end subroutine dealloc_dxidxs_ele
!
!  ---------------------------------------------------------------------
!
      end module t_filter_dxdxi
