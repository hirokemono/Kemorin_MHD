!>@file   t_surface_data.f90
!!@brief  module t_surface_data
!!
!!@author  H. Matsui
!!@date Programmed in 2008
!
!> @brief structure of surface data (geometry and connectivity)
!!
!!@verbatim
!!      subroutine alloc_numsurf_stack(num_pe, surf)
!!      subroutine allocate_inod_in_surf(surf)
!!      subroutine allocate_surface_connect_type(surf, nele)
!!      subroutine allocate_ext_surface_type(surf)
!!      subroutine allocate_iso_surface_type(surf)
!!      subroutine alloc_surface_geometory(surf)
!!      subroutine allocate_normal_vect_type(surf)
!!      subroutine alloc_surf_param_smp(surf)
!!      subroutine alloc_ele_4_surf_type(surf)
!!
!!      subroutine dealloc_numsurf_stack(surf)
!!      subroutine dealloc_inod_in_surf(surf)
!!      subroutine deallocate_surface_connect_type(surf)
!!      subroutine deallocate_ext_surface_type(surf)
!!      subroutine deallocate_iso_surface_type(surf)
!!      subroutine dealloc_surface_geometory(surf)
!!      subroutine deallocate_normal_vect_type(surf)
!!      subroutine dealloc_surf_param_smp(surf)
!!      subroutine dealloc_ele_4_surf_type(surf)
!!        integer(kind = kint), intent(in) :: nele
!!        type(surface_data), intent(inout) :: surf
!!@endverbatim
!
      module t_surface_data
!
      use m_precision
!
      implicit  none
!
!>      structure of surface data (geometry and connectivity)
      type surface_data
!>       number of surface on local PE
        integer( kind=kint )  ::  numsurf
!>       number of internal surface on local PE
        integer( kind=kint )  ::  internal_surf
!>       number of nodes in each surface
        integer(kind=kint) :: nnod_4_surf
!>       number of external sueface
        integer(kind=kint) ::  numsurf_ext
!>       number of isolated sueface
        integer(kind=kint) ::  numsurf_iso
!
!>        Stack list of number of surface
        integer(kind=kint_gl), allocatable  :: istack_numsurf(:)
!>        Stack list of number of internal surface
        integer(kind=kint_gl), allocatable  :: istack_intersurf(:)
!
!>   local index for surface on each element
        integer (kind=kint), allocatable :: node_on_sf(:,:)
!>   local index for opposite surface on each element
        integer (kind=kint), allocatable :: node_on_sf_n(:,:)
!
!>     smp stack for surface on  local PE
        integer( kind=kint ), allocatable :: istack_surf_smp(:)
!>     maximum number of smp surface on local PE
        integer( kind=kint )  ::  max_surf_smp
!>     maximum number of smp internal surface on local PE
        integer( kind=kint )  ::  max_internal_surf_smp
!
!>       global surface id (where i:surface id)
        integer(kind=kint_gl), allocatable  ::  isurf_global(:)
!
!>   surface connectivity ie_surf(i:surface ID,j:surface index)
        integer(kind=kint), allocatable  :: ie_surf(:,:)
!
!>   surface ID for element surface isf_4_ele(:,:)
!>          ...i:element ID, j:surface ID
!>@n          Positive: normal direction negative: reverse direction
        integer(kind=kint), allocatable  :: isf_4_ele(:,:)
!>   rotation ID for element surface isf_rot_ele(:,:)
!>          ...i:element ID, j:surface ID
!>@n          0: normal direction  1-4: rotation flag for reverse surface
        integer(kind=kint), allocatable  :: isf_rot_ele(:,:)
!
!>     external surface list
        integer(kind=kint), allocatable  ::  isf_external(:)
!>     isolated surface list
        integer(kind=kint), allocatable  ::  isf_isolate(:)
!
!>   belonged element for surface(surface#,face#,
!>                                1:element or 2:local surface)
        integer(kind=kint), allocatable  :: iele_4_surf(:,:,:)
!
!>  integer flag for interior surface 1...interior, 0...exterior
        integer(kind = kint), allocatable :: interior_surf(:)
!
!>       position of center of surface
        real(kind=kreal)  , allocatable  :: x_surf(:,:)
! 
!>       area of each surface
        real (kind=kreal), allocatable :: area_surf(:)
!>       1 / area_surf
        real (kind=kreal), allocatable :: a_area_surf(:)
!
!>       normal vector for sach surface
        real (kind=kreal), allocatable :: vnorm_surf(:,:)
      end type surface_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_numsurf_stack(num_pe, surf)
!
      integer, intent(in) :: num_pe
      type(surface_data), intent(inout) :: surf
!
!
      allocate(surf%istack_numsurf(0:num_pe))
      allocate(surf%istack_intersurf(0:num_pe))
      surf%istack_numsurf =   0
      surf%istack_intersurf = 0
!
      end subroutine alloc_numsurf_stack
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_inod_in_surf(surf)
!
      use m_geometry_constants
!
      type(surface_data), intent(inout) :: surf
!
!
       allocate ( surf%node_on_sf  (surf%nnod_4_surf,nsurf_4_ele) )
       allocate ( surf%node_on_sf_n(surf%nnod_4_surf,nsurf_4_ele) )
!
       surf%node_on_sf =   0
       surf%node_on_sf_n = 0
!
       end subroutine allocate_inod_in_surf
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surface_connect_type(surf, nele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%isf_4_ele(nele,nsurf_4_ele) )
      allocate( surf%isf_rot_ele(nele,nsurf_4_ele) )
      allocate( surf%ie_surf(surf%numsurf,surf%nnod_4_surf) )
      allocate( surf%isurf_global(surf%numsurf) )
      allocate( surf%interior_surf(surf%numsurf) )
!
      if (surf%numsurf.gt. 0) then
        surf%isf_4_ele =     0
        surf%isf_rot_ele =  -1
        surf%ie_surf =       0
        surf%isurf_global =  0
        surf%interior_surf = 0
      end if
!
      end subroutine allocate_surface_connect_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ext_surface_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%isf_external(surf%numsurf_ext) )
      if (surf%numsurf_ext .gt. 0) surf%isf_external = 0
!
      end subroutine allocate_ext_surface_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_iso_surface_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%isf_isolate(surf%numsurf_iso) )
      if (surf%numsurf_iso .gt. 0) surf%isf_isolate = 0
!
      end subroutine allocate_iso_surface_type
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_surface_geometory(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%x_surf(surf%numsurf,3) )
!
      if(surf%numsurf .gt. 0) then
!$omp parallel workshare
        surf%x_surf(1:surf%numsurf,1:3) = 0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_surface_geometory
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_normal_vect_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%area_surf(surf%numsurf) )
      allocate( surf%a_area_surf(surf%numsurf) )
      allocate( surf%vnorm_surf(surf%numsurf,3) )
!
      if ( surf%numsurf .gt. 0 ) then
!$omp parallel workshare
        surf%area_surf(1:surf%numsurf) =     0.0d0
        surf%a_area_surf(1:surf%numsurf) =   0.0d0
        surf%vnorm_surf(1:surf%numsurf,1) =  0.0d0
        surf%vnorm_surf(1:surf%numsurf,2) =  0.0d0
        surf%vnorm_surf(1:surf%numsurf,3) =  0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine allocate_normal_vect_type
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_param_smp(surf)
!
      use m_machine_parameter
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%istack_surf_smp(0:np_smp))
      surf%istack_surf_smp = 0
!
      end subroutine alloc_surf_param_smp
!
!-----------------------------------------------------------------------
!
      subroutine alloc_ele_4_surf_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
!
      allocate(surf%iele_4_surf(surf%numsurf,2,2) )
      if ( surf%numsurf .gt. 0 ) surf%iele_4_surf = 0
!
      end subroutine alloc_ele_4_surf_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_numsurf_stack(surf)
!
      type(surface_data), intent(inout) :: surf
!
!
      if(allocated(surf%istack_numsurf) .eqv. .FALSE.) return
      deallocate (surf%istack_numsurf, surf%istack_intersurf)
!
      end subroutine dealloc_numsurf_stack
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_inod_in_surf(surf)
!
      type(surface_data), intent(inout) :: surf
!
!
      if(allocated(surf%node_on_sf) .eqv. .FALSE.) return
      deallocate (surf%node_on_sf, surf%node_on_sf_n)
!
      end subroutine dealloc_inod_in_surf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_surface_connect_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%isf_4_ele) .eqv. .FALSE.) return
      deallocate( surf%isf_4_ele     )
      deallocate( surf%isf_rot_ele   )
      deallocate( surf%ie_surf       )
      deallocate( surf%isurf_global  )
      deallocate( surf%interior_surf )
!
      end subroutine deallocate_surface_connect_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ext_surface_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%isf_external) .eqv. .FALSE.) return
      deallocate( surf%isf_external )
!
      end subroutine deallocate_ext_surface_type
!
! ------------------------------------------------------
!
      subroutine deallocate_iso_surface_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%isf_isolate) .eqv. .FALSE.) return
      deallocate( surf%isf_isolate )
!
      end subroutine deallocate_iso_surface_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_surface_geometory(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%x_surf) .eqv. .FALSE.) return
      deallocate( surf%x_surf )
!
      end subroutine dealloc_surface_geometory
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_normal_vect_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%area_surf) .eqv. .FALSE.) return
      deallocate( surf%area_surf )
      deallocate( surf%a_area_surf )
      deallocate( surf%vnorm_surf )
!
      end subroutine deallocate_normal_vect_type
!
! ------------------------------------------------------
!
      subroutine dealloc_surf_param_smp(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%istack_surf_smp) .eqv. .FALSE.) return
      deallocate( surf%istack_surf_smp )
!
      end subroutine dealloc_surf_param_smp
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_4_surf_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
!
      if(allocated(surf%iele_4_surf) .eqv. .FALSE.) return
      deallocate(surf%iele_4_surf)
!
      end subroutine dealloc_ele_4_surf_type
!
!-----------------------------------------------------------------------
!
      end module t_surface_data
