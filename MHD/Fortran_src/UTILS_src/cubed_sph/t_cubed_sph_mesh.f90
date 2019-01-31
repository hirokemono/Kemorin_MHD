!
!      module t_cubed_sph_mesh
!
!      Written by H. Matsui on Apr., 2006
!
!!       subroutine alloc_coarse_mesh_stack(max_coarse_level, csph_mesh)
!!       subroutine dealloc_coarse_mesh_stack(csph_mesh)
!!         type(cubed_sph_mesh), intent(inout) :: csph_mesh
!
      module t_cubed_sph_mesh
!
      use m_precision
!
      implicit none
!
      type cubed_sph_mesh
!   num. of node, element
        integer(kind = kint) :: nnod_cb_sph
        integer(kind = kint) :: nele_cb_sph
        integer(kind = kint) :: nsurf_cb_sph
        integer(kind = kint) :: nedge_cb_sph
!
        integer(kind = kint) :: numnod_20
        integer(kind = kint) :: numele_20
!
!   position
        real(kind = kreal) :: xyz(3)
!
!   connectivity
        integer(kind = kint) :: ie(8)
        integer(kind = kint) :: isurf(4)
        integer(kind = kint) :: iedge(2)
!
        integer(kind = kint) :: ie20(20)
        integer(kind = kint) :: isurf8(8)
        integer(kind = kint) :: iedge3(3)
!
        integer(kind = kint), allocatable :: inod_stack_csph(:)
        integer(kind = kint), allocatable :: iele_stack_csph(:)
        integer(kind = kint), allocatable :: isurf_stack_csph(:)
        integer(kind = kint), allocatable :: iedge_stack_csph(:)
      end type cubed_sph_mesh
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
       subroutine alloc_coarse_mesh_stack(max_coarse_level, csph_mesh)
!
       integer(kind = kint), intent(in) :: max_coarse_level
       type(cubed_sph_mesh), intent(inout) :: csph_mesh
!
       allocate( csph_mesh%inod_stack_csph(0:max_coarse_level) )
       allocate( csph_mesh%iele_stack_csph(0:max_coarse_level) )
       allocate( csph_mesh%iedge_stack_csph(0:max_coarse_level) )
       allocate( csph_mesh%isurf_stack_csph(0:max_coarse_level) )
!
       csph_mesh%inod_stack_csph = 0
       csph_mesh%iele_stack_csph = 0
       csph_mesh%iedge_stack_csph = 0
       csph_mesh%isurf_stack_csph = 0
!
       end subroutine alloc_coarse_mesh_stack
!
!   --------------------------------------------------------------------
!
       subroutine dealloc_coarse_mesh_stack(csph_mesh)
!
       type(cubed_sph_mesh), intent(inout) :: csph_mesh
!
       deallocate( csph_mesh%inod_stack_csph  )
       deallocate( csph_mesh%iele_stack_csph  )
       deallocate( csph_mesh%iedge_stack_csph )
       deallocate( csph_mesh%isurf_stack_csph )
!
       end subroutine dealloc_coarse_mesh_stack
!
!   --------------------------------------------------------------------
!
      end module t_cubed_sph_mesh
