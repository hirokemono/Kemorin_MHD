!t_jacobian_1d.f90
!     module t_jacobian_1d
!
!      Written by H. Matsui on Dec., 2008
!
!>   Structure of 1D Jacobian and difference of shape functions
!
!
!      subroutine alloc_1d_jac_type(numedge, nnod_4_edge, jac_1d)
!        integer(kind = kint), intent(in) :: numedge, nnod_4_edge
!        type(jacobians_1d), intent(inout) :: jac_1d
!
!      subroutine dealloc_1d_jac_type(jac_1d)
!
      module t_jacobian_1d
!
      use m_precision
!
      implicit  none
!
!>     Stracture for Jacobians for edge
      type jacobians_1d
        integer(kind = kint) :: ntot_int
!
        real (kind=kreal), pointer :: an_edge(:,:)
! 
        real (kind=kreal), pointer :: xeg_edge(:,:,:)
!
        real (kind=kreal), pointer :: xj_edge(:,:)
        real (kind=kreal), pointer :: axj_edge(:,:)
      end type jacobians_1d
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_1d_jac_type(numedge, nnod_4_edge, jac_1d)
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
!
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      allocate(jac_1d%an_edge(nnod_4_edge,jac_1d%ntot_int))
!
      allocate(jac_1d%xeg_edge(numedge,jac_1d%ntot_int,3))
!
      allocate(jac_1d%xj_edge(numedge,jac_1d%ntot_int))
      allocate(jac_1d%axj_edge(numedge,jac_1d%ntot_int))
!
      jac_1d%an_edge = 0.0d0
!
      if (numedge .gt. 0) then
        jac_1d%xeg_edge = 0.0d0
!
        jac_1d%xj_edge = 0.0d0
        jac_1d%axj_edge = 0.0d0
      end if
!
      end subroutine alloc_1d_jac_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_1d_jac_type(jac_1d)
!
      type(jacobians_1d), intent(inout) :: jac_1d
!
!
      deallocate(jac_1d%an_edge)
      deallocate(jac_1d%xeg_edge)
!
      deallocate(jac_1d%xj_edge, jac_1d%axj_edge)
!
      end subroutine dealloc_1d_jac_type
!
!  ---------------------------------------------------------------------
!
      end module t_jacobian_1d
