!m_fem_gauss_int_coefs
!   module   m_fem_gauss_int_coefs
!.......................................................................
!
!
!      Written by H. Matsui on Dec. 2003
!      Modified by H. Matsui on Oct., 2006
!
!!      subroutine copy_fem_gauss_int_coefs(g_FEM)
!
      module   m_fem_gauss_int_coefs
!
      use m_precision
      use m_constants
      use t_fem_gauss_int_coefs
!
      implicit  none
!
      integer (kind=kint), save :: max_int_point = 4
      integer (kind=kint), save :: maxtot_int_3d= 100
      integer (kind=kint), save :: maxtot_int_2d= 30
      integer (kind=kint), save :: maxtot_int_1d= 10
!
!
      integer (kind=kint), allocatable :: int_start1(:)
      integer (kind=kint), allocatable :: int_start2(:)
      integer (kind=kint), allocatable :: int_start3(:)
!
      real (kind=kreal), allocatable :: owe(:)
      real (kind=kreal), allocatable :: owe2d(:)
      real (kind=kreal), allocatable :: owe3d(:)
! 
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_fem_gauss_int_coefs(g_FEM)
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!
!
      max_int_point = g_FEM%max_int_point
!
      maxtot_int_3d = g_FEM%maxtot_int_3d
      maxtot_int_2d = g_FEM%maxtot_int_2d
      maxtot_int_1d = g_FEM%maxtot_int_1d
!
      allocate( owe(maxtot_int_1d)   )
      allocate( owe2d(maxtot_int_2d) )
      allocate( owe3d(maxtot_int_3d) )
!
      allocate( int_start1(max_int_point) )
      allocate( int_start2(max_int_point) )
      allocate( int_start3(max_int_point) )
!
      int_start1(1:max_int_point) = g_FEM%int_start1(1:max_int_point)
      int_start2(1:max_int_point) = g_FEM%int_start2(1:max_int_point)
      int_start3(1:max_int_point) = g_FEM%int_start3(1:max_int_point)
!
      owe(1:maxtot_int_1d) =   g_FEM%owe(1:maxtot_int_1d)
      owe2d(1:maxtot_int_2d) = g_FEM%owe2d(1:maxtot_int_2d)
      owe3d(1:maxtot_int_3d) = g_FEM%owe3d(1:maxtot_int_3d)
!
      end subroutine copy_fem_gauss_int_coefs
!
!-----------------------------------------------------------------------
!
!
      end module   m_fem_gauss_int_coefs
