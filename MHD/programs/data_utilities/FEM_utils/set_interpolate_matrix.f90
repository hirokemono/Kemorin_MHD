!
!     module set_interpolate_matrix
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine count_interpolate_matrix_8(np_smp, istack_wtype_smp,  &
!     &          num_points, NC, NCM, INOD_DJO, INM, NUM_SUM, NUM_NCOMP,&
!     &          IEND_SUM, IEND_SUM_smp)
!      subroutine count_interpolate_matrix_20(np_smp, istack_wtype_smp, &
!     &          num_points, NC, NCM, INOD_DJO, INM, NUM_SUM, NUM_NCOMP,&
!     &          IEND_SUM, IEND_SUM_smp)
!      subroutine count_interpolate_matrix_27(np_smp, istack_wtype_smp, &
!     &          num_points, NC, NCM, INOD_DJO, INM, NUM_SUM, NUM_NCOMP,&
!     &          IEND_SUM, IEND_SUM_smp)
!
!      subroutine set_interpolate_matrix_8(np_smp, numele, ie,          &
!     &          istack_wtype_smp, num_points, iele_gauss, itype_gauss, &
!     &          xi_gauss, NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!      subroutine set_interpolate_matrix_20(np_smp, numele, ie,         &
!     &          istack_wtype_smp, num_points, iele_gauss, itype_gauss, &
!     &          xi_gauss, NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!      subroutine set_interpolate_matrix_27(np_smp, numele, ie,         &
!     &          istack_wtype_smp, num_points, iele_gauss, itype_gauss, &
!     &          xi_gauss, NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      module set_interpolate_matrix
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_interpolate_matrix_8(np_smp, istack_wtype_smp,   &
     &          num_points, NC, NCM, INOD_DJO, INM, NUM_SUM, NUM_NCOMP, &
     &          IEND_SUM, IEND_SUM_smp)
!
      use interpolate_matrix_node
      use interpolate_matrix_edge2
      use interpolate_matrix_surf4
      use interpolate_matrix_ele28
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
!
      integer(kind = kint), intent(in) :: NC, NUM_NCOMP
!
      integer(kind = kint), intent(inout) :: NCM
      integer(kind = kint), intent(inout) :: INOD_DJO(NC)
      integer(kind = kint), intent(inout) :: INM(NC)
      integer(kind = kint), intent(inout) :: NUM_SUM(NUM_NCOMP)
      integer(kind = kint), intent(inout) :: IEND_SUM(NUM_NCOMP)
      integer(kind = kint), intent(inout)                               &
     &                     :: IEND_SUM_smp(0:np_smp*NUM_NCOMP)
!
      integer(kind = kint) :: ist
!
!
      NC = num_points
      NUM_NCOMP = 4
!
      ist = 0
      call count_interpolate_mat_node(np_smp, istack_wtype_smp(ist),    &
     &    NC, INOD_DJO, INM, NUM_SUM(1), IEND_SUM(1),                   &
     &    IEND_SUM_smp(ist))
!
      ist = np_smp
      call count_interpolate_mat_edge2(np_smp, istack_wtype_smp(ist),   &
     &    NC, INOD_DJO, INM, NUM_SUM(2), IEND_SUM(2),                   &
     &    IEND_SUM_smp(ist))
!
      ist = 2*np_smp
      call count_interpolate_mat_surf4(np_smp, istack_wtype_smp(ist),   &
     &    NC, INOD_DJO, INM, NUM_SUM(3), IEND_SUM(3),                   &
     &    IEND_SUM_smp(ist))
!
      ist = 3*np_smp
      call count_interpolate_mat_ele8(np_smp, istack_wtype_smp(ist),    &
     &    NC, INOD_DJO, INM, NUM_SUM(4), IEND_SUM(4),                   &
     &    IEND_SUM_smp(ist))
!
      NCM = INM(NC)
!
      end subroutine count_interpolate_matrix_8
!
! ----------------------------------------------------------------------
!
      subroutine count_interpolate_matrix_20(np_smp, istack_wtype_smp,  &
     &          num_points, NC, NCM, INOD_DJO, INM, NUM_SUM, NUM_NCOMP, &
     &          IEND_SUM, IEND_SUM_smp)
!
      use interpolate_matrix_node
      use interpolate_matrix_edge3
      use interpolate_matrix_surf8
      use interpolate_matrix_ele20
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
!
      integer(kind = kint), intent(in) :: NC, NUM_NCOMP
!
      integer(kind = kint), intent(inout) :: NCM
      integer(kind = kint), intent(inout) :: INOD_DJO(NC)
      integer(kind = kint), intent(inout) :: INM(NC)
      integer(kind = kint), intent(inout) :: NUM_SUM(NUM_NCOMP)
      integer(kind = kint), intent(inout) :: IEND_SUM(NUM_NCOMP)
      integer(kind = kint), intent(inout)                               &
     &                     :: IEND_SUM_smp(0:np_smp*NUM_NCOMP)
!
      integer(kind = kint) :: ist
!
!
      NC = num_points
      NUM_NCOMP = 4
!
      ist = 0
      call count_interpolate_mat_node(np_smp, istack_wtype_smp(ist),    &
     &    NC, INOD_DJO, INM, NUM_SUM(1), IEND_SUM(1),                   &
     &    IEND_SUM_smp(ist))
!
      ist = np_smp
      call count_interpolate_mat_edge3(np_smp, istack_wtype_smp(ist),   &
     &    NC, INOD_DJO, INM, NUM_SUM(2), IEND_SUM(2),                   &
     &    IEND_SUM_smp(ist))
!
      ist = 2*np_smp
      call count_interpolate_mat_surf8(np_smp, istack_wtype_smp(ist),   &
     &    NC, INOD_DJO, INM, NUM_SUM(3), IEND_SUM(3),                   &
     &    IEND_SUM_smp(ist))
!
      ist = 3*np_smp
      call count_interpolate_mat_ele20(np_smp, istack_wtype_smp(ist),   &
     &    NC, INOD_DJO, INM, NUM_SUM(4), IEND_SUM(4),                   &
     &    IEND_SUM_smp(ist))
!
      NCM = INM(NC)
!
      end subroutine count_interpolate_matrix_20
!
!  ---------------------------------------------------------------------
!
      subroutine count_interpolate_matrix_27(np_smp, istack_wtype_smp,  &
     &          num_points, NC, NCM, INOD_DJO, INM, NUM_SUM, NUM_NCOMP, &
     &          IEND_SUM, IEND_SUM_smp)
!
      use interpolate_matrix_node
      use interpolate_matrix_edge3
      use interpolate_matrix_surf9
      use interpolate_matrix_ele27
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer(kind = kint), intent(in) :: NC, NUM_NCOMP
!
      integer(kind = kint), intent(inout) :: NCM
      integer(kind = kint), intent(inout) :: INOD_DJO(NC)
      integer(kind = kint), intent(inout) :: INM(NC)
      integer(kind = kint), intent(inout) :: NUM_SUM(NUM_NCOMP)
      integer(kind = kint), intent(inout) :: IEND_SUM(NUM_NCOMP)
      integer(kind = kint), intent(inout)                               &
     &                     :: IEND_SUM_smp(0:np_smp*NUM_NCOMP)
!
      integer(kind = kint) :: ist
!
!
      NC = num_points
      NUM_NCOMP = 4
!
      ist = 0
      call count_interpolate_mat_node(np_smp, istack_wtype_smp(ist),    &
     &    NC, INOD_DJO, INM, NUM_SUM(1), IEND_SUM(1),                   &
     &    IEND_SUM_smp(ist))
!
      ist = np_smp
      call count_interpolate_mat_edge3(np_smp, istack_wtype_smp(ist),   &
     &    NC, INOD_DJO, INM, NUM_SUM(2), IEND_SUM(2),                   &
     &    IEND_SUM_smp(ist))
!
      ist = 2*np_smp
      call count_interpolate_mat_surf9(np_smp, istack_wtype_smp(ist),   &
     &    NC, INOD_DJO, INM, NUM_SUM(3), IEND_SUM(3),                   &
     &    IEND_SUM_smp(ist))
!
      ist = 3*np_smp
      call count_interpolate_mat_ele27(np_smp, istack_wtype_smp(ist),   &
     &    NC, INOD_DJO, INM, NUM_SUM(4), IEND_SUM(4),                   &
     &    IEND_SUM_smp(ist))
!
      NCM = INM(NC)
!
      end subroutine count_interpolate_matrix_27
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_interpolate_matrix_8(np_smp, numele, ie,           &
     &          istack_wtype_smp, num_points, iele_gauss, itype_gauss,  &
     &          xi_gauss, NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      use interpolate_matrix_node
      use interpolate_matrix_edge2
      use interpolate_matrix_surf4
      use interpolate_matrix_ele28
!
      integer(kind = kint), parameter :: nnod_4_ele = 8
      integer (kind = kint), intent(in) :: np_smp, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
!
      integer (kind = kint), intent(in) :: NC, NCM
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call set_interpolate_mat_node(np_smp, numele, nnod_4_ele, ie,     &
     &    iele_gauss, itype_gauss, NC, NCM, INM, IAM, AM,               &
     &    IEND_SUM_smp(ist))
!
      ist = np_smp
      call set_interpolate_mat_edge2(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist))
!
      ist = 2*np_smp
      call set_interpolate_mat_surf4(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist))
!
      ist = 3*np_smp
      call set_interpolate_mat_ele8(np_smp, numele, ie,                 &
     &    iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,                  &
     &    IEND_SUM_smp(ist))
!
      end subroutine set_interpolate_matrix_8
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_matrix_20(np_smp, numele, ie,          &
     &          istack_wtype_smp, num_points, iele_gauss, itype_gauss,  &
     &          xi_gauss, NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      use interpolate_matrix_node
      use interpolate_matrix_edge3
      use interpolate_matrix_surf8
      use interpolate_matrix_ele20
!
      integer(kind = kint), parameter :: nnod_4_ele = 20
      integer (kind = kint), intent(in) :: np_smp, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
!
      integer (kind = kint), intent(in) :: NC, NCM
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call set_interpolate_mat_node(np_smp, numele, nnod_4_ele, ie,     &
     &    iele_gauss, itype_gauss, NC, NCM, INM, IAM, AM,               &
     &    IEND_SUM_smp(ist))
!
      ist = np_smp
      call set_interpolate_mat_edge3(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist))
!
      ist = 2*np_smp
      call set_interpolate_mat_surf8(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist))
!
      ist = 3*np_smp
      call set_interpolate_mat_ele20(np_smp, numele, ie,                &
     &    iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,                  &
     &    IEND_SUM_smp(ist))
!
      end subroutine set_interpolate_matrix_20
!
!  ---------------------------------------------------------------------
!
      subroutine set_interpolate_matrix_27(np_smp, numele, ie,          &
     &          istack_wtype_smp, num_points, iele_gauss, itype_gauss,  &
     &          xi_gauss, NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      use interpolate_matrix_node
      use interpolate_matrix_edge3
      use interpolate_matrix_surf9
      use interpolate_matrix_ele27
!
      integer(kind = kint), parameter :: nnod_4_ele = 27
      integer (kind = kint), intent(in) :: np_smp, numele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
!
      integer (kind = kint), intent(in) :: NC, NCM
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call set_interpolate_mat_node(np_smp, numele, nnod_4_ele, ie,     &
     &    iele_gauss, itype_gauss, NC, NCM, INM, IAM, AM,               &
     &    IEND_SUM_smp(ist))
!
      ist = np_smp
      call set_interpolate_mat_edge3(np_smp, numele, ie(1,1),           &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist))
!
      ist = 2*np_smp
      call set_interpolate_mat_surf9(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist))
!
      ist = 3*np_smp
      call set_interpolate_mat_ele27(np_smp, numele, ie,                &
     &    iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,                  &
     &    IEND_SUM_smp(ist))
!
      end subroutine set_interpolate_matrix_27
!
! ----------------------------------------------------------------------
!
      end module set_interpolate_matrix
