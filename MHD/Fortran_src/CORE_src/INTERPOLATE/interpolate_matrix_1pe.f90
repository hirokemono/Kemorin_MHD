!
!     module interpolate_matrix_1pe
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine count_interpolate_mat_1pe(np_smp, istack_wtype_smp,  &
!!     &      NC, NUM_NCOMP, NCM, NUM_SUM, INOD_DJO, INM,               &
!!     &      IEND_SUM, IEND_SUM_smp)
!!      subroutine set_interpolate_mat_1pe_8(np_smp, numele, ie,        &
!!     &          iele_gauss, itype_gauss, xi_gauss, NC, NCM,           &
!!     &          INM, IAM, AM, IEND_SUM_smp)
!!      subroutine set_interpolate_mat_1pe_20(np_smp, numele, ie,       &
!!     &          iele_gauss, itype_gauss, xi_gauss, NC, NCM,           &
!!     &          INM, IAM, AM, IEND_SUM_smp)
!!      subroutine set_interpolate_mat_1pe_27(np_smp, numele, ie,       &
!!     &          iele_gauss, itype_gauss, xi_gauss, NC, NCM,           &
!!     &          INM, IAM, AM, IEND_SUM_smp)
!
      module interpolate_matrix_1pe
!
      use m_precision
!
      use m_geometry_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_interpolate_mat_1pe(np_smp, istack_wtype_smp,   &
     &      NC, NUM_NCOMP, NCM, NUM_SUM, INOD_DJO, INM,               &
     &      IEND_SUM, IEND_SUM_smp)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
!
      integer(kind = kint), intent(in) :: NC, NUM_NCOMP
      integer(kind = kint), intent(in) :: NUM_SUM(4)
!
      integer(kind = kint), intent(inout) :: INOD_DJO(NC)
      integer(kind = kint), intent(inout) :: INM(0:NC)
      integer(kind = kint), intent(inout) :: IEND_SUM(0:4), NCM
      integer(kind = kint), intent(inout) :: IEND_SUM_smp(0:4*np_smp)
!
      integer(kind = kint) :: i, ist, num, inum
!
!
      IEND_SUM_smp(0:NUM_NCOMP*np_smp)                                  &
     &            = istack_wtype_smp(0:NUM_NCOMP*np_smp)
      do i = 1, NUM_NCOMP
        IEND_SUM(i) = istack_wtype_smp(i*np_smp)
!
        ist = IEND_SUM(i-1)
        num = IEND_SUM(i  ) - IEND_SUM(i-1)
!$omp parallel do
        do inum = 1, num
          INOD_DJO(inum+ist) = inum + ist
          INM(inum+ist) = INM(ist-1) + inum * NUM_SUM(i)
        end do
!$omp end parallel do
      end do
      NCM = INM(NC)
!
      end subroutine count_interpolate_mat_1pe
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_interpolate_mat_1pe_8(np_smp, numele, ie,          &
     &          iele_gauss, itype_gauss, xi_gauss, NC, NCM,             &
     &          INM, IAM, AM, IEND_SUM_smp)
!
      use interpolate_matrix_ele8
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      integer (kind = kint), intent(in) :: itype_gauss(NC)
      real (kind=kreal), intent(in) :: xi_gauss(NC,3)
!
      integer (kind = kint), intent(in) :: NC, NCM
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:4*np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call set_interpolate_mat_node(np_smp, numele, num_t_linear,       &
     &    ie, iele_gauss, itype_gauss, NC, NCM, INM, IAM, AM,           &
     &    IEND_SUM_smp(ist) )
!
      ist = np_smp
      call set_interpolate_mat_edge2(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist) )
!
      ist = 2*np_smp
      call set_interpolate_mat_surf4(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist) )
!
      ist = 3*np_smp
      call set_interpolate_mat_ele8(np_smp, numele, ie,                 &
     &    iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,                  &
     &    IEND_SUM_smp(ist) )
!
      end subroutine set_interpolate_mat_1pe_8
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_1pe_20(np_smp, numele, ie,         &
     &          iele_gauss, itype_gauss, xi_gauss, NC, NCM,             &
     &          INM, IAM, AM, IEND_SUM_smp)
!
      use interpolate_matrix_ele8
      use interpolate_matrix_ele20
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_quad)
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      integer (kind = kint), intent(in) :: itype_gauss(NC)
      real (kind=kreal), intent(in) :: xi_gauss(NC,3)
!
      integer (kind = kint), intent(in) :: NC, NCM
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:4*np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call set_interpolate_mat_node(np_smp, numele, num_t_quad,         &
     &    ie, iele_gauss, itype_gauss, NC, NCM, INM, IAM, AM,           &
     &    IEND_SUM_smp(ist) )
!
      ist = np_smp
      call set_interpolate_mat_edge3(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist) )
!
      ist = 2*np_smp
      call set_interpolate_mat_surf8(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist) )
!
      ist = 3*np_smp
      call set_interpolate_mat_ele20(np_smp, numele, ie,                &
     &    iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,                  &
     &    IEND_SUM_smp(ist) )
!
      end subroutine set_interpolate_mat_1pe_20
!
! ----------------------------------------------------------------------
!
      subroutine set_interpolate_mat_1pe_27(np_smp, numele, ie,         &
     &          iele_gauss, itype_gauss, xi_gauss, NC, NCM,             &
     &          INM, IAM, AM, IEND_SUM_smp)
!
      use interpolate_matrix_ele8
      use interpolate_matrix_ele20
      use interpolate_matrix_ele27
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numele
      integer (kind = kint), intent(in) :: ie(numele,num_t_lag)
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      integer (kind = kint), intent(in) :: itype_gauss(NC)
      real (kind=kreal), intent(in) :: xi_gauss(NC,3)
!
      integer (kind = kint), intent(in) :: NC, NCM
      integer(kind=kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:4*np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      call set_interpolate_mat_node(np_smp, numele, num_t_lag,          &
     &    ie, iele_gauss, itype_gauss, NC, NCM, INM, IAM, AM,           &
     &    IEND_SUM_smp(ist) )
!
      ist = np_smp
      call set_interpolate_mat_edge3(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist) )
!
      ist = 2*np_smp
      call set_interpolate_mat_surf9(np_smp, numele, ie,                &
     &    iele_gauss, itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM,     &
     &    IEND_SUM_smp(ist) )
!
      ist = 3*np_smp
      call set_interpolate_mat_ele27(np_smp, numele, ie,                &
     &    iele_gauss, xi_gauss, NC, NCM, INM, IAM, AM,                  &
     &    IEND_SUM_smp(ist) )
!
      end subroutine set_interpolate_mat_1pe_27
!
! ----------------------------------------------------------------------
!
      end module interpolate_matrix_1pe
