!
!     module interpolate_matrix_para
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine count_interporate_mat_para(np_smp, nnod_4_ele,       &
!!     &          istack_tbl_wtype_smp, NC, NUM_NCOMP,                  &
!!     &          NCM, INOD_DJO, INM,  NUM_SUM, IEND_SUM, IEND_SUM_smp)
!!
!!      subroutine set_interporate_mat_para(np_smp, numele, nnod_4_ele, &
!!     &          ie, iele_gauss, itype_gauss, xi_gauss,                &
!!     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      module interpolate_matrix_para
!
      use m_precision
      use m_constants
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
      subroutine count_interporate_mat_para(np_smp, nnod_4_ele,         &
     &          istack_tbl_wtype_smp, NC, NUM_NCOMP,                    &
     &          NCM, INOD_DJO, INM,  NUM_SUM, IEND_SUM, IEND_SUM_smp)
!
      use interpolate_matrix_1pe
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: nnod_4_ele
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp)
!
      integer(kind = kint), intent(in) :: NC
      integer(kind = kint), intent(in) :: NUM_NCOMP
!
      integer(kind = kint), intent(inout) :: INOD_DJO(NC)
      integer(kind = kint), intent(inout) :: INM(NC)
      integer(kind = kint), intent(inout) :: NUM_SUM(NUM_NCOMP)
      integer(kind = kint), intent(inout) :: IEND_SUM(0:NUM_NCOMP)
      integer(kind = kint), intent(inout) :: IEND_SUM_smp(0:4*np_smp)
      integer(kind = kint), intent(inout) :: NCM
!
!
      if(nnod_4_ele .eq. num_t_lag) then
        NUM_SUM(1) = ione
        NUM_SUM(2) = num_quad_edge
        NUM_SUM(3) = num_lag_sf
        NUM_SUM(4) = num_t_lag
      else if(nnod_4_ele .eq. num_t_quad) then
        NUM_SUM(1) = ione
        NUM_SUM(2) = num_quad_edge
        NUM_SUM(3) = num_quad_sf
        NUM_SUM(4) = num_t_quad
      else
        NUM_SUM(1) = ione
        NUM_SUM(2) = num_linear_edge
        NUM_SUM(3) = num_linear_sf
        NUM_SUM(4) = num_t_linear
      end if
!
        call count_interpolate_mat_1pe(np_smp, istack_tbl_wtype_smp,    &
     &      NC, NUM_NCOMP, NCM, NUM_SUM, INOD_DJO, INM,                 &
     &      IEND_SUM, IEND_SUM_smp)
!
      end subroutine count_interporate_mat_para
!
! ----------------------------------------------------------------------
!
      subroutine set_interporate_mat_para(np_smp, numele, nnod_4_ele,   &
     &          ie, iele_gauss, itype_gauss, xi_gauss,                  &
     &          NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      use interpolate_matrix_1pe
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind = kint), intent(in) :: iele_gauss(NC)
      integer (kind = kint), intent(in) :: itype_gauss(NC)
      real (kind=kreal), intent(in) :: xi_gauss(NC,3)
!
      integer(kind = kint), intent(in) :: NC, NCM
      integer(kind = kint), intent(in) :: INM(0:NC)
      integer(kind = kint), intent(in) :: IEND_SUM_smp(0:4*np_smp)
!
      integer(kind=kint), intent(inout) :: IAM(NCM)
      real(kind = kreal), intent(inout) :: AM(NCM)
!
!
      if(nnod_4_ele .eq. num_t_lag) then
        call set_interpolate_mat_1pe_27(np_smp, numele, ie, iele_gauss, &
     &      itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      else if(nnod_4_ele .eq. num_t_quad) then
        call set_interpolate_mat_1pe_20(np_smp, numele, ie, iele_gauss, &
     &      itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM, IEND_SUM_smp)
!
      else
        call set_interpolate_mat_1pe_8(np_smp, numele, ie, iele_gauss,  &
     &      itype_gauss, xi_gauss, NC, NCM, INM, IAM, AM, IEND_SUM_smp)
      end if
!
      end subroutine set_interporate_mat_para
!
! ----------------------------------------------------------------------
!
      end module interpolate_matrix_para
