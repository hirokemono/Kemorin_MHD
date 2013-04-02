!m_interpolate_matrix.f90
!     module m_interpolate_matrix
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine deallocate_itp_mat_item
!      subroutine const_interporate_mat(numele, nnod_4_ele, ie)
!
      module m_interpolate_matrix
!
      use m_precision
!
      implicit none
!
!
      integer(kind=kint ) ::  NC_itp
      integer(kind=kint ) ::  NCM_itp
      integer(kind=kint ), allocatable :: INOD_itp_mat(:)
      integer(kind=kint ), allocatable :: INM_itp(:)
      integer(kind=kint ), allocatable :: IAM_itp(:)
!
      integer(kind=kint ) ::  NUM_NCOMP_itp
      integer(kind=kint ), allocatable :: NUM_SUM_itp(:)
      integer(kind=kint ), allocatable :: IEND_SUM_itp(:)
      integer(kind=kint ), allocatable :: IEND_SUM_itp_smp(:)
!
      real   (kind=kreal), allocatable ::  AM_itp(:)
!
      private :: allocate_itp_mat_num, allocate_itp_mat_item
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_mat_num
!
      use m_machine_parameter
!
!
      allocate (INOD_itp_mat(NC_itp) )
      allocate (INM_itp(0:NC_itp) )
      allocate (IEND_SUM_itp(0:NUM_NCOMP_itp) )
      allocate (IEND_SUM_itp_smp(0:np_smp*NUM_NCOMP_itp) )
!
      if ( NC_itp .gt. 0) INOD_itp_mat = 0
      INM_itp =      0
      IEND_SUM_itp = 0
      IEND_SUM_itp_smp = 0
!
      end subroutine allocate_itp_mat_num
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_itp_mat_item
!
!
      allocate(IAM_itp(0:NCM_itp) )
      allocate(AM_itp(0:NCM_itp) )
!
      if(NCM_itp .gt. 0) then
        AM_itp = 0.0d0
        IAM_itp= 0
      end if
!
      end subroutine allocate_itp_mat_item
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_mat_item
!
!
      deallocate(IAM_itp, AM_itp)
!
      deallocate(INOD_itp_mat, INM_itp)
      deallocate(IEND_SUM_itp, IEND_SUM_itp_smp)
!
      end subroutine deallocate_itp_mat_item
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_interporate_mat(numele, nnod_4_ele, ie)
!
      use m_machine_parameter
      use m_interpolate_table_orgin
      use interpolate_matrix_para
!
      integer (kind = kint), intent(in) :: numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
!
      NC_itp =          ntot_table_org
      NUM_NCOMP_itp = 4*num_dest_domain
!
      call allocate_itp_mat_num
!
      call count_interporate_mat_para(np_smp, nnod_4_ele,               &
     &    num_dest_domain, istack_table_wtype_org_smp,                  &
     &    NC_itp, NUM_NCOMP_itp, NCM_itp, INOD_itp_mat, INM_itp,        &
     &    NUM_SUM_itp, IEND_SUM_itp, IEND_SUM_itp_smp)
!
      call allocate_itp_mat_item
!
      call set_interporate_mat_para(np_smp, numele, nnod_4_ele,         &
     &    ie, num_dest_domain, iele_org_4_org, itype_inter_org,         &
     &    coef_inter_org, NC_itp, NCM_itp, NUM_NCOMP_itp,               &
     &    INM_itp, IAM_itp, AM_itp, IEND_SUM_itp_smp)
!
      end subroutine const_interporate_mat
!
! ----------------------------------------------------------------------
!
      end module m_interpolate_matrix
