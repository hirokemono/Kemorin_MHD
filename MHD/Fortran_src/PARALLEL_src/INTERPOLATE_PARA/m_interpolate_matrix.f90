!m_interpolate_matrix.f90
!     module m_interpolate_matrix
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine deallocate_itp_mat_item
!!      subroutine const_interporate_mat(nele_org, nnod_ele_org, ie_org)
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
      allocate(INOD_itp_mat(NC_itp) )
      allocate(INM_itp(0:NC_itp) )
      allocate(NUM_SUM_itp(NUM_NCOMP_itp) )
      allocate(IEND_SUM_itp(0:NUM_NCOMP_itp) )
      allocate(IEND_SUM_itp_smp(0:np_smp*NUM_NCOMP_itp) )
!
      if ( NC_itp .gt. 0) INOD_itp_mat = 0
      INM_itp =      0
      IEND_SUM_itp = 0
      IEND_SUM_itp_smp = 0
      NUM_SUM_itp = 0
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
      deallocate(INOD_itp_mat, INM_itp, NUM_SUM_itp)
      deallocate(IEND_SUM_itp, IEND_SUM_itp_smp)
!
      end subroutine deallocate_itp_mat_item
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_interporate_mat(nele_org, nnod_ele_org, ie_org)
!
      use calypso_mpi
      use m_machine_parameter
      use m_interpolate_table_orgin
      use interpolate_matrix_para
!
      integer(kind = kint), intent(in) :: nele_org, nnod_ele_org
      integer(kind = kint), intent(in) :: ie_org(nele_org,nnod_ele_org)
!
!
      NC_itp = itp1_org%ntot_table_org
      NUM_NCOMP_itp = 4
!
      call allocate_itp_mat_num
!
      call count_interporate_mat_para                                   &
     &   (np_smp, nnod_ele_org, itp1_org%istack_tbl_type_org_smp,       &
     &    NC_itp, NUM_NCOMP_itp, NCM_itp, INOD_itp_mat, INM_itp,        &
     &    NUM_SUM_itp, IEND_SUM_itp, IEND_SUM_itp_smp)
!
      call allocate_itp_mat_item
!
      call set_interporate_mat_para(np_smp, nele_org,                   &
     &    nnod_ele_org, ie_org, itp1_org%iele_org_4_org,                &
     &    itp1_org%itype_inter_org, itp1_org%coef_inter_org,            &
     &    NC_itp, NCM_itp, INM_itp, IAM_itp, AM_itp, IEND_SUM_itp_smp)
!
      end subroutine const_interporate_mat
!
! ----------------------------------------------------------------------
!
      end module m_interpolate_matrix
