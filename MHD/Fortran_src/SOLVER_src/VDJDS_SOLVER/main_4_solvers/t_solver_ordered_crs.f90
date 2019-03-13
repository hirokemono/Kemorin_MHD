!t_solver_ordered_crs.f90
!     module t_solver_ordered_crs
!
!      Written by H. Matsui on Apr., 2012
!
!!      subroutine alloc_crs_smp_num(np_smp, crs_smp)
!!      subroutine alloc_zero_crs_smp_mat(np_smp, crs_smp)
!!      subroutine alloc_crs_smp_mat(crs_smp)
!!      subroutine dealloc_crs_smp_mat(crs_smp)
!!
!!      subroutine alloc_type_djo_num(np_smp, djo_tbl)
!!      subroutine alloc_type_djo_table(djo_tbl)
!!      subroutine alooc_djo_zero_connect_type(np_smp, djo_tbl)
!!
!!      subroutine dealloc_type_connect_4_djo(djo_tbl)
!!      subroutine dealloc_type_djo_mat(mat)
!!
      module t_solver_ordered_crs
!
      use m_precision
!
      use t_comm_table
!
      implicit none
!
      type CRS_SMP_CONNECT_MATRIX
        integer(kind=kint ) ::  NC
        integer(kind=kint ) ::  NCM
        integer(kind=kint ), allocatable :: INOD_DJO(:)
        integer(kind=kint ), allocatable :: INM(:)
        integer(kind=kint ), allocatable :: IAM(:)
!
        integer(kind=kint ) ::  NUM_NCOMP
        integer(kind=kint ), allocatable :: NUM_SUM(:)
        integer(kind=kint ), allocatable :: IEND_SUM(:)
        integer(kind=kint ), allocatable :: IEND_SUM_smp(:)
!
        real   (kind=kreal), allocatable ::  AM(:)
      end type CRS_SMP_CONNECT_MATRIX
!
      type DJORS_CONNECT
        integer(kind=kint ) ::  NC
        integer(kind=kint ) ::  NCM
        integer(kind=kint ), allocatable :: INOD_DJO(:)
        integer(kind=kint ), allocatable :: INM(:)
        integer(kind=kint ), allocatable :: IAM(:)
!
        integer(kind=kint ) ::  NUM_NCOMP
        integer(kind=kint ), allocatable :: NUM_SUM(:)
        integer(kind=kint ), allocatable :: IEND_SUM(:)
        integer(kind=kint ), allocatable :: IEND_SUM_smp(:)
      end type DJORS_CONNECT
!
      type DJORS_MATRIX
        integer(kind=kint ) ::  num_non0
        real   (kind=kreal), allocatable ::  aiccg(:)
        real   (kind=kreal), allocatable ::  AM(:)
      end type DJORS_MATRIX
!
!
      type DJORS_SOLVER_ARRAYS
        type(DJORS_CONNECT) ::       djo_tbl
        type(DJORS_MATRIX) ::        djo_mat
        type(communication_table) :: djo_comm
      end type DJORS_SOLVER_ARRAYS
!
      integer(kind = kint), parameter, private :: izero = 0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_crs_smp_num(np_smp, crs_smp)
!
      integer(kind = kint), intent(in) :: np_smp
      type(CRS_SMP_CONNECT_MATRIX), intent(inout) :: crs_smp
!
!
      allocate (crs_smp%INOD_DJO(crs_smp%NC) )
      allocate (crs_smp%INM(0:crs_smp%NC) )
      allocate (crs_smp%NUM_SUM(crs_smp%NUM_NCOMP) )
      allocate (crs_smp%IEND_SUM(0:crs_smp%NUM_NCOMP) )
      allocate (crs_smp%IEND_SUM_smp(0:np_smp*crs_smp%NUM_NCOMP) )
!
      if ( crs_smp%NC .gt. 0) crs_smp%INOD_DJO = 0
      if ( crs_smp%NUM_NCOMP .gt. 0) crs_smp%NUM_SUM = 0
      crs_smp%INM =      0
      crs_smp%IEND_SUM = 0
      crs_smp%IEND_SUM_smp = 0
!
      end subroutine alloc_crs_smp_num
!
!-----------------------------------------------------------------------
!
      subroutine alloc_crs_smp_mat(crs_smp)
!
      type(CRS_SMP_CONNECT_MATRIX), intent(inout) :: crs_smp
!
!
      allocate (crs_smp%IAM(crs_smp%NCM) )
      allocate (crs_smp%AM(crs_smp%NCM) )
!
      if ( crs_smp%NCM .gt. 0) then
        crs_smp%IAM = 0
        crs_smp%AM = 0.0d0
      end if
!
      end subroutine alloc_crs_smp_mat
!
!-----------------------------------------------------------------------
!
      subroutine alloc_zero_crs_smp_mat(np_smp, crs_smp)
!
      integer(kind = kint), intent(in) :: np_smp
      type(CRS_SMP_CONNECT_MATRIX), intent(inout) :: crs_smp
!
!
      crs_smp%NC = 0
      crs_smp%NUM_NCOMP = 0
      call alloc_crs_smp_num(np_smp, crs_smp)
!
      crs_smp%NCM = 0
      call alloc_crs_smp_mat(crs_smp)
!
      end subroutine alloc_zero_crs_smp_mat
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_crs_smp_mat(crs_smp)
!
      type(CRS_SMP_CONNECT_MATRIX), intent(inout) :: crs_smp
!
!
      deallocate(crs_smp%IAM, crs_smp%AM, crs_smp%INM)
      deallocate(crs_smp%INOD_DJO)
      deallocate(crs_smp%NUM_SUM, crs_smp%IEND_SUM)
      deallocate(crs_smp%IEND_SUM_smp)
!
      end subroutine dealloc_crs_smp_mat
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_type_djo_num(np_smp, djo_tbl)
!
      integer(kind = kint), intent(in) :: np_smp
      type(DJORS_CONNECT), intent(inout) :: djo_tbl
!
!
      allocate (djo_tbl%INOD_DJO(djo_tbl%NC) )
      allocate (djo_tbl%INM(0:djo_tbl%NC) )
      allocate (djo_tbl%NUM_SUM(djo_tbl%NUM_NCOMP) )
      allocate (djo_tbl%IEND_SUM(0:djo_tbl%NUM_NCOMP) )
      allocate (djo_tbl%IEND_SUM_smp(0:np_smp*djo_tbl%NUM_NCOMP) )
!
      if ( djo_tbl%NC .gt. 0) djo_tbl%INOD_DJO = 0
      djo_tbl%INM =      0
      djo_tbl%IEND_SUM = 0
      djo_tbl%IEND_SUM_smp = 0
      djo_tbl%NUM_SUM =      0
!
      end subroutine alloc_type_djo_num
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_type_djo_table(djo_tbl)
!
!
      type(DJORS_CONNECT), intent(inout) :: djo_tbl
!
      allocate (djo_tbl%IAM(0:djo_tbl%NCM) )
      if ( djo_tbl%NCM .gt. 0) djo_tbl%IAM= 0
!
      end subroutine alloc_type_djo_table
!
!  ---------------------------------------------------------------------
!
      subroutine alooc_djo_zero_connect_type(np_smp, djo_tbl)
!
      integer(kind = kint), intent(in) :: np_smp
      type(DJORS_CONNECT), intent(inout) :: djo_tbl
!
!
      djo_tbl%NC =       izero
      djo_tbl%NCM =      izero
      djo_tbl%NUM_NCOMP = izero
!
      call alloc_type_djo_num(np_smp, djo_tbl)
      call alloc_type_djo_table(djo_tbl)
!
      end subroutine alooc_djo_zero_connect_type
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_type_connect_4_djo(djo_tbl)
!
      type(DJORS_CONNECT), intent(inout) :: djo_tbl
!
      deallocate (djo_tbl%INOD_DJO)

      deallocate (djo_tbl%INM)
      deallocate (djo_tbl%IAM)
!
      deallocate (djo_tbl%IEND_SUM, djo_tbl%IEND_SUM_smp)
      deallocate (djo_tbl%NUM_SUM)
!
      end subroutine dealloc_type_connect_4_djo
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_type_djo_mat(mat)
!
      type(DJORS_MATRIX), intent(inout) :: mat
!
!
      deallocate(mat%aiccg)
!
      end subroutine dealloc_type_djo_mat
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine check_type_DJO_table_info(id_rank, np_smp, djo_tbl)
!
       integer, intent(in) :: id_rank
       integer(kind = kint), intent(in) :: np_smp
       type(DJORS_CONNECT), intent(in) :: djo_tbl
!
       integer(kind = kint) :: i
!
      write(50+id_rank,'(a,i15)')                                       &
     &                      'inod, INOD_DJO, INOD_DJO', djo_tbl%NC
      do i = 1, djo_tbl%NC
        write(50+id_rank,'(10i16)')                                     &
     &                      i, djo_tbl%INOD_DJO(i), djo_tbl%INM(i)
      end do
!
      write(50+id_rank,'(a,i15)') 'NUM_NCOMP', djo_tbl%NUM_NCOMP
      write(50+id_rank,'(a,i15)') 'IEND_SUM'
      write(50+id_rank,'(10i16)') djo_tbl%IEND_SUM(1:djo_tbl%NUM_NCOMP)
      write(50+id_rank,'(a,i15)') 'IEND_SUM_smp'
      write(50+id_rank,'(10i16)')                                       &
     &                djo_tbl%IEND_SUM_smp(1:np_smp*djo_tbl%NUM_NCOMP)
!
      write(50+id_rank,'(a,i15)') 'IAM', djo_tbl%NCM
      write(50+id_rank,'(10i16)') djo_tbl%IAM(1:djo_tbl%NCM)
!
      end subroutine check_type_DJO_table_info
!
!  ---------------------------------------------------------------------
!
      end module t_solver_ordered_crs
