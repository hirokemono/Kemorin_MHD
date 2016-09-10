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
!!      subroutine link_alloc_type_djo11_mat(comm_table, djo_tbl,        &
!!     &          solver)
!!      subroutine link_alloc_type_djo33_mat(comm_table, djo_tbl,        &
!!     &          solver)
!!      subroutine link_alloc_type_djoNN_mat(NB, comm_table, djo_tbl,    &
!!     &          solver)
!!      subroutine link_alloc_type_zero_mat(comm_table, djds_table,      &
!!     &          solver)
!!
!!      subroutine alloc_type_djo_num(np_smp, djo_tbl)
!!      subroutine alloc_type_djo_table(djo_tbl)
!!      subroutine alooc_djo_zero_connect_type(np_smp, djo_tbl)
!!
!!      subroutine alloc_type_djo11_mat(djo_tbl, mat11)
!!      subroutine alloc_type_djo33_mat(djo_tbl, mat33)
!!      subroutine alloc_type_djoNN_mat(NB, djo_tbl, matNN)
!!      subroutine alloc_type_djo_zero_mat(djo_tbl, mat)
!!
!!      subroutine dealloc_type_connect_4_djo(djo_tbl)
!!      subroutine dealloc_type_djo_mat(mat)
!!
!!      subroutine link_djo_connect_structs(djo_org, djo_tbl)
!
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
        integer(kind=kint ), pointer :: INOD_DJO(:)
        integer(kind=kint ), pointer :: INM(:)
        integer(kind=kint ), pointer :: IAM(:)
!
        integer(kind=kint ) ::  NUM_NCOMP
        integer(kind=kint ), pointer :: NUM_SUM(:)
        integer(kind=kint ), pointer :: IEND_SUM(:)
        integer(kind=kint ), pointer :: IEND_SUM_smp(:)
!
        real   (kind=kreal), pointer ::  AM(:)
      end type CRS_SMP_CONNECT_MATRIX
!
      type DJORS_CONNECT
        integer(kind=kint ) ::  NC
        integer(kind=kint ) ::  NCM
        integer(kind=kint ), pointer :: INOD_DJO(:)
        integer(kind=kint ), pointer :: INM(:)
        integer(kind=kint ), pointer :: IAM(:)
!
        integer(kind=kint ) ::  NUM_NCOMP
        integer(kind=kint ), pointer :: NUM_SUM(:)
        integer(kind=kint ), pointer :: IEND_SUM(:)
        integer(kind=kint ), pointer :: IEND_SUM_smp(:)
      end type DJORS_CONNECT
!
      type DJORS_MATRIX
        integer(kind=kint ) ::  num_non0
        real   (kind=kreal), pointer ::  aiccg(:)
        real   (kind=kreal), pointer ::  AM(:)
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
!-----------------------------------------------------------------------
!
      subroutine link_alloc_type_djo11_mat(comm_table, djo_tbl,         &
     &          solver)
!
      type(communication_table), intent(in) :: comm_table
      type(DJORS_CONNECT), intent(in) :: djo_tbl
      type(DJORS_SOLVER_ARRAYS), intent(inout) :: solver
!
!
      call link_comm_tbl_types(comm_table, solver%djo_comm)
      call link_djo_connect_structs(djo_tbl, solver%djo_tbl)
      call alloc_type_djo11_mat(solver%djo_tbl, solver%djo_mat)
!
      end subroutine link_alloc_type_djo11_mat
!
!  ---------------------------------------------------------------------
!
      subroutine link_alloc_type_djo33_mat(comm_table, djo_tbl,         &
     &          solver)
!
      type(communication_table), intent(in) :: comm_table
      type(DJORS_CONNECT), intent(in) :: djo_tbl
      type(DJORS_SOLVER_ARRAYS), intent(inout) :: solver
!
!
      call link_comm_tbl_types(comm_table, solver%djo_comm)
      call link_djo_connect_structs(djo_tbl, solver%djo_tbl)
      call alloc_type_djo33_mat(solver%djo_tbl, solver%djo_mat)
!
      end subroutine link_alloc_type_djo33_mat
!
!  ---------------------------------------------------------------------
!
      subroutine link_alloc_type_djoNN_mat(NB, comm_table, djo_tbl,     &
     &          solver)
!
      integer(kind = kint), intent(in) :: NB
      type(communication_table), intent(in) :: comm_table
      type(DJORS_CONNECT), intent(in) :: djo_tbl
      type(DJORS_SOLVER_ARRAYS), intent(inout) :: solver
!
!
      call link_comm_tbl_types(comm_table, solver%djo_comm)
      call link_djo_connect_structs(djo_tbl, solver%djo_tbl)
      call alloc_type_djoNN_mat(NB, solver%djo_tbl, solver%djo_mat)
!
      end subroutine link_alloc_type_djoNN_mat
!
!  ---------------------------------------------------------------------
!
      subroutine link_alloc_type_zero_mat(comm_table, djo_table,        &
     &          solver)
!
      type(communication_table), intent(in) :: comm_table
      type(DJORS_CONNECT), intent(in) :: djo_table
      type(DJORS_SOLVER_ARRAYS), intent(inout) :: solver
!
!
      call link_comm_tbl_types(comm_table, solver%djo_comm)
      call link_djo_connect_structs(djo_table, solver%djo_tbl)
      call alloc_type_djo_zero_mat(solver%djo_tbl, solver%djo_mat)
!
      end subroutine link_alloc_type_zero_mat
!
!  ---------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
      subroutine alloc_type_djo11_mat(djo_tbl, mat11)
!
       type(DJORS_CONNECT), intent(in) :: djo_tbl
       type(DJORS_MATRIX), intent(inout) :: mat11
!
!
       mat11%num_non0 = djo_tbl%NCM
!
       allocate(mat11%aiccg(0:mat11%num_non0) )
       if(mat11%num_non0 .gt. 0) mat11%aiccg = 0.0d0
!
       mat11%AM =>  mat11%aiccg(1:mat11%num_non0)
!
       end subroutine alloc_type_djo11_mat
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_type_djo33_mat(djo_tbl, mat33)
!
       type(DJORS_CONNECT), intent(in) :: djo_tbl
       type(DJORS_MATRIX), intent(inout) :: mat33
!
!
       mat33%num_non0 = 9 * djo_tbl%NCM
!
       allocate(mat33%aiccg(-8:mat33%num_non0) )
       if(mat33%num_non0 .gt. 0) mat33%aiccg = 0.0d0
!
       mat33%AM =>  mat33%aiccg(1:mat33%num_non0)
!
       end subroutine alloc_type_djo33_mat
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_type_djoNN_mat(NB, djo_tbl, matNN)
!
       integer(kind = kint), intent(in) :: NB
       type(DJORS_CONNECT), intent(in) :: djo_tbl
       type(DJORS_MATRIX), intent(inout) :: matNN
       integer(kind = kint) :: NB2
!
!
       NB2= NB*NB
       matNN%num_non0 = NB2 * djo_tbl%NCM
!
       allocate(matNN%aiccg(-NB2+1:matNN%num_non0) )
       if(matNN%num_non0 .gt. 0) matNN%aiccg = 0.0d0
!
       matNN%AM =>  matNN%aiccg(1:matNN%num_non0)
!
       end subroutine alloc_type_djoNN_mat
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_type_djo_zero_mat(djo_tbl, mat)
!
       type(DJORS_CONNECT), intent(inout) :: djo_tbl
      type(DJORS_MATRIX), intent(inout) :: mat
!
!
      djo_tbl%NC =  izero
      djo_tbl%NCM = izero
!
      djo_tbl%NUM_NCOMP = izero
!
      allocate(mat%aiccg(izero:izero) )
      mat%AM(izero) = 0.0d0
!
      mat%AM =>  mat%aiccg(0:0)
!
      end subroutine alloc_type_djo_zero_mat
!
!  ---------------------------------------------------------------------
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
!  ---------------------------------------------------------------------
!
      subroutine dealloc_type_djo_mat(mat)
!
      type(DJORS_MATRIX), intent(inout) :: mat
!
!
      nullify(mat%AM)
      deallocate(mat%aiccg)
!
      end subroutine dealloc_type_djo_mat
!
!  ---------------------------------------------------------------------
!
      subroutine link_djo_connect_structs(djo_org, djo_tbl)
!
      type(DJORS_CONNECT), intent(in) ::    djo_org
      type(DJORS_CONNECT), intent(inout) :: djo_tbl
!
!
      djo_tbl%NC =       djo_org%NC
      djo_tbl%NCM =      djo_org%NCM
      djo_tbl%NUM_NCOMP = djo_org%NUM_NCOMP
!
      djo_tbl%INOD_DJO => djo_org%INOD_DJO
      djo_tbl%INM =>      djo_org%INM
      djo_tbl%IAM =>      djo_org%IAM
      djo_tbl%NUM_SUM =>  djo_org%NUM_SUM
      djo_tbl%IEND_SUM => djo_org%IEND_SUM
      djo_tbl%IEND_SUM_smp => djo_org%IEND_SUM_smp
!
      end subroutine link_djo_connect_structs
!
!  ---------------------------------------------------------------------
!
      subroutine link_djo_matrix_structs(mat_org, mat)
!
      type(DJORS_MATRIX), intent(inout) :: mat
      type(DJORS_MATRIX), intent(inout) :: mat_org
!
      mat%AM => mat_org%AM
!
      end subroutine link_djo_matrix_structs
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_djo_connect_structs(djo_tbl)
!
      type(DJORS_CONNECT), intent(inout) :: djo_tbl
!
!
      djo_tbl%NC =       0
      djo_tbl%NCM =      0
      djo_tbl%NUM_NCOMP = 0
!
      nullify( djo_tbl%INOD_DJO )
      nullify( djo_tbl%INM, djo_tbl%IAM )
      nullify( djo_tbl%IEND_SUM )
!
      end subroutine unlink_djo_connect_structs
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine check_type_DJO_table_info(my_rank, np_smp, djo_tbl)
!
       integer (kind = kint), intent(in) :: my_rank, np_smp
       type(DJORS_CONNECT), intent(in) :: djo_tbl
!
       integer(kind = kint) :: i
!
      write(50+my_rank,'(a,i15)')                                       &
     &                      'inod, INOD_DJO, INOD_DJO', djo_tbl%NC
      do i = 1, djo_tbl%NC
        write(50+my_rank,'(10i16)')                                     &
     &                      i, djo_tbl%INOD_DJO(i), djo_tbl%INM(i)
      end do
!
      write(50+my_rank,'(a,i15)') 'NUM_NCOMP', djo_tbl%NUM_NCOMP
      write(50+my_rank,'(a,i15)') 'IEND_SUM'
      write(50+my_rank,'(10i16)') djo_tbl%IEND_SUM(1:djo_tbl%NUM_NCOMP)
      write(50+my_rank,'(a,i15)') 'IEND_SUM_smp'
      write(50+my_rank,'(10i16)')                                       &
     &                djo_tbl%IEND_SUM_smp(1:np_smp*djo_tbl%NUM_NCOMP)
!
      write(50+my_rank,'(a,i15)') 'IAM', djo_tbl%NCM
      write(50+my_rank,'(10i16)') djo_tbl%IAM(1:djo_tbl%NCM)
!
      end subroutine check_type_DJO_table_info
!
!  ---------------------------------------------------------------------
!
      end module t_solver_ordered_crs
