!write_djds_matrix_struct.f90
!      module write_djds_matrix_struct
!
!     Written by H. Matsui on Jan., 2010
!
!      subroutine write_djds_mat11_comp_type(id_file, np_smp,           &
!     &          djds_tbl, mat11)
!        integer(kind=kint ), intent(in) :: id_file
!        integer(kind = kint), intent(in)  :: np_smp
!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!        type(DJDS_MATRIX), intent(in) :: mat11
!      subroutine write_djds_mat33_comp_type(id_file, np_smp,           &
!     &          djds_tbl, mat33)
!        integer(kind=kint ), intent(in) :: id_file
!        integer(kind = kint), intent(in)  :: np_smp
!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!        type(DJDS_MATRIX), intent(in) :: mat33
!      subroutine write_djds_mat_connect_type(id_file, np_smp,          &
!     &          comm_tbl, djds_tbl, mat)
!        integer(kind=kint ), intent(in) :: id_file
!        integer(kind = kint), intent(in)  :: np_smp
!        type(communication_table), intent(in) :: comm_tbl
!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!        type(DJDS_MATRIX), intent(in) :: mat
!
      module write_djds_matrix_struct
!
      use m_precision
!
      use m_machine_parameter
      use t_comm_table
      use t_solver_djds
      use write_djds_matrix_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat11_comp_type(id_file, np_smp,            &
     &          djds_tbl, mat11)
!
      integer(kind=kint ), intent(in) :: id_file
      integer(kind = kint), intent(in)  :: np_smp
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(in) :: mat11
!
!
      call write_djds_mat11_comp(id_file,                               &
     &     mat11%internal_diag, mat11%num_diag,                         &
     &     djds_tbl%NLmax, djds_tbl%NUmax,                              &
     &     djds_tbl%itotal_l, djds_tbl%itotal_u,                        &
     &     djds_tbl%npLX1, djds_tbl%npUX1, djds_tbl%NHYP,               &
     &     np_smp, djds_tbl%NEWtoOLD, mat11%D,                          &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     mat11%AL, mat11%AU,  mat11%ALUG_L, mat11%ALUG_U)
!
      end subroutine write_djds_mat11_comp_type
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat33_comp_type(id_file, np_smp,            &
     &          djds_tbl, mat33)
!
      integer(kind=kint ), intent(in) :: id_file
      integer(kind = kint), intent(in)  :: np_smp
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(in) :: mat33
!
!
      call write_djds_mat33_comp(id_file,                               &
     &     mat33%internal_diag, mat33%num_diag,                         &
     &     djds_tbl%NLmax, djds_tbl%NUmax,                              &
     &     djds_tbl%itotal_l, djds_tbl%itotal_u,                        &
     &     djds_tbl%npLX1, djds_tbl%npUX1, djds_tbl%NHYP,               &
     &     np_smp, djds_tbl%NEWtoOLD, mat33%D,                          &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     mat33%AL, mat33%AU,  mat33%ALUG_L, mat33%ALUG_U)
!
      end subroutine write_djds_mat33_comp_type
!
! ----------------------------------------------------------------------
!
      subroutine write_djds_mat_connect_type(id_file, np_smp,           &
     &          comm_tbl, djds_tbl, mat)
!
      integer(kind=kint ), intent(in) :: id_file
      integer(kind = kint), intent(in)  :: np_smp
      type(communication_table), intent(in) :: comm_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(in) :: mat
!
!
      call write_djds_mat_connects(id_file,                             &
     &     mat%num_diag, djds_tbl%NHYP, np_smp,                         &
     &     djds_tbl%STACKmcG, djds_tbl%STACKmc,                         &
     &     djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP, djds_tbl%IVECT,        &
     &     djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,          &
     &     djds_tbl%NEWtoOLD_DJDS_U, djds_tbl%LtoU,                     &
     &     comm_tbl%num_neib, comm_tbl%istack_export,                   &
     &     djds_tbl%NOD_EXPORT_NEW)
!
      end subroutine write_djds_mat_connect_type
!
! ----------------------------------------------------------------------
!
      end module write_djds_matrix_struct
