!
!      module solve_by_mass_z
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!     solve using CRS matrix
!
!      subroutine solve_crs_by_mass_z(nod_comm, node)
!      subroutine solve_crs_by_mass_z2(nod_comm, node)
!
      module solve_by_mass_z
!
      use m_precision
      use m_int_edge_vart_width
      use m_consist_mass_crs
!
      use t_geometry_data
      use t_comm_table
      use t_solver_djds
!
      implicit none
!
      type(DJDS_ordering_table), save :: djds_tbl1
      type(DJDS_MATRIX), save :: djds_mat1
!
      real(kind = kreal) :: rtime, starttime, endtime
!
      private :: djds_tbl1, djds_mat1
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine solve_crs_by_mass_z(nod_comm, node)
!
      use calypso_mpi
      use m_crs_matrix
      use solve_precond_DJDS
      use copy_matrix_2_djds_array
!
      type(node_data), intent(inout) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint) :: i, ierr
!
!
      mat1_crs%NB_crs = 1
         write(*,*) 'alloc_crs_mat_data'
      call alloc_crs_mat_data(tbl1_crs, mat1_crs)
!
      do i = 1, node%numnod
        mat1_crs%D_crs(i) = d_mk_crs(i)
        mat1_crs%B_crs(i) = rhs_mk_crs(i)
      end do
      do i = 1, tbl1_crs%ntot_l
        mat1_crs%AL_crs(i) = al_mk_crs(i)
      end do
      do i = 1, tbl1_crs%ntot_u
        mat1_crs%AU_crs(i) = au_mk_crs(i)
      end do
!
      write(*,*) 'solve_by_djds_solver11'
      call transfer_crs_2_djds_matrix(node, nod_comm,                   &
     &    tbl1_crs, mat1_crs, djds_tbl1, djds_mat1)
      call solve_by_djds_solver11                                       &
     &   (node, nod_comm, mat1_crs, djds_tbl1, djds_mat1, ierr)
!
      do i = 1, node%numnod
        sol_mk_crs(i) = mat1_crs%X_crs(i)
      end do
!
         write(*,*) 'dealloc_crs_mat_data'
      call dealloc_crs_mat_data(mat1_crs)
!
      end subroutine solve_crs_by_mass_z
!
!  ---------------------------------------------------------------------
!
      subroutine solve_crs_by_mass_z2(nod_comm, node)
!
      use calypso_mpi
      use m_machine_parameter
      use m_iccg_parameter
      use m_crs_matrix
!
      use copy_matrix_2_djds_array
      use solver_DJDS11_struct
      use solve_precond_DJDS
!
      type(node_data), intent(inout) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint) :: i, ierr
!
!
      mat1_crs%NB_crs = 1
         write(*,*) 'alloc_crs_mat_data'
      call alloc_crs_mat_data(tbl1_crs, mat1_crs)
!
      do i = 1, node%numnod
        mat1_crs%B_crs(i) = rhs_mk_crs(i)
      end do
!
      djds_mat1%NB = mat1_crs%NB_crs
      call transfer_crs_2_djds_matrix(node, nod_comm,                   &
     &    tbl1_crs, mat1_crs, djds_tbl1, djds_mat1)
!
      call solve_by_djds_solverNN                                       &
     &   (node, nod_comm, mat1_crs, djds_tbl1, djds_mat1, ierr)
!
      do i = 1, node%numnod
        sol_mk_crs(i) = mat1_crs%X_crs(i)
      end do
!
      write(*,*) 'dealloc_crs_mat_data'
      call dealloc_crs_mat_data(mat1_crs)
!
      end subroutine solve_crs_by_mass_z2
!
!  ---------------------------------------------------------------------
!
      end module solve_by_mass_z
